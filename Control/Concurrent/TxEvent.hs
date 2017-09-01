-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.TxEvent
-- Copyright   :  (c) 2006,2007 Kevin Donnelly & Matthew Fluet; 2011 Edward Amsden & Matthew Fluet
-- License     :  BSD3
-- Maintainer  :  Matthew Fluet <Matthew.Fluet@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (requires Control.Concurrent and Control.Concurrent.STM)
--
-- Transactional Events.
-- 
-- This library provides /first-class synchronous events/ in the style
-- of CML (<http://cml.cs.uchicago.edu/>), but generalizes the concept
-- to allow /multiple/, /dependent/ events to be combined in a single
-- event.  The semantics of these generalized events ensures an
-- /all-or-nothing/ transactional property -- either all of the
-- constituent events synchronize or none of them synchronize.  When
-- the constituent events include synchronous message passing, the
-- semantics ensures that no thread is able to complete its
-- synchronization until all of its (transitive) communications
-- partners are willing to commit to a compatible synchronization.
--
-----------------------------------------------------------------------------

module Control.Concurrent.TxEvent (
  -- * Event type
  Evt,                  -- abstract, instance Functor, Monad, MonadPlus

  -- * Synchronization
  sync,                 -- :: Evt a -> IO a

  -- * Monadic event combinators
  alwaysEvt,            -- :: a -> Evt a
  thenEvt,              -- :: Evt a -> (a -> Evt b) -> Evt b
  neverEvt,             -- :: Evt a
  chooseEvt,            -- :: Evt a -> Evt a -> Evt a,

  -- * Exceptions
  throwEvt,             -- :: Exception e => e -> Evt a
  catchEvt,             -- :: Exception e => Evt a -> (e -> Evt a) -> Evt a

  -- * Synchronous channels
  SChan,                -- abstract, instance Eq
  newSChan,             -- :: Evt (SChan a)
  sendEvt,              -- :: SChan a -> a -> Evt ()
  recvEvt,              -- :: SChan a -> Evt a

  -- * Time delays
  timeOutEvt,           -- :: Int -> Evt
  utcTimeEvt,           -- :: Data.Time.Clock.UTCTime -> Evt ()
  nominalDiffTimeEvt,   -- :: Data.Time.Clock.NominalDiffTime -> Evt ()

  -- *  Miscellaneous
  myThreadIdEvt,        -- :: Evt ThreadId
  forkEvt,              -- :: Evt a -> (a -> IO ()) -> Evt ThreadId
  ) where

import Prelude
import Control.Applicative
import Control.Monad
import qualified Control.Exception as Exception
import Control.Exception (Exception, SomeException)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Data.IORef

import qualified Data.Foldable as Foldable
import Data.Foldable (Foldable)
import qualified Data.Traversable as Traversable
import Data.Traversable (Traversable)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Time.Clock as Clock
import Data.Time.Clock (UTCTime,NominalDiffTime)

import qualified Debug.Trace as Trace

(==>>) :: Bool -> Bool -> Bool
(==>>) p q = (not p) || q

andForFoldM :: (Monad m) => (a -> m Bool) -> Bool -> a -> m Bool
andForFoldM f True z = f z
andForFoldM _ False _ = return False

----------------------------------------------------------------------
----------------------------------------------------------------------
----------------------------------------------------------------------

----------------------------------------------------------------------
-- Flags
----------------------------------------------------------------------

debugEvt = False
debugMsg = when debugEvt . Trace.putTraceMsg

-- Flag to control whether invariant checks are performed.
doChecks = False

-- Flag to control whether to yield after spawning search threads for
-- matching send/recv pairs; gives the search threads a chance to run
-- before more search threads for matching send/recv pairs are
-- spawned.
doYields = True

-- Flag to control whether counts are maintained.
doCounts = False
-- Flag to control whether a limit is placed on the number of
-- simultaneously running search threads.
doLimit = False && doCounts
countLimit = 100

----------------------------------------------------------------------
-- Global boolean references
----------------------------------------------------------------------

-- A unique boolean reference is associated with each thread
-- synchronization; the reference is True when the thread has not yet
-- synchronized.
-- In practice, we split the reference into a 'clean' STM TVar and a
-- 'dirty' IORef.  The 'clean' reference is used for synchronization
-- commitment and serves as the master state of synchronization.  The
-- 'dirty' reference is used for polling, since reading an IORef is
-- much cheaper than reading an STM TVar.
type BoolRef = (TVar Bool, IORef Bool)
newBoolRef = do bSTM <- newTVarIO True
                bIO <- newIORef True
                return (bSTM, bIO)
writeBoolRefClean (bSTM, bIO) = writeTVar bSTM False
readBoolRefClean (bSTM, bIO) = readTVar bSTM
writeBoolRefDirty (bSTM, bIO) = writeIORef bIO False
readBoolRefDirty (bSTM, bIO) = readIORef bIO

----------------------------------------------------------------------
-- Global result references
----------------------------------------------------------------------

-- A unique result reference is associated with each thread
-- synchronization; the reference is Nothing when the thread has not
-- yet synchronized, and Just e when the thread has synchronized with
-- synchronization result e.
type ResultRef a = TVar (Maybe a)
newResultRef = newTVarIO Nothing
writeResultRef r rv = writeTVar r (Just rv)
readResult r = do
  mrv <- readTVar r
  case mrv of
    Nothing -> retry
    Just rv -> return rv

----------------------------------------------------------------------
-- Global integer references
----------------------------------------------------------------------

-- A unique integer reference is associated with each thread
-- synchronization.  It maintains the number of running search threads
-- and the maximum number of simultaneously running search threads.
type CountRef = TVar (Int, Int)
newCountRef = newTVarIO (0, -1)

-- Increment a count ref.  Called when a search thread is spawned.
incCountRef :: ThreadId -> CountRef -> IO ()
incCountRef tid cnt = when doCounts $ do
  ((c,max),(c',max')) <- atomically $ do
    (c, max) <- readTVar cnt
    let c' = c + 1
    when doLimit $ if c' > countLimit then retry else return ()
    let max' = if c' > max then c' else max
    writeTVar cnt (c', max')
    return ((c,max),(c',max'))
  when (max' > max) $ debugMsg $ (show tid) ++ ": cnt -> " ++ (show c')
-- Decrement a count ref.  Called when a search thread is terminated.
decCountRef :: ThreadId -> CountRef -> IO ()
decCountRef tid cnt = when doCounts $ do
  ((c,max),(c',max')) <- atomically $ do
    (c, max) <- readTVar cnt
    let c' = c - 1
    let max' = max
    writeTVar cnt (c', max')
    return ((c,max),(c',max'))
  when False $ debugMsg $ (show tid) ++ ": cnt -> " ++ (show c')

----------------------------------------------------------------------
-- Event synchronization identifiers
----------------------------------------------------------------------

-- A 'sync id' uniquely identifies a synchronization action.
-- It records the thread identifier of the synchronizing thread, the
-- unique boolean reference associated with that thread
-- synchronization, the time at which the synchronization action was
-- started, and a counter for the number of active search threads.
-- These values are shared by all search threads spawned for a thread
-- synchronization.
newtype SyncId = SyncId (ThreadId, BoolRef, UTCTime, CountRef)

instance Eq SyncId
    where
      (==) (SyncId (tid1, b1, _, _)) (SyncId (tid2, b2, _, _)) =
          tid1 == tid2 && b1 == b2

instance Show SyncId
    where
      show (SyncId (tid, _, start, _)) =
          "(" ++ (show tid) ++ "@" ++ (show (Clock.diffUTCTime start zeroUTCTime)) ++ ")"

----------------------------------------------------------------------
-- Event synchronization paths, trails, tracks, and completion references
----------------------------------------------------------------------

-- A 'path' records the non-deterministic actions made during the
-- evaluation of a transactional event; in particular, it records the
-- alternative taken at chooseEvt and the communication partner at
-- sendEvt and recvEvt. 
-- A 'trail' is an active path recording the sync id of the
-- synchronization and the current event synchronization path.
-- A 'track' is a completed path recording the sync id of the
-- synchronization, the complete event synchronization path, and the
-- IO action that will return the synchronization result.
-- A 'completion reference' maintains a list of tracks (completed
-- searches).
data ChooseElement = ChooseLeft | ChooseRight deriving (Eq)
data CommElement = CommSend | CommRecv deriving (Eq)
data PathElement = Choose ChooseElement
                 | Comm CommElement Trail CompletionRef CompletionRef
type Path = [PathElement]
newtype Trail = Trail (SyncId, Path)
newtype Track = Track (SyncId, Path, IO ())
type CompletionRef = TVar [Track]

-- The path element (Choose ChooseLeft) indicates that the first
-- alternative in a chooseEvt was taken, while the path element
-- (Choose ChooseRight) indicates that the second alternative was
-- taken.

-- The path element (Comm CommSend (Trail (sidr, pr)) cr cs)
-- indicates that a synchronous message was sent to a search thread
-- with synchronization id sidr and path pr.
-- The path element (Comm CommRecv (Trail (sids, ps)) cs cr)
-- indicates that a synchronous message was received from a search
-- thread with synchronization identifier sids and path ps. 
-- Note that neither the channel nor the message is included in the
-- path element.

-- In the trail (Trail (sids, ((Comm CommSend (Trail (sidr, pr)) cr cs):ps))), 
-- the completion reference cr maintains lists of tracks (completed
-- searches) that extend the path pr of the receiver and the
-- completion reference cs maintains sets of tracks (completed
-- searches) that extend the path ps of the sender.  
-- Note that the matching trail of the receiver, 
-- (Trail (sidr, ((Comm CommRecv (Trail (sids, ps)) cs cr):pr))),
-- includes the same completion references, but in the reversed order.

instance Show ChooseElement
    where
      show ChooseLeft = "Left"
      show ChooseRight = "Right"
instance Show CommElement
    where
      show CommSend = "Send"
      show CommRecv = "Recv"

eqPathElementSlow (Choose ce1) (Choose ce2) = ce1 == ce2
eqPathElementSlow (Comm ce1 tr1 c1 c1') (Comm ce2 tr2 c2 c2') =
    ce1 == ce2 && tr1 == tr2 && c1 == c2 && c1' == c2'
eqPathElementSlow _ _ = False
-- The completion references associated with a communication path
-- element are unique; hence, equality of the completion references
-- implies equality of the trail.
eqPathElementFast (Choose ce1) (Choose ce2) = ce1 == ce2
eqPathElementFast (Comm ce1 tr1 c1 c1') (Comm ce2 tr2 c2 c2') =
    ce1 == ce2 && c1 == c2 && c1' == c2'
eqPathElementFast _ _ = False
instance Eq PathElement
    where
      (==) = eqPathElementFast
instance Show PathElement
    where
      show (Choose ce) = show ce
      show (Comm ce tr _ _) = (show ce) ++ "(" ++ (show tr) ++ ")"

chooseLeft = Choose ChooseLeft
chooseRight = Choose ChooseRight
commSend tr cr cs = Comm CommSend tr cr cs
commRecv tr cs cr = Comm CommRecv tr cs cr

instance Eq Trail
    where
      (==) (Trail (sid1, p1)) (Trail (sid2, p2)) =
          sid1 == sid2 && p1 == p2
instance Show Trail
    where
      show (Trail (sid, p)) = 
          "(" ++ (show sid) ++ "#" ++ (show p) ++ ")"

instance Eq Track
    where
      (==) (Track (sid1, p1, _)) (Track (sid2, p2, _)) =
          sid1 == sid2 && p1 == p2
instance Show Track
    where
      show (Track (sid, p, _)) = 
          "(" ++ (show sid) ++ "#" ++ (show p) ++ ")"

newCompletionRef = newTVarIO []
readCompletionRef c = readTVar c
writeTrackToCompletionRef tr c = do
  trs <- readTVar c
  writeTVar c (tr:trs)

----------------------------------------------------------------------
-- Extends
----------------------------------------------------------------------

-- Def: The path pa /extends/ the path pb if pa is an extension of pb.
($>) :: Path -> Path -> Bool
pa $> pb = List.isSuffixOf pb pa

-- Def: The paths pa and pb are /comparable/ if pa extends pb or pb
-- extends pa.
(<$$>) :: Path -> Path -> Bool
pa <$$> pb = (pa $> pb) || (pb $> pa)

----------------------------------------------------------------------
-- Dependencies
----------------------------------------------------------------------

-- Def: The /dependencies/ of a trail tr is the set of trails implied
-- by the trail.
dep :: Trail -> [Trail]
dep tr = tr : (depAux tr)

depAux :: Trail -> [Trail]
depAux (Trail (sid, [])) = []
depAux (Trail (sid, ((Choose _):p))) = dep (Trail (sid, p))
depAux (Trail (sid, (Comm ce (Trail (sid', p')) c' c):p)) =
    let ce' = case ce of CommSend -> CommRecv ; CommRecv -> CommSend in
    let tr' = Trail (sid', ((Comm ce' (Trail (sid, p)) c c'):p')) in
    tr' : (dep (Trail (sid, p))) ++ (dep (Trail (sid', p')))

----------------------------------------------------------------------
-- Consistent
----------------------------------------------------------------------

-- Def: The trail tr is /consistent/ if no thread identifier in the
-- dependencies of tr is grouped with unequal boolean references or
-- with incomparable paths.
consistent :: Trail -> Bool
consistent tr =
    Foldable.all (\ (Trail (SyncId (tid1, b1, _, _), p1)) ->
    Foldable.all (\ (Trail (SyncId (tid2, b2, _, _), p2)) ->
                      (tid1 == tid2) ==>> ((b1 == b2) && (p1 <$$> p2)))
                 (dep tr))
                 (dep tr)

----------------------------------------------------------------------
-- Dependency trail map
----------------------------------------------------------------------

-- A 'dependency trail map' is an efficient representation of the
-- dependencies of a consistent trail; it records the maximal path of
-- each thread in the dependencies of a trail.
type DepTrailMap = Map ThreadId Trail

-- Given a trail, check that the given dependency trail map of the
-- trail is an accurate representation of the dependencies.
checkDepTrailMap :: Trail -> DepTrailMap -> Bool
checkDepTrailMap tr dm = 
    -- Every dependency has an extension in the dependency trail map
    Foldable.all (\ (Trail (sid@(SyncId (tid, _, _, _)), p)) ->
                      case Map.lookup tid dm of
                        Nothing -> False
                        Just (Trail (sid', p')) ->
                            (sid' == sid) && (p' $> p))
                 (dep tr) &&
    -- Every element of the dependency trail map is a dependency
    Foldable.all (\ (Trail (sid, p)) ->
                      Foldable.any (\ (Trail (sid', p')) ->
                                        (sid == sid') && (p == p'))
                                   (dep tr))
                 dm

-- Build the dependency trail map for a trail.
mkDepTrailMap :: Trail -> DepTrailMap
mkDepTrailMap tr = mkDepTrailMapAux tr Map.empty

mkDepTrailMapAux :: Trail -> DepTrailMap -> DepTrailMap
mkDepTrailMapAux tr@(Trail (sid@(SyncId(tid, _, _, _)), p)) dm =
    case Map.lookup tid dm of
      Nothing -> mkDepTrailMapAuxAux tr (Map.insert tid tr dm)
      Just (Trail (sid', p')) ->
          if sid' /= sid
             then error "mkDepTrailMapAux -- inconsistent trail"
          else if p' $> p
             then dm
          else if p $> p'
             then mkDepTrailMapAuxAux tr (Map.insert tid tr dm)
          else error "mkDepTrailMapAux -- inconsistent trail"

mkDepTrailMapAuxAux :: Trail -> DepTrailMap -> DepTrailMap
mkDepTrailMapAuxAux (Trail (sid@(SyncId (tid, bf, _, _)), p)) dm =
    case p of
      [] -> dm
      (Choose _):p -> mkDepTrailMapAuxAux (Trail (sid, p)) dm
      (Comm ce (Trail (sid'@(SyncId (tid', b', _, _)), p')) c' c):p -> 
          let dm' = mkDepTrailMapAuxAux (Trail (sid, p)) dm in 
          let ce' = case ce of CommSend -> CommRecv ; CommRecv -> CommSend in
          let tr' = Trail (sid', ((Comm ce' (Trail (sid, p)) c c'):p')) in
          mkDepTrailMapAux tr' dm'

----------------------------------------------------------------------
-- Coherent
----------------------------------------------------------------------

-- Def: The trails trs and trr are coherent if the trails are an
-- acceptable sender/receiver pair.
coherent :: Trail -> Trail -> Bool
coherent trs@(Trail (SyncId (tids, bs, _, _), ps))
         trr@(Trail (SyncId (tidr, br, _, _), pr)) =
    -- No self communication
    tids /= tidr &&
    -- If the receiver communicated (directly or indirectly) with the
    -- sender in the past, then it must have been in the history of
    -- this sender with path ps.
    Foldable.all (\ (Trail (SyncId (tid, b, _, _), p)) ->
                      (tids == tid) ==>> ((bs == b) && (ps $> p)))
                 (dep trr) &&
    -- If the sender communicated (directly or indirectly) with the
    -- receiver in the past, then it must have been in the history of
    -- this receiver with path pr.
    Foldable.all (\ (Trail (SyncId (tid, b, _, _), p)) ->
                      (tidr == tid) ==>> ((br == b) && (pr $> p)))
                 (dep trs) &&
    -- If there is a common dependend upon search thread, then the
    -- path in one dependency must be an extension of the path in the
    -- other dependency.
    Foldable.all (\ (Trail (SyncId (tid1, b1, _, _), p1)) ->
    Foldable.all (\ (Trail (SyncId (tid2, b2, _, _), p2)) ->
                      (tid1 == tid2) ==>> ((b1 == b2) && (p1 <$$> p2)))
                 (dep trr))
                 (dep trs)

-- Build (directly) the combined dependency trail map of coherent
-- trails (with dependency trail maps).
mkCoherentDepTrailMap :: (Trail, DepTrailMap) -> 
                         (Trail, DepTrailMap) -> Maybe DepTrailMap
mkCoherentDepTrailMap (trs@(Trail (sids@(SyncId (tids, _, _, _)), ps)), dms)
                      (trr@(Trail (sidr@(SyncId (tidr, _, _, _)), pr)), dmr) =
    -- No self communication
    if not (sids /= sidr)
       then Nothing
    -- If the receiver communicated (directly or indirectly) with the
    -- sender in the past, then it must have been in the history of
    -- this sender with path ps.
    else if not (case Map.lookup tids dmr of
                   Nothing -> True
                   Just (Trail (sid, p)) -> (sids == sid) && (ps $> p))
       then Nothing
    -- If the sender communicated (directly or indirectly) with the
    -- receiver in the past, then it must have been in the history of
    -- this receiver with path pr.
    else if not (case Map.lookup tidr dms of
                   Nothing -> True
                   Just (Trail (sid, p)) -> (sidr == sid) && (pr $> p))
       then Nothing
    -- If there is a common dependend upon search thread, then the
    -- path in one dependency must be an extension of the path in the
    -- other dependency.
    else Map.fold (\ tr@(Trail (sid@(SyncId (tid, _, _, _)), p)) mdm ->
                       case mdm of
                         Nothing -> Nothing
                         Just dm ->
                             case Map.lookup tid dm of
                               Nothing -> Just (Map.insert tid tr dm)
                               Just (Trail (sid', p')) ->
                                   -- Same thread id with different sync ids;
                                   -- sender or receiver is inactive.
                                   if sid' /= sid
                                      then Nothing
                                   -- The trail in the map is maximal.
                                   else if p' $> p
                                      then Just dm
                                   -- The other trail is maximal.
                                   else if p $> p'
                                      then Just (Map.insert tid tr dm)
                                   -- The trails are incomparable.
                                   else Nothing)
                  (Just dms) 
                  dmr

-- Given trails (with dependency trail maps), check that the given
-- dependency trail map corresponds to the combined dependency trail
-- map.
checkCoherentDepTrailMap :: (Trail, DepTrailMap) -> 
                            (Trail, DepTrailMap) -> 
                            Maybe DepTrailMap -> Bool
checkCoherentDepTrailMap (trs, dms) (trr, dmr) mdm =
    if coherent trs trr
       then mdm == Just (Map.unionWith
                         (\ tr1@(Trail (sid1, p1)) tr2@(Trail (sid2, p2)) ->
                              if sid1 /= sid2
                                 then error "checkCoherentDepTrailMaps -- incoherent"
                              else if p1 $> p2
                                 then tr1
                              else if p2 $> p1
                                 then tr2
                              else error "checkCoherentDepTrailMaps -- incoherent")
                         dms dmr)
       else mdm == Nothing

----------------------------------------------------------------------
-- Committable
----------------------------------------------------------------------

committable :: [Track] -> STM Bool
committable _ = undefined

----------------------------------------------------------------------
-- Commit track map
----------------------------------------------------------------------

-- A 'commit track map' is an efficient representation of a
-- committable set of tracks.
type CommitTrackMap = Map ThreadId Track

-- Check that the given commit track map corresponds to a committable
-- set of tracks.
checkCommitTrackMap :: CommitTrackMap -> STM Bool
checkCommitTrackMap ctm = 
    if Foldable.all (\ (Track (sid, p, _)) ->
       Foldable.all (\ (Trail (sid'@(SyncId (tid', _, _, _)), p')) ->
                         case Map.lookup tid' ctm of
                           Nothing -> False
                           Just (Track (sid'', p'', _)) ->
                               (sid'' == sid') && (p'' $> p'))
                    (dep (Trail (sid, p))))
                    ctm
       then Foldable.foldlM (andForFoldM (\ (Track (SyncId (_, b, _, _), _, _)) -> readBoolRefClean b))
                            True ctm
       else return False

----------------------------------------------------------------------
-- CommitScan
----------------------------------------------------------------------

-- Scan for a committable set of tracks (completed searches).
commitScan :: Track -> STM [CommitTrackMap]
commitScan tr@(Track (sid@(SyncId (tid, b, _, _)), p, _)) = do
  bv <- readBoolRefClean b
  if bv
     then commitScanAux (Trail (sid, p)) (Map.singleton tid tr)
     else return []

commitScanAux :: Trail -> CommitTrackMap -> STM [CommitTrackMap]
commitScanAux tr@(Trail (sid@(SyncId (tid, _, _, _)), p)) ctm =
    case p of
      [] -> return [ctm]
      (Choose _):p -> commitScanAux (Trail (sid, p)) ctm
      (Comm ce tr'@(Trail (sid'@(SyncId (tid', b', _, _)), p')) c' c):p -> do
        bv <- readBoolRefClean b'
        if bv
           then case Map.lookup tid' ctm of
                  Just (Track (_, p'', _)) -> do
                    let ce' = case ce of CommSend -> CommRecv ; CommRecv -> CommSend 
                    if p'' $> ((Comm ce' (Trail (sid,p)) c c'):p')
                       then commitScanAux (Trail (sid, p)) ctm
                       else return []
                  Nothing -> do
                    trs <- readCompletionRef c'
                    ctm'ss <- mapM (\ tr''@(Track (sid'', p'', _)) -> 
                                        commitScanAux (Trail (sid'', p''))
                                                      (Map.insert tid' tr'' ctm)) trs
                    let ctm's = concat ctm'ss
                    ctm''ss <- mapM (\ ctm' -> commitScanAux (Trail (sid, p)) ctm') ctm's
                    let ctm''s = concat ctm''ss
                    return ctm''s
           else return []

----------------------------------------------------------------------
-- AddTrack
----------------------------------------------------------------------

-- Add a track to the completion references on its path.
addTrack :: Track -> STM ()
addTrack tr@(Track (sid, p, act)) =
    mapM_ (\ pe ->
               case pe of
                 Choose _ -> return ()
                 Comm _ _ _ c -> writeTrackToCompletionRef tr c)
          p

----------------------------------------------------------------------
-- Active
----------------------------------------------------------------------

-- The trail tr is /active/ if every boolean reference in the
-- dependencies of tr is True.
-- Generalize over boolean reference reader.
active :: Monad m => (BoolRef -> m Bool) -> Trail -> m Bool
active readBoolRef tr = activeAux readBoolRef (dep tr)
activeDepTrailMap :: Monad m => (BoolRef -> m Bool) -> DepTrailMap -> m Bool
activeDepTrailMap readBoolRef dm = activeAux readBoolRef dm

activeAux :: (Monad m, Foldable s) => (BoolRef -> m Bool) -> (s Trail) -> m Bool
activeAux readBoolRef s = 
    Foldable.foldlM (andForFoldM (\ (Trail (SyncId (_, b, _, _), _)) -> readBoolRef b)) 
                    True s

----------------------------------------------------------------------
-- Event synchronization state
----------------------------------------------------------------------

-- The (search thread local) state of an event synchronization
-- includes the sync id, the current event synchronization path, and
-- its dependency trail map.
newtype EvtState = EvtState (SyncId, Path, DepTrailMap)

instance Show EvtState
    where
      show (EvtState (sid, p, dm)) = 
          "(" ++ (show sid) ++
          "," ++ (show p) ++
          -- "," ++ (show dm) ++ 
          ")"

-- Given an event state, check that it satisfies invariants.
checkEvtState :: EvtState -> Bool
checkEvtState (EvtState (sid, p, dm)) =
    consistent (Trail (sid, p)) &&
    checkDepTrailMap (Trail (sid, p)) dm

----------------------------------------------------------------------
-- Synchronous event computation
----------------------------------------------------------------------

data EvtRes a = Always a | Throw SomeException
type EvtCont a = EvtRes a -> EvtState -> IO ()
newtype Evt a = Evt (EvtCont a -> EvtState -> IO ())
{- ^
A value of type @'Evt' a@ is an /event/ which, when synchronized upon,
performs some synchronous operations before returning a value of type
@a@.

When synchronized upon, the event may perform (tentative) synchronous
message passing, but will exhibit no observable effects (i.e., will
not return) until the event and all communication partners can commit.

'Evt' is a monad (with plus), so 'Evt' actions can be combined using
either the @do@-notation or the operations from the 'Monad' and
'MonadPlus' classes.
-}
unEvt (Evt f) = f

-- Force evaluation of an event, catching exceptions in the Evt monad.
forceEvt :: Evt a -> EvtCont a -> EvtState -> IO ()
forceEvt evt k s = do
  f <- Exception.catch (Exception.evaluate (unEvt evt))
                       (\ exn -> return (\ k s -> k (Throw exn) s))
  f k s

-- Fizzle a search thread if it is inactive.
fizzleEvt :: String -> EvtState -> IO () -> IO ()
fizzleEvt str s@(EvtState (sid@(SyncId(tid, b, start, cnt)), p, dm)) act = do
  when doChecks $ 
    unless (checkEvtState s) $ 
      fail ("fizzleEvt:" ++ str ++ " -- checkEvtState failed")
  -- One large transaction
  -- bv <- atomically $ activeDepTrailMap readBoolRefClean dm
  -- Many small transactions
  -- bv <- activeDepTrailMap (atomically . readBoolRefClean) dm
  -- Using dirty reads
  bv <- activeDepTrailMap readBoolRefDirty dm
  if bv
     then act
     else do decCountRef tid cnt
             return ()

----------------------------------------------------------------------
-- Monadic event combinators.
----------------------------------------------------------------------

{-|
The always commitable event computation; 
the 'return' of the `Evt` monad.
-}
alwaysEvt :: a -> Evt a
alwaysEvt x = 
    Evt (\ k s@(EvtState (sid@(SyncId(tid, b, start, cnt)), p, dm)) -> 
             let alwaysEvtAct = k (Always x) s in
             fizzleEvt "alwaysEvt" s alwaysEvtAct)

{-|
Sequential composition of event computations; 
the '>>=' of the `Evt` monad.
-}
thenEvt :: Evt a -> (a -> Evt b) -> Evt b
thenEvt evt f = 
    Evt (\ k s@(EvtState (sid@(SyncId(tid, b, start, cnt)), p, dm)) ->
             let thenEvtAct = do
                   let k' = (\ x s ->
                                 case x of
                                   Always x -> forceEvt (f x) k s
                                   Throw exn -> k (Throw exn) s)
                   forceEvt evt k' s in
             fizzleEvt "thenEvt" s thenEvtAct)

{-|
The never commitable event computation; 
the 'mzero' of the `Evt` monad.
-}
neverEvt :: Evt a
neverEvt = 
    Evt (\ k s@(EvtState (sid@(SyncId(tid, b, start, cnt)), p, dm)) -> 
             let neverEvtAct = do
                   decCountRef tid cnt
                   return () in
             fizzleEvt "neverEvt" s neverEvtAct)

{-|
Non-deterministic composition of event compuations;
the 'mplus' of the `Evt` monad.
-}
chooseEvt :: Evt a -> Evt a -> Evt a
chooseEvt evt1 evt2 = 
    Evt (\ k s@(EvtState (sid@(SyncId(tid, b, start, cnt)), p, dm)) -> 
             let chooseEvtAct = do
                   decCountRef tid cnt
                   forkIO $ do
                     incCountRef tid cnt
                     let p' = (chooseLeft):p
                         tr' = Trail (sid, p')
                         dm' = Map.insert tid tr' dm
                         s' = EvtState (sid, p', dm')
                     forceEvt evt1 k s'
                   forkIO $ do
                     incCountRef tid cnt
                     let p' = (chooseRight):p
                         tr' = Trail (sid, p')
                         dm' = Map.insert tid tr' dm
                         s' = EvtState (sid, p', dm')
                     forceEvt evt2 k s'
                   return () in
             fizzleEvt "chooseEvt" s chooseEvtAct)

instance Monad Evt
    where 
      {-# INLINE return #-}
      {-# INLINE (>>=)  #-}
      {-# INLINE (>>)   #-}
      return x = alwaysEvt x
      evt >>= f = thenEvt evt f
      evt >> evt' = evt >>= (\ _ -> evt')
instance MonadPlus Evt
    where
      {-# INLINE mzero  #-}
      {-# INLINE mplus  #-}
      mzero = neverEvt
      mplus = chooseEvt
instance Functor Evt
    where
      {-# INLINE fmap   #-}
      fmap f evt = evt >>= return . f
instance Applicative Evt
	where
	  {-# INLINE pure   #-}
	  {-# INLINE (<*>)  #-}
	  pure = alwaysEvt
	  (<*>) = ap
instance Alternative Evt
	where
	  {-# INLINE empty  #-}
	  {-# INLINE (<|>)  #-}
	  empty = neverEvt
	  (<|>) = chooseEvt

----------------------------------------------------------------------
-- Exceptions.
----------------------------------------------------------------------

{-|
A variant of `Exception.throw` that can be used within the 'Evt' monad.
-}
throwEvt :: Exception e => e -> Evt a
throwEvt exn = 
    Evt (\ k s@(EvtState (sid@(SyncId(tid, b, start, cnt)), p, dm)) ->
             let throwEvtAct = k (Throw (Exception.toException exn)) s in
             fizzleEvt "throwEvt" s throwEvtAct)

{-|
Exception handling within the 'Evt' monad.
-}
catchEvt :: Exception e => Evt a -> (e -> Evt a) -> Evt a
catchEvt evt f =
    Evt (\ k s@(EvtState (sid@(SyncId(tid, b, start, cnt)), p, dm)) ->
             let catchEvtAct = do
                   let k' = \ x s ->
                                case x of
                                  Always x -> k (Always x) s
                                  Throw exn ->
                                      case Exception.fromException exn of
                                        Nothing -> k (Throw exn) s
                                        Just exn -> forceEvt (f exn) k s
                   forceEvt evt k' s in
             fizzleEvt "catchEvt" s catchEvtAct)

----------------------------------------------------------------------
-- Synchronous channels.
----------------------------------------------------------------------

-- A suspended sender records the event state of the sender, the value
-- sent, and a continuation to spawn a search thread when supplied
-- with the information from a matching recv.
-- A suspended receiver records the event state of the receiver, and a
-- continuation to spawn a search thread when supplied with the
-- information from a matching send.
type Sender a = (EvtState, a, EvtCont ())
type SendList a = [Sender a]
type Recver a = (EvtState, EvtCont a)
type RecvList a = [Recver a]

-- A synchronous channel is represented by a reference maintaining the
-- pair of a list of suspended senders and a list of suspended
-- receivers.
newtype SChan a = SChan (TVar (SendList a, RecvList a)) deriving (Eq)
{- ^
A `SChan` is a synchronous channel, used for communication between
concurrent threads.  Message passing is synchronous: both the sender
and the receiver must be ready to communicate before either can
proceed.
-}

{-| 
Create a new synchronous channel.
-}
newSChan :: Evt (SChan a)
newSChan = 
    Evt (\ k s@(EvtState (sid@(SyncId(tid, b, start, cnt)), p, dm)) ->
             let newSChanAct = do
                   ch <- newTVarIO ([], [])
                   k (Always (SChan ch)) s in
             fizzleEvt "newSChan" s newSChanAct)

-- Clean a channel of inactive entries, while adding new entries;
-- returns the cleaned lists.
cleanSChan :: SChan a -> (SendList a, RecvList a) -> STM (SendList a, RecvList a)
cleanSChan (SChan ch) (newSends, newRecvs) = do
  (sends, recvs) <- readTVar ch
  sends' <- foldM (\ sends send@(EvtState (sid, p, dm), x, k) -> do
                     bv <- activeDepTrailMap readBoolRefClean dm
                     if bv
                        then return (send : sends)
                        else return sends)
                  newSends
                  sends
  recvs' <- foldM (\ recvs recv@(EvtState (sid, p, dm), k) -> do
                     bv <- activeDepTrailMap readBoolRefClean dm
                     if bv
                        then return (recv : recvs)
                        else return recvs)
                  newRecvs
                  recvs
  writeTVar ch (sends', recvs') 
  return (sends', recvs')

-- Spawn search threads for coherent communication;
-- the sender and receiver are assumed to be active.
spawnSendRecv :: Sender a -> Recver a -> IO ()
spawnSendRecv (EvtState(sids@(SyncId (tids, _, _, cnts)), ps, dms), x, ks) 
              (EvtState(sidr@(SyncId (tidr, _, _, cntr)), pr, dmr), kr) = do
  let trs = Trail (sids, ps) 
  let trr = Trail (sidr, pr)
  let mdm = mkCoherentDepTrailMap (trs, dms) (trr, dmr)
  when doChecks $
    unless (checkCoherentDepTrailMap (trs, dms) (trr, dmr) mdm) $
      fail "spawnSendRecv -- checkCoherentDepTrailMap failed"
  case mdm of
    Nothing -> return ()
    Just dm -> do
      cs <- newCompletionRef
      cr <- newCompletionRef
      let ps' = (commSend trr cr cs):ps
          pr' = (commRecv trs cs cr):pr
          trs' = Trail (sids, ps')
          trr' = Trail (sidr, pr')
          dm' = Map.insert tids trs' (Map.insert tidr trr' dm)
      forkIO $ do
         incCountRef tids cnts
         ks (Always ()) (EvtState (sids, ps', dm'))
      forkIO $ do
         incCountRef tidr cntr
         kr (Always x) (EvtState (sidr, pr', dm'))
      when doYields yield
      return ()

{-| 
Send a value on the channel.
-}
sendEvt :: SChan a -> a -> Evt ()
sendEvt ch x = 
    Evt (\ k s@(EvtState (sid@(SyncId(tid, b, start, cnt)), p, dm)) ->
             let sendEvtAct = do
                   let send = (s, x, k)
                   -- Note that by atomically adding ourselves to the
                   -- send list and taking a copy of the recv list,
                   -- we send to all receivers already on the recv
                   -- list and are on the send list for all future
                   -- receivers.
                   (_, recvs) <- atomically $ cleanSChan ch ([send],[])
                   decCountRef tid cnt
                   mapM_ (\ recv -> spawnSendRecv send recv) recvs
                   return () in
             fizzleEvt "sendEvt" s sendEvtAct)

{-| 
Receive a value on the channel.
-}
recvEvt :: SChan a -> Evt a
recvEvt ch = 
    Evt (\ k s@(EvtState (sid@(SyncId(tid, b, start, cnt)), p, dm)) ->
             let recvEvtAct = do
                   decCountRef tid cnt
                   let recv = (s, k)
                   -- Note that by atomically adding ourselves to the
                   -- recv list and taking a copy of the send list,
                   -- we recv from all senders already on the send
                   -- list and are on the recv list for all future
                   -- senders.
                   (sends, _) <- atomically $ cleanSChan ch ([],[recv])
                   mapM_ (\ send -> spawnSendRecv send recv) sends
                   return () in
             fizzleEvt "recvEvt" s recvEvtAct)

----------------------------------------------------------------------
-- Time delays.
----------------------------------------------------------------------

-- 
zeroUTCTime = Clock.UTCTime {Clock.utctDay = toEnum 0, Clock.utctDayTime = 0}

-- Utility function to convert NominalDiffTime values to microseconds.
nominalDiffTimeToMicroSeconds :: NominalDiffTime -> Int
nominalDiffTimeToMicroSeconds ndt =
    ceiling (toRational ndt * 1000000)

-- -- Utility function to convert TimeDiff values to microseconds.
-- timeDiffToMicroSeconds :: TimeDiff -> Int
-- timeDiffToMicroSeconds td =
--     fromIntegral ((secs * 100000) + (Time.tdPicosec td) `quot` 1000000)
--     where 
--       secs = toInteger (Time.tdSec td)
--              + 60 * (toInteger (Time.tdMin td)
--                      + 60 * (toInteger (Time.tdHour td)
--                              + 24 * (toInteger (Time.tdDay td)
--                                      + 30 * toInteger (Time.tdMonth td)
--                                      + 365 * toInteger (Time.tdYear td))))


{-|
Create an event that becomes commitable in the given number of
microseconds after the start of synchronization.  
-}
timeOutEvt :: Int -> Evt ()
timeOutEvt us = 
    Evt (\ k s@(EvtState (sid@(SyncId(tid, b, start, cnt)), p, dm)) ->
             let timeOutEvtAct = do
                   current <- Clock.getCurrentTime
                   let diff = nominalDiffTimeToMicroSeconds (Clock.diffUTCTime current start)
                   if us <= diff
                      then k (Always ()) s
                      else do threadDelay (us - diff)
                              fizzleEvt "timeOutEvt" s (k (Always ()) s) in
             fizzleEvt "timeOutEvt" s timeOutEvtAct)


{-|
Create an event that becomes commitable at the specified time.
-}
utcTimeEvt :: UTCTime -> Evt ()
utcTimeEvt end =
    Evt (\ k s@(EvtState (sid@(SyncId(tid, b, start, cnt)), p, dm)) ->
             let utcTimeEvtAct = do
                   current <- Clock.getCurrentTime
                   if end <= current
                      then k (Always ()) s
                      else do let diff = Clock.diffUTCTime end current
                              threadDelay (nominalDiffTimeToMicroSeconds diff)
                              fizzleEvt "utcTimeEvt" s (k (Always ()) s) in
             fizzleEvt "utcTimeEvt" s utcTimeEvtAct)

{-|
Create an event that becomes commitable at the specified time interval
after the start of synchronization.
-}
nominalDiffTimeEvt :: NominalDiffTime -> Evt ()
nominalDiffTimeEvt ndt = 
    Evt (\ k s@(EvtState (sid@(SyncId(tid, b, start, cnt)), p, dm)) ->
             let timeDiffEvtAct = do
                   let end = Clock.addUTCTime ndt start
                   current <- Clock.getCurrentTime
                   if end <= current
                      then k (Always ()) s
                      else do let diff = Clock.diffUTCTime end current
                              threadDelay (nominalDiffTimeToMicroSeconds diff)
                              fizzleEvt "timeDiffEvt" s (k (Always ()) s) in
             fizzleEvt "timeDiffEvt" s timeDiffEvtAct)

----------------------------------------------------------------------
-- Miscellaneous.
----------------------------------------------------------------------

{-| 
An always commitable event computation that returns the 'ThreadId' of
the synchronizing thread.
-}
myThreadIdEvt :: Evt ThreadId
myThreadIdEvt =
    Evt (\ k s@(EvtState (sid@(SyncId(tid, b, start, cnt)), p, dm)) -> 
             let myThreadIdEvtAct = k (Always tid) s in
             fizzleEvt "myThreadIdEvt" s myThreadIdEvtAct)

unsafeIOtoEvt :: IO a -> Evt a
unsafeIOtoEvt act =
    Evt (\ k s@(EvtState (sid@(SyncId(tid, b, start, cnt)), p, dm)) ->
             let unsafeIOtoEvtAct = do
                   x <- Exception.catch (act >>= (return . Always)) (return . Throw)
                   k x s in
             fizzleEvt "unsafeIOtoEvt" s unsafeIOtoEvtAct)

{-|
@'forkEvt' evt f@ sparks off a new thread to synchronize on @evt@
(which must synchronize in the same synchronization group as the
parent synchronization); upon successful synchronization (with result
@r@), the sparked thread acts as @f r@.
-}
forkEvt :: Evt a -> (a -> IO ()) -> Evt ThreadId
forkEvt evt f = do
  ch <- newSChan
  let evt' = (recvEvt ch) >> evt
  tid' <- unsafeIOtoEvt (forkIO (sync evt' >>= f))
  sendEvt ch ()
  alwaysEvt tid'

----------------------------------------------------------------------
-- Synchronization.
----------------------------------------------------------------------

{-|
Synchronize on an event.
-}
sync :: Evt a -> IO a
-- Synchronization on an event proceeds as follows:
--  a) allocate a new boolean reference for this synchronization,
--  b) allocate a new result reference for this synchronization,
--  c) spawn a search thread with an initial continuation and intial
--     event state,
--  d) block reading the TVar for the synchronization result.
sync evt = do
  tid <- myThreadId
  b <- newBoolRef
  r <- newResultRef
  forkIO $ initSearchThread tid b r evt
  Exception.catch (atomically $ readResult r)
                  (\ exn -> do
                     -- The exception is necessarily asynchronous.
                     -- Set the boolean reference to True to fizzle
                     -- all search threads.  Note: if the flag is
                     -- still False, then the semantics is as though
                     -- the exception arrived before the
                     -- synchronization.  If the flag is already True,
                     -- then the semantics is as though the exception
                     -- arrived after the synchronization.  In either
                     -- case, the atomicity of the synchronization is
                     -- preserved.
                     do atomically $ writeBoolRefClean b
                        writeBoolRefDirty b
                     Exception.throwIO (exn :: SomeException))

-- Initial search thread.
initSearchThread :: ThreadId -> BoolRef -> ResultRef a -> Evt a -> IO ()
initSearchThread tid b r evt = do
  start <- Clock.getCurrentTime
  cnt <- newCountRef
  let sid = SyncId (tid, b, start, cnt)
  let k = initEvtCont r
      p = []
      tr = Trail (sid, p)
      dm = Map.singleton tid tr
      s = EvtState (sid, p, dm)
  incCountRef tid cnt
  forceEvt evt k s
        
-- Initial event synchronization continuation.
initEvtCont :: ResultRef a -> EvtCont a
initEvtCont r x s@(EvtState (sid@(SyncId(tid, b, start, cnt)), p, dm)) = do
  when doChecks $
    unless (checkEvtState s) $
      fail "initEvtCont -- checkEvtState failed"
  decCountRef tid cnt
  case x of
    Throw _ -> do
      -- An uncaught exception fizzles the path.
      return ()
    Always x -> do
      -- Save IO action that will return the synchronization result.
      let act = atomically $ writeResultRef r x
      let tr = Track (sid, p, act)
      finishTrack tr

-- Finish processing a track (completed search).
finishTrack :: Track -> IO ()
finishTrack tr = do
  atomically $ addTrack tr
  (join . atomically) $ do
            ctms <- commitScan tr
            when doChecks $ do
              bvs <- Traversable.mapM checkCommitTrackMap ctms
              unless (Foldable.and bvs) $
                fail "finishTrack -- checkCommitTrackMap"
            case ctms of
              ctm:_ -> do Foldable.mapM_ writeBoolRefCleanInTrack ctm
                          return (do Foldable.mapM_ writeBoolRefDirtyInTrack ctm
                                     Foldable.mapM_ writeResultRefInTrack ctm)
              _ -> return (return ())
  return ()
  where 
    writeBoolRefCleanInTrack (Track (SyncId (_, b, _, _), _, _)) = writeBoolRefClean b
    writeBoolRefDirtyInTrack (Track (SyncId (_, b, _, _), _, _)) = writeBoolRefDirty b
    writeResultRefInTrack (Track (_, _, act)) = act
