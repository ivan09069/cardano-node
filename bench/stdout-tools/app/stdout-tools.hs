{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ExistentialQuantification #-}

{-

nix-shell OR nix-shell -p ghc cabal-install haskellPackages.eventlog2html jq

Count lines
--------------------------------------------------------------------------------

> ls -lah bench/stdout-tools/5nodes.stdout
-rw-r--r-- 1 fmaste users 6.4G Apr 10 19:28 bench/stdout-tools/5nodes.stdout

> time cat bench/stdout-tools/5nodes.stdout | wc -l
real  0m1.946s
user  0m0.105s
sys 0m2.728s

> time jq --raw-input . bench/stdout-tools/5nodes.stdout | wc -l
25581640
real  1m30.707s
user  1m28.129s
sys 0m8.124s

lineFoldl' (\l _ -> l + 1) (0::Int) fp >>= print
> time cabal run stdout-tools -- --file big-node:bench/stdout-tools/5nodes.stdout --filter count-lines
25581640
real  0m15.605s
user  0m13.545s
sys 0m2.047s

Count all the ns="Forge.Loop.StartLeadershipCheckPlus"
--------------------------------------------------------------------------------

-- Using jq for everything:
> time jq --raw-input --compact-output 'try fromjson | if (type == "object" and has("at")) then select(.ns=="Forge.Loop.StartLeadershipCheckPlus") else empty end' bench/stdout-tools/5nodes.stdout | wc -l
264150
real  1m30.615s
user  1m29.159s
sys 0m1.502s

-- Using jq but first filter non JSON lines with grep:
> time grep -E "^{.*" bench/stdout-tools/5nodes.stdout | jq --compact-output 'select(.ns == "Forge.Loop.StartLeadershipCheckPlus")' | wc -l
264150
real  1m9.828s
user  1m12.247s
sys 0m5.901s

$ time cabal run stdout-tools -- --file big-node:bench/stdout-tools/5nodes.stdout --filter count-FLSLCP
264150
real  1m2.851s
user  1m0.511s
sys 0m2.262s

-}

{-- RTS params:

-N:
There are two ways to run a program on multiple processors: call
Control.Concurrent.setNumCapabilities from your program, or use the RTS -N ⟨x⟩
options. -N⟨x⟩

-s:
Add the -s [⟨file⟩] RTS option when running the program to see timing stats,
which will help to tell you whether your program got faster by using more CPUs
or not. If the user time is greater than the elapsed time, then the program used
more than one CPU. You should also run the program without -N ⟨x⟩ for
comparison.

The output of +RTS -s tells you how many “sparks” were created and executed
during the run of the program (see RTS options to control the garbage
collector), which will give you an idea how well your par annotations are
working.

> eventlog2html stdout-tools.eventlog

Cabal
 --enable-profiling             Enable Executable and library profiling
 --disable-profiling            Disable Executable and library profiling
 --profiling-detail=level       Profiling detail level for executable and
                                library (default, none, exported-functions,
                                toplevel-functions, all-functions, late).
 --library-profiling-detail=level
                                Profiling detail level for libraries only.
--}

--------------------------------------------------------------------------------

module Main (main) where

--------------------------------------------------------------------------------

-- base.
import           Control.Applicative (some)
import           GHC.Generics
import           Data.Kind (Type)

-- package: time.
import           Data.Time.Clock
  ( UTCTime
  , getCurrentTime
  , NominalDiffTime
  , diffUTCTime
  )
-- package: containers.
import qualified Data.Sequence as Seq
-- package: text.
import qualified Data.Text as Text
-- package: aeson.
import qualified Data.Aeson as Aeson
-- package: async.
import qualified Control.Concurrent.Async as Async
-- package: optparse-applicative.
import qualified Options.Applicative as Opt

import qualified Data.Log as Log

--------------------------------------------------------------------------------

data CliOpts = CliOpts
  {
  -- "--file" arguments with an optional file label if ":" separator is found.
    files      :: [(String, FilePath)]
  , inParallel :: Bool
  -- "--reducer" arguments.
  , reducers   :: [ReducerElem]
  }
  deriving Show

data ReducerElem = forall r. (Show r, Reducer r) => MkReducer r

instance Show ReducerElem where
  show (MkReducer r) = show r

cliFilterReader :: String -> Either String ReducerElem
cliFilterReader str = case str of
  "count-lines"  -> Right $ MkReducer CountLines
  "count-FLSLCP" -> Right $ MkReducer CountStartLeadershipCheckPlus
  "heap-changes" -> Right $ MkReducer HeapChanges
  "missed-slots" -> Right $ MkReducer MissedSlots
  "1s-silences"  -> Right $ MkReducer OneSecondSilences
  _ -> Left str

main :: IO ()
main = do
  cliOpts <- Opt.execParser $ Opt.info (optsParser Opt.<**> Opt.helper)
    (     Opt.fullDesc
       <> Opt.progDesc "Print a greeting for TARGET"
       <> Opt.header "hello - a test for optparse-applicative"
    )
  run cliOpts

--------------------------------------------------------------------------------

class Show r => Reducer r where
  type family Accum r :: Type
  initialOf :: r -> Accum r
  reducerOf :: r -> Accum r -> Cursor -> Accum r
  showAns   :: r -> Accum r -> String

data CountLines = CountLines
  deriving Show

data CountStartLeadershipCheckPlus = CountStartLeadershipCheckPlus
  deriving Show

data HeapChanges = HeapChanges
  deriving Show

data MissedSlots = MissedSlots
  deriving Show

data OneSecondSilences = OneSecondSilences
  deriving Show

instance Reducer CountLines where
  type instance Accum CountLines = Int
  initialOf _ = 0
  reducerOf _ = (\l _ -> l + 1)
  showAns   _ = show

instance Reducer CountStartLeadershipCheckPlus where
  type instance Accum CountStartLeadershipCheckPlus = Int
  initialOf _ = 0
  reducerOf _ = (\l (Cursor _ maybeMsg) ->
    case maybeMsg of
      Nothing -> l
      (Just msg) ->
        if ns msg == "Forge.Loop.StartLeadershipCheckPlus"
        then l + 1
        else l
    )
  showAns  _ = show
instance Reducer HeapChanges where
  type instance Accum HeapChanges = (Maybe Integer, Seq.Seq (UTCTime, Integer))
  initialOf _ = (Nothing, Seq.empty)
  reducerOf _ ans (Cursor _ Nothing) = ans
  reducerOf _ ans@(maybePrevHeap, sq) (Cursor _ (Just !cursorMsg)) =
    case Aeson.fromJSON (msgData cursorMsg) of
      (Aeson.Success !resources) ->
        -- TODO: Use `unsnoc` when available
        let actualHeap = resourcesHeap resources
        in case maybePrevHeap of
          Nothing -> (Just actualHeap, Seq.singleton (at cursorMsg, actualHeap))
          (Just prevHeap) ->
            if actualHeap == prevHeap
            then ans
            else (Just actualHeap, sq Seq.|> (at cursorMsg, actualHeap))
      (Aeson.Error _) -> ans
  showAns _ = show

instance Reducer MissedSlots where
  type instance Accum MissedSlots = (Maybe Integer, Seq.Seq Integer)
  initialOf _ = (Nothing, Seq.empty)
  reducerOf _ ans (Cursor _ Nothing) = ans
  reducerOf _ ans@(maybePrevSlot, !sq) (Cursor _ (Just !(Msg _ "Forge.Loop.StartLeadershipCheckPlus" aeson))) =
    case Aeson.fromJSON aeson of
      (Aeson.Success !dataWithSlot) ->
        -- TODO: Use `unsnoc` when available
        let actualSlot = slot dataWithSlot
        in case maybePrevSlot of
          Nothing -> (Just actualSlot, Seq.empty)
          (Just prevSlot) ->
            if actualSlot == prevSlot + 1
            then (Just actualSlot, sq)
            else (Just actualSlot, sq Seq.>< (Seq.fromList [(prevSlot+1)..(actualSlot-1)]))
      (Aeson.Error _) -> ans
  reducerOf _ ans (Cursor _ (Just _)) = ans
  showAns _ = show

instance Reducer OneSecondSilences where
  type instance Accum OneSecondSilences = (Maybe Msg, Seq.Seq (NominalDiffTime, Msg, Msg))
  initialOf _ = (Nothing, Seq.empty)
  reducerOf _ (Nothing, sq) cursor = (_maybeCursorMsg cursor, sq)
  reducerOf _ (Just _prevMsg, sq) (Cursor _ Nothing) = (Just _prevMsg, sq)
  reducerOf _ (Just _prevMsg, sq) (Cursor _ (Just cursorMsg)) =
    let diffTime = diffUTCTime (at cursorMsg) (at _prevMsg)
    in if diffTime >  fromInteger 2
    then (Just cursorMsg, sq Seq.|> (diffTime, _prevMsg, cursorMsg))
    else (Just cursorMsg, sq)
  showAns   _ = show

--------------------------------------------------------------------------------

optsParser :: Opt.Parser CliOpts
optsParser = CliOpts <$>
        (map
          -- Parse the optional file label, looks for ":" as separator.
          (\str ->
            case span (/= ':') str of
              (f,"") -> ("",f)
              (f, s) -> (f,drop 1 s)
          )
          <$>
          some (
            Opt.strOption
            (    Opt.long "file"
              <> Opt.short 'f'
              <> Opt.metavar "FILENAME"
              <> Opt.help "Input file"
            )
          )
        )
    <*> Opt.flag False True
          (    Opt.long "parallel"
            <> Opt.help "Process files in parallel"
          )
    <*> (some (
          (Opt.option $ Opt.eitherReader cliFilterReader)
          (    Opt.long "filter"
            <> Opt.short 'f'
            <> Opt.metavar "FILTER"
            <> Opt.help "Filter"
          )
        ))

--------------------------------------------------------------------------------

run :: CliOpts -> IO ()
run (CliOpts _ _ []) = putStrLn "Nothing to do, bye!"
run cliOpts@(CliOpts _ parallel ((MkReducer r):_)) = do
  t0 <- getCurrentTime
  print r
  if not parallel
  then do
    --------------------------------------------------------
    putStrLn "---------------------------------------------"
    putStrLn "Do something with all files (NOT in parallel)"
    putStrLn "---------------------------------------------"
    --------------------------------------------------------
    mapM_
      (\(logName,fp) -> do
        ans <- lineFoldl'
          (reducerOf r)
          (initialOf r)
          fp
        print logName
        putStrLn $ showAns r ans
      )
      (files cliOpts)
  else do
    ---------------------------------------------------------
    putStrLn "----------------------------------------------"
    putStrLn "Do the same with all files but now in parallel"
    putStrLn "----------------------------------------------"
    ---------------------------------------------------------
    ansParallel <- Async.mapConcurrently
      (\(logName,fp) -> do
        ans <- lineFoldl'
          (reducerOf r)
          (initialOf r)
          fp
        return (logName, ans)
      )
      (files cliOpts)
    mapM_
      (\(logName,ans) -> do
        print logName
        putStrLn $ showAns r ans
      )
      ansParallel
  t1 <- getCurrentTime
  print $ diffUTCTime t1 t0

{-- TODO: Switch to open type families for "sequential" and "parallel" folds.
  mapM_
    (\(logName,fp) -> do

      ans <- lineFoldl'
        (\accs cursor -> zipWith (\r' acc -> reducerOf r' acc cursor) rs accs)
        (map initialOf rs)
        fp
      print logName
      mapM_ (\(r, acc) -> putStrLn $ showAns r acc) (zip rs ans)
    )
    (files cliOpts)
--}

  -- End
  return ()

-- A log message.
--------------------------------------------------------------------------------

-- Keep it simple!
-- TODO:
-- All traces start with, use this assumption to build a "fast" decoder.
-- {"at":"2024-03-30T00:30:27.015631111Z","ns":"Reflection.TracerInfo"
data Msg = Msg
  -- Strict of keep thunks of `Data.Time.FromText.parseUTCTime`.
  { at :: UTCTime -- "2024-04-06T11:27:45.37268578Z"
  , ns :: Text.Text
  -- Only does a final `fromJSON` if needed!
  , msgData :: Aeson.Value
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Msg where
  -- Only using a non-automatic instance because of "data" and "msgData".
  toJSON p@(Msg _ _ _) =
    Aeson.object
      [ "at"   Aeson..= at p
      , "ns"   Aeson..= ns p
      , "data" Aeson..= msgData p
      ]

instance Aeson.FromJSON Msg where
  -- Only using a non-automatic instance because of "data" and "msgData".
  parseJSON =
    Aeson.withObject "Msg" $ \o -> do
      Msg
        <$> o Aeson..: "at"
        <*> o Aeson..: "ns"
        <*> o Aeson..: "data"

{--
class Cursor a where
  cursorText :: a -> Text.Text
  maybeCursorMsg :: a -> Maybe Msg

instance Cursor BasicCursor where
  cursorText = _cursorText
  maybeCursorMsg = _cursorText
--}

-- Keep it simple, stak accumulators if you need more things like line number.
data Cursor = Cursor
  { _cursorText :: Text.Text
  -- All lines are read (converted to Text) but `fromJSON` is lazy.
  , _maybeCursorMsg :: Maybe Msg
  }

-- Allow to `fold'` through the log file but in JSON format.
lineFoldl' :: (a -> Cursor -> a) -> a -> FilePath -> IO a
lineFoldl' f initialAcc filePath = do
  Log.lineFoldl'
    (\acc textLine ->
      let maybeMsg = case Aeson.eitherDecodeStrictText textLine of
                      (Left _) -> Nothing
                      (Right msg) -> Just msg
          -- CRITICAL: Has to be "STRICT" to keep `Log.lineFoldl'`'s behaviour.
          --           I repeat, the accumulator function has to be strict!
          !nextAcc = f acc (Cursor textLine maybeMsg)
      in nextAcc
    )
    initialAcc
    filePath

--------------------------------------------------------------------------------

{--
-- Keep it simple!
data MsgAt = MsgAt
  -- Strict of keep thunks of `Data.Time.FromText.parseUTCTime`.
  { at :: !UTCTime -- "2024-04-06T11:27:45.37268578Z"
  }
  deriving (Eq, Show, Generic)

instance Aeson.FromJSON Msg where

instance Aeson.ToJSON Msg where
--}

-- TODO:
_foldlLog3 :: (
               ((a -> Cursor -> a), a)
             , ((b -> Cursor -> b), b)
             , ((c -> Cursor -> c), c)
             )
          -> FilePath
          -> IO (a,b,c)
{-# SCC _foldlLog3 "_foldlLog3" #-}
_foldlLog3 ((fa,a),(fb,b),(fc,c)) filePath = do
  (a', b', c') <- lineFoldl'
    (\(accA,accB,accC) cursor -> {-# SCC "foldlLog3_f" #-}
        (fa accA cursor, fb accB cursor, fc accC cursor)
    )
    (a,b,c)
    filePath
  --return $! (a', b', c')
  return $ seq a' $ seq b' $ seq c' (a', b', c')

{-- TODO:
foldlWhile :: Foldable t => (a -> Bool) -> (r -> a -> r) -> r -> t a -> r
foldlWhile t f a xs  =  foldr cons (\acc -> acc) xs a
  where
    cons x r acc | t x  =  r (f acc x)
                 | otherwise  =  acc
--}

--------------------------------------------------------------------------------

{--
  "data": {
    "Alloc": 98169147912,
    "CentiBlkIO": 0,
    "CentiCpu": 10831,
    "CentiGC": 4726,
    "CentiMut": 6104,
    "FsRd": 0,
    "FsWr": 8192,
    "GcsMajor": 7,
    "GcsMinor": 3590,
    "Heap": 8629780480,
    "Live": 2529171488,
    "NetRd": 0,
    "NetWr": 0,
    "RSS": 8683200512,
    "Threads": 9,
    "kind": "ResourceStats"
  },
--}
data Resources = Resources
  { resourcesAlloc :: Integer
  , resourcesCentiBlkIO :: Integer
  , resourcesCentiCpu :: Integer
  , resourcesCentiGC :: Integer
  , resourcesCentiMut :: Integer
  , resourcesFsRd :: Integer
  , resourcesGcsMajor :: Integer
  , resourcesGcsMinor :: Integer
  , resourcesHeap :: Integer
  , resourcesLive :: Integer
  , resourcesNetRd :: Integer
  , resourcesNetWr :: Integer
  , resourcesRSS :: Integer
  , resourcesThreads :: Integer
  }
  deriving (Eq, Show, Generic)

msgCustomOptions :: Aeson.Options
msgCustomOptions = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = drop 9
  , Aeson.constructorTagModifier = id
  }

instance Aeson.ToJSON Resources where
  toJSON = Aeson.genericToJSON msgCustomOptions
  toEncoding = Aeson.genericToEncoding msgCustomOptions

instance Aeson.FromJSON Resources where
  parseJSON = Aeson.genericParseJSON msgCustomOptions

--------------------------------------------------------------------------------

data DataWithSlot = DataWithSlot
  { slot :: Integer }
  deriving Generic

instance Aeson.ToJSON DataWithSlot where

instance Aeson.FromJSON DataWithSlot where
