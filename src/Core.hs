module Core where

import           Data.Aeson            as Aeson
import qualified Data.Time.Clock.POSIX as Time
import qualified Docker
import           RIO
import           RIO.List              as List
import           RIO.Map               as Map
import qualified RIO.NonEmpty          as NonEmpty
import qualified RIO.Text              as Text

data Pipeline = Pipeline {
        steps :: NonEmpty Step
    } deriving (Eq, Show, Generic, Aeson.FromJSON)

data Step = Step {
        name     :: StepName,
        commands :: NonEmpty Text,
        image    :: Docker.Image
    } deriving (Eq, Show, Generic, Aeson.FromJSON)

data Build = Build {
        pipeline       :: Pipeline,
        state          :: BuildState,
        completedSteps :: Map StepName StepResult,
        volume         :: Docker.Volume
    } deriving (Eq, Show)

data StepResult
    = StepFailed Docker.ContainerExitCode
    | StepSucceeded
    deriving (Eq, Show)

data BuildState
    = BuildReady
    | BuildRunning BuildRunningState
    | BuildFinished BuildResult
    deriving (Eq, Show)

newtype BuildNumber = BuildNumber Int
    deriving (Eq, Show)

data BuildRunningState = BuildRunningState {
        step      :: StepName,
        container :: Docker.ContainerId
    } deriving (Eq, Show)

data BuildResult
    = BuildSucceeded
    | BuildFailed
    | BuildUnexpectedState Text
    deriving (Eq, Show)

newtype StepName = StepName Text
    deriving (Eq, Show, Ord, Generic, Aeson.FromJSON)

data Log = Log {
        output :: ByteString,
        step   :: StepName
    }
    deriving (Eq, Show)

type LogCollection = Map StepName CollectionStatus

data CollectionStatus
    = CollectionReady
    | CollectionLogs Docker.ContainerId Time.POSIXTime
    | CollectionFinished
    deriving (Eq, Show)

initLogCollection :: Pipeline -> LogCollection
initLogCollection pipeline = Map.fromList $ NonEmpty.toList steps
    where
        steps = pipeline.steps <&> \step -> (step.name, CollectionReady)

collectLogs
    :: Docker.Service
    -> LogCollection
    -> Build
    -> IO (LogCollection, [Log])

collectLogs docker collection build = do
    now <- Time.getPOSIXTime
    logs <- runCollection docker now collection
    let newCollection = updateCollection build.state now collection
    pure (newCollection, logs)

runCollection
    :: Docker.Service
    -> Time.POSIXTime
    -> LogCollection
    -> IO [Log]

runCollection docker collectUntil collection = do
    logs <- Map.traverseWithKey f collection
    pure $ concat (Map.elems logs)
    where
        f step = \case
            CollectionReady -> pure []
            CollectionFinished -> pure []
            CollectionLogs container since -> do
                let options =
                        Docker.FetchLogsOptions {
                                container = container,
                                since = since,
                                until = collectUntil
                            }
                output <- docker.fetchLogs options
                pure [Log {step = step, output = output}]

updateCollection
    :: BuildState
    -> Time.POSIXTime
    -> LogCollection
    -> LogCollection

updateCollection state lastTimestamp collection =
    Map.mapWithKey f collection
    where
        f step = \case
            CollectionReady ->
                update step 0 CollectionReady

            CollectionLogs _ _ ->
                update step lastTimestamp CollectionFinished

            CollectionFinished -> CollectionFinished

        update step timestamp nextState =
            case state of
                BuildRunning state ->
                    if state.step == step
                        then CollectionLogs state.container timestamp
                        else nextState
                _ -> nextState

stepNameToText :: StepName -> Text
stepNameToText (StepName t) = t

exitCodeToStepResult :: Docker.ContainerExitCode -> StepResult
exitCodeToStepResult exit
    | Docker.exitCodeToInt exit == 0 = StepSucceeded
    | otherwise = StepFailed exit

buildNumberToInt :: BuildNumber -> Int
buildNumberToInt (BuildNumber n) = n

buildHasNextStep :: Build -> Either BuildResult Step
buildHasNextStep build =
    if allSucceeded
        then case nextStep of
            Just step -> Right step
            Nothing   -> Left BuildSucceeded
        else Left BuildFailed
    where
        allSucceeded = List.all ((==) StepSucceeded) build.completedSteps
        nextStep = List.find f build.pipeline.steps
        f step = not $ Map.member step.name build.completedSteps

progress :: Docker.Service -> Build -> IO Build
progress docker build =
    case build.state of
        BuildReady      ->
            case buildHasNextStep build of
                Left result -> pure $ build { state = BuildFinished result }
                Right step  -> do
                    let script = Text.unlines $ ["set -ex"] <> NonEmpty.toList step.commands
                    let options = Docker.CreateContainerOptions {
                            image = step.image,
                            script = script,
                            volume = build.volume
                        }
                    docker.pullImage step.image
                    container <- docker.createContainer options
                    docker.startContainer container

                    let s = BuildRunningState {
                            step = step.name,
                            container = container
                        }
                    pure $ build { state = BuildRunning s }

        BuildRunning state   -> do
            status <- docker.containerStatus state.container

            case status of
                Docker.ContainerRunning ->
                    pure build

                Docker.ContainerExited exit -> do
                    let result = exitCodeToStepResult exit
                    pure build {
                            completedSteps = Map.insert state.step result build.completedSteps,
                            state = BuildReady
                        }

                Docker.ContainerOther other -> do
                    let s = BuildUnexpectedState other
                    pure build { state = BuildFinished s }

        BuildFinished _ -> pure build

