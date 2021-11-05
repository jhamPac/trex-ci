module Core where

import           RIO

data Pipeline = Pipeline { steps :: NonEmpty Step }
    deriving (Eq, Show)

data Step = Step {
    name     :: StepName,
    commands :: NonEmpty Text,
    image    :: Image
} deriving (Eq, Show)

data Build = Build {
    pipeline       :: Pipeline,
    state          :: BuildState,
    completedSteps :: Map StepName StepResult
} deriving (Eq, Show)

data StepResult = StepFailed ContainerExitCode | StepSucceeded
    deriving (Eq, Show)

newtype ContainerExitCode = ContainerExitCode Int
    deriving (Eq, Show)

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode code) = code

data BuildState = BuildReady | BuildRunning | BuildFinished BuildResult
    deriving (Eq, Show)

data BuildResult = BuildSucceeded | BuildFailed
    deriving (Eq, Show)

newtype StepName = StepName Text
    deriving (Eq, Show)

newtype Image = Image Text
    deriving (Eq, Show)

stepNameToText :: StepName -> Text
stepNameToText (StepName t) = t

imageToText :: Image -> Text
imageToText (Image t) = t

progress :: Build -> IO Build
progress build =
    case build.state of
        BuildReady      -> undefined
        BuildRunning    -> undefined
        BuildFinished _ -> pure build

