module Docker where

import           RIO

data CreateContainerOptions = CreateContainerOptions {
    image :: Image
    }

newtype Image = Image Text
    deriving (Eq, Show)

newtype ContainerExitCode = ContainerExitCode Int
    deriving (Eq, Show)

createContainer :: CreateContainerOptions -> IO ()
createContainer options = undefined

imageToText :: Image -> Text
imageToText (Image t) = t

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode code) = code
