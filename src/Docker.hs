module Docker where

import qualified Network.HTTP.Simple as HTTP
import           RIO

data CreateContainerOptions = CreateContainerOptions {
    image :: Image
    }

newtype Image = Image Text
    deriving (Eq, Show)

newtype ContainerExitCode = ContainerExitCode Int
    deriving (Eq, Show)

createContainer :: CreateContainerOptions -> IO ()
createContainer options = do
    let body = ()
    let req = HTTP.defaultRequest
            & HTTP.setRequestPath "/containers/create"
            & HTTP.setRequestMethod "POST"
            & HTTP.setRequestBodyJSON body
    res <- HTTP.httpBS req
    traceShowIO res

imageToText :: Image -> Text
imageToText (Image t) = t

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode code) = code

