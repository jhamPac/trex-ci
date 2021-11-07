module Docker where

import qualified Data.Aeson          as Aeson
import qualified Network.HTTP.Simple as HTTP
import           RIO
import qualified Socket

data CreateContainerOptions = CreateContainerOptions {
    image :: Image
    }

newtype Image = Image Text
    deriving (Eq, Show)

newtype ContainerExitCode = ContainerExitCode Int
    deriving (Eq, Show)

createContainer :: CreateContainerOptions -> IO ()
createContainer options = do
    manager <- Socket.newManager "/var/run/docker.sock"
    let image = imageToText options.image
    let body = Aeson.object
                    [
                        ("Image", Aeson.toJSON image)
                    ]
    let req = HTTP.defaultRequest
            & HTTP.setRequestManager manager
            & HTTP.setRequestPath "/containers/create"
            & HTTP.setRequestMethod "POST"
            & HTTP.setRequestBodyJSON body
    res <- HTTP.httpBS req
    traceShowIO res

imageToText :: Image -> Text
imageToText (Image t) = t

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode code) = code

