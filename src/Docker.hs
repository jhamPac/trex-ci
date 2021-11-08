module Docker where

import qualified Data.Aeson          as Aeson
import           Data.Aeson.Types    as Aeson.Types
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

newtype ContainerId = ContainerId Text
    deriving (Eq, Show)

createContainer :: CreateContainerOptions -> IO ContainerId
createContainer options = do
    manager <- Socket.newManager "/var/run/docker.sock"
    let image = imageToText options.image
    let body = Aeson.object
                    [
                        ("Image", Aeson.toJSON image),
                        ("Tty", Aeson.toJSON True),
                        ("Labels", Aeson.object [("t-rex", "")]),
                        ("Cmd", "echo T-Rex is here, ROARRR"),
                        ("Entrypoint", Aeson.toJSON [Aeson.String "/bin/sh", "-c"])
                    ]
    let req = HTTP.defaultRequest
            & HTTP.setRequestManager manager
            & HTTP.setRequestPath "/containers/create"
            & HTTP.setRequestMethod "POST"
            & HTTP.setRequestBodyJSON body
    let parser = Aeson.withObject "create-container" $ \o -> do
            cId <- o .: "Id"
            pure $ ContainerId cId

    res <- HTTP.httpBS req
    parseResponse res parser

parseResponse
    :: HTTP.Response ByteString
    -> (Aeson.Value -> Aeson.Types.Parser a)
    -> IO a

parseResponse res parser = do
    let result = do
            value <- Aeson.eitherDecodeStrict (HTTP.getResponseBody res)
            Aeson.Types.parseEither parser value

    case result of
        Left e       -> throwString e
        Right status -> pure status

imageToText :: Image -> Text
imageToText (Image t) = t

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode code) = code

