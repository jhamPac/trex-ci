module Docker where

import qualified Data.Aeson          as Aeson
import           Data.Aeson.Types    as Aeson.Types
import qualified Network.HTTP.Simple as HTTP
import           RIO
import qualified Socket

data Service = Service {
        createContainer :: CreateContainerOptions -> IO ContainerId,
        startContainer  :: ContainerId -> IO (),
        containerStatus :: ContainerId -> IO ContainerStatus,
        createVolume    :: IO Volume
    }

data CreateContainerOptions = CreateContainerOptions {
        image  :: Image,
        script :: Text
    }

data ContainerStatus
    = ContainerRunning
    | ContainerExited ContainerExitCode
    | ContainerOther Text
    deriving (Eq, Show)

newtype Image = Image Text
    deriving (Eq, Show)

newtype ContainerExitCode = ContainerExitCode Int
    deriving (Eq, Show)

newtype ContainerId = ContainerId Text
    deriving (Eq, Show)

newtype Volume = Volume Text
    deriving (Eq, Show)

type RequestBuilder = Text -> HTTP.Request

imageToText :: Image -> Text
imageToText (Image t) = t

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode code) = code

containerIdToText :: ContainerId -> Text
containerIdToText (ContainerId t) = t

createService :: IO Service
createService = do
    manager <- Socket.newManager "/var/run/docker.sock"

    let makeReq :: RequestBuilder
        makeReq path =
            HTTP.defaultRequest
                & HTTP.setRequestPath (encodeUtf8 $ "/v1.41" <> path)
                & HTTP.setRequestManager manager

    pure Service {
            createContainer = createContainer' makeReq,
            startContainer = startContainer' makeReq,
            containerStatus = containerStatus' makeReq,
            createVolume = createVolume' makeReq
        }

createContainer' :: RequestBuilder -> CreateContainerOptions -> IO ContainerId
createContainer' makeReq options = do
    let image = imageToText options.image
    let body = Aeson.object
                    [
                        ("Image", Aeson.toJSON image),
                        ("Tty", Aeson.toJSON True),
                        ("Labels", Aeson.object [("trex", "")]),
                        ("Entrypoint", Aeson.toJSON [Aeson.String "/bin/sh", "-c"]),
                        ("Cmd", "echo \"$TREX_SCRIPT\" | /bin/sh"),
                        ("Env", Aeson.toJSON ["TREX_SCRIPT=" <> options.script])
                    ]
    let path = "/containers/create"
    let req = makeReq path
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

startContainer' :: RequestBuilder -> ContainerId -> IO ()
startContainer' makeReq id = do
    let path =
            "/containers/" <> containerIdToText id <> "/start"

    let req = makeReq path
                & HTTP.setRequestMethod "POST"

    void $ HTTP.httpBS req

containerStatus' :: RequestBuilder -> ContainerId -> IO ContainerStatus
containerStatus' makeReq container = do
    let parser = Aeson.withObject "container-inspect" $ \o -> do
                    state <- o .: "State"
                    status <- state .: "Status"
                    case status of
                        "running" -> pure ContainerRunning
                        "exited" -> do
                            code <- state .: "ExitCode"
                            pure $ ContainerExited (ContainerExitCode code)
                        other -> pure $ ContainerOther other

    let req = makeReq $ "/containers/" <> containerIdToText container <> "/json"

    res <- HTTP.httpBS req
    parseResponse res parser

createVolume' :: RequestBuilder -> IO Volume
createVolume' makeReq = do
    let body = Aeson.object [
                    ("Labels", Aeson.object [("trex", "")])
                ]

    let req = makeReq "/volumes/create"
                & HTTP.setRequestMethod "POST"
                & HTTP.setRequestBodyJSON body

    let parser = Aeson.withObject "create-volume" $ \o -> do
            name <- o .: "Name"
            pure $ Volume name

    res <- HTTP.httpBS req
    parseResponse res parser
