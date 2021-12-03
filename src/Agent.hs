module Agent where

import qualified Codec.Serialise     as Serialise
import           Core
import qualified Network.HTTP.Simple as HTTP
import           RIO

data Config = Config { endpoint :: String }

data Cmd
    = StartBuild BuildNumber Pipeline
    deriving (Eq, Show, Generic, Serialise.Serialise)

data Msg
    = LogCollected BuildNumber Log
    | BuildUpdated BuildNumber Build
    deriving (Eq, Show, Generic, Serialise.Serialise)

run :: Config -> IO ()
run config = forever do
        endpoint <- HTTP.parseRequest config.endpoint

        let req = endpoint
                    & HTTP.setRequestMethod "POST"
                    & HTTP.setRequestPath "/agent/pull"

        res <- HTTP.httpLBS req
        let cmd = Serialise.deserialise (HTTP.getResponseBody res) :: Maybe Cmd
        threadDelay (1 * 1000 * 1000)
