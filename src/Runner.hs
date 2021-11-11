module Runner where

import           Core
import qualified Docker
import           RIO

data Service = Service {
        prepareBuild :: Pipeline -> IO Build,
        runBuild     :: Build -> IO Build
    }

createService :: Docker.Service -> IO Service
createService docker = do
    pure Service {
        prepareBuild = prepareBuild' docker,
        runBuild = runBuild' docker
    }

prepareBuild' :: Docker.Service -> Pipeline -> IO Build
prepareBuild' docker pipeline = do
    pure Build {
        pipeline = pipeline,
        state = BuildReady,
        completedSteps = mempty
    }

runBuild' :: Docker.Service -> Build -> IO Build
runBuild' docker build = do
    newBuild <- Core.progress docker build
    case newBuild.state of
        BuildFinished _ -> pure newBuild
        _ -> do
            threadDelay (1 * 1000 * 1000)
            runBuild' docker newBuild
