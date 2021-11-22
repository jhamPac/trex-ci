module Runner where

import           Core
import qualified Docker
import           RIO

data Hooks = Hooks {
        logCollected :: Log -> IO ()
    }

data Service = Service {
        prepareBuild :: Pipeline -> IO Build,
        runBuild     :: Hooks -> Build -> IO Build
    }

createService :: Docker.Service -> IO Service
createService docker = do
    pure Service {
        prepareBuild = prepareBuild' docker,
        runBuild = runBuild' docker
    }

prepareBuild' :: Docker.Service -> Pipeline -> IO Build
prepareBuild' docker pipeline = do
    volume <- docker.createVolume
    pure Build {
        pipeline = pipeline,
        state = BuildReady,
        completedSteps = mempty,
        volume = volume
    }

runBuild' :: Docker.Service -> Hooks -> Build -> IO Build
runBuild' docker hooks build = do
    newBuild <- Core.progress docker build
    case newBuild.state of
        BuildFinished _ -> pure newBuild
        _ -> do
            threadDelay (1 * 1000 * 1000)
            runBuild' docker hooks newBuild
