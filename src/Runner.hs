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
    loop build $ Core.initLogCollection build.pipeline
    where
        loop :: Build -> LogCollection -> IO Build
        loop build collection = do
            (newCollection, logs) <- Core.collectLogs docker collection
            traverse_ hooks.logCollected logs
            newBuild <- Core.progress docker build
            case newBuild.state of
                BuildFinished _ -> pure newBuild
                _ -> do
                    threadDelay (1 * 1000 * 1000)
                    loop newBuild newCollection
