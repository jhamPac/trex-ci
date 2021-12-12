import           Agent
import qualified Control.Concurrent.Async as Async
import           Core
import           Data.Yaml                as Yaml
import qualified Docker
import           JobHandler
import qualified JobHandler.Memory
import           RIO
import qualified RIO.ByteString           as ByteString
import qualified RIO.Map                  as Map
import           RIO.NonEmpty.Partial     as NE
import qualified RIO.Set                  as Set
import           Runner
import           Server
import           System.Process.Typed     as Process
import           Test.Hspec

makeStep :: Text -> Text -> [Text] -> Step
makeStep name image commands = Step {
        name = StepName name,
        image = Docker.Image { name = image, tag = "latest" },
        commands = NE.fromList commands
    }

makePipeline :: [Step] -> Pipeline
makePipeline steps = Pipeline {
        steps = NE.fromList steps
    }

cleanUpDocker :: IO ()
cleanUpDocker = void do
    Process.readProcessStdout "docker rm -f $(docker ps -aq --filter \"label=trex\")"
    Process.readProcessStdout "docker volume rm -f $(docker volume ls -q --filter \"label=trex\")"

testRunSuccess :: Runner.Service -> IO ()
testRunSuccess runner = do
    build <- runner.prepareBuild $ makePipeline [
                    makeStep "First step" "ubuntu" ["date"],
                    makeStep "Second step" "ubuntu" ["uname -r"]
                ]

    result <- runner.runBuild emptyHooks build

    result.state `shouldBe` BuildFinished BuildSucceeded
    Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]

testRunFailure :: Runner.Service -> IO ()
testRunFailure runner = do
    build <- runner.prepareBuild $ makePipeline [
                    makeStep "Should fail" "ubuntu" ["exit 1"]
                ]

    result <- runner.runBuild emptyHooks build

    result.state `shouldBe` BuildFinished BuildFailed
    Map.elems result.completedSteps `shouldBe` [StepFailed (Docker.ContainerExitCode 1)]

testSharedWorkspace :: Docker.Service -> Runner.Service -> IO ()
testSharedWorkspace docker runner = do
    build <- runner.prepareBuild $ makePipeline [
                    makeStep "Create file" "ubuntu" ["echo testing > test.txt"],
                    makeStep "Read file" "ubuntu" ["cat test.txt"]
                ]
    result <- runner.runBuild emptyHooks build
    result.state `shouldBe` BuildFinished BuildSucceeded
    Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]

emptyHooks :: Runner.Hooks
emptyHooks = Runner.Hooks {
        logCollected = \_ -> pure ()
    }

testLogCollection :: Runner.Service -> IO ()
testLogCollection runner = do
    expected <- newMVar $ Set.fromList ["hello", "world", "Linux"]

    let onLog :: Log -> IO ()
        onLog log = do
            remaining <- readMVar expected
            forM_ remaining $ \word -> do
                case ByteString.breakSubstring word log.output of
                    (_, "") -> pure ()
                    _       -> modifyMVar_ expected (pure . Set.delete word)

    let hooks = Runner.Hooks { logCollected = onLog }

    build <- runner.prepareBuild $ makePipeline [
                    makeStep "Long step" "ubuntu" ["echo hello", "sleep 2", "echo world"],
                    makeStep "Echo Linux" "ubuntu" ["uname -s"]
                ]

    result <- runner.runBuild hooks build
    result.state `shouldBe` BuildFinished BuildSucceeded
    Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]

    readMVar expected >>= \logs -> logs `shouldBe` Set.empty

testImagePull :: Runner.Service -> IO ()
testImagePull runner = do
    Process.readProcessStdout "docker rmi -f busybox"

    build <- runner.prepareBuild $ makePipeline [
                    makeStep "First step" "busybox" ["date"]
                ]

    result <- runner.runBuild emptyHooks build

    result.state `shouldBe` BuildFinished BuildSucceeded
    Map.elems result.completedSteps `shouldBe` [StepSucceeded]

testYamlDecoding :: Runner.Service -> IO ()
testYamlDecoding runner = do
    pipeline <- Yaml.decodeFileThrow "test/pipeline.yaml"
    build <- runner.prepareBuild pipeline
    result <- runner.runBuild emptyHooks build
    result.state `shouldBe` BuildFinished BuildSucceeded

testServerAndAgent :: Runner.Service -> IO ()
testServerAndAgent runner = do
    handler <- JobHandler.Memory.createService

    serverThread <- Async.async do
        Server.run (Server.Config 4000) handler

    Async.link serverThread

    agentThread <- Async.async do
        Agent.run (Agent.Config "http://localhost:4001") runner

    Async.link agentThread

    let pipeline = makePipeline [makeStep "agent-test" "busybox" ["echo Agent Yello is live"]]

    number <- handler.queueJob pipeline
    checkBuild handler number

    Async.cancel serverThread
    Async.cancel agentThread

    pure ()

checkBuild :: JobHandler.Service -> BuildNumber -> IO ()
checkBuild handler number = loop
    where loop = do
            Just job <- handler.findJob number
            case job.state of
                JobHandler.JobScheduled build -> do
                    case build.state of
                        BuildFinished s -> s `shouldBe` BuildSucceeded
                        _               -> loop
                _ -> loop

main :: IO ()
main = hspec do
    docker <- runIO Docker.createService
    runner <- runIO $ Runner.createService docker
    afterAll_ cleanUpDocker $ describe "T-Rex CI" do
        it "should decode yaml files" do
            testYamlDecoding runner

        it "should pull images" do
            testImagePull runner

        it "should run a build (success)" do
            testRunSuccess runner

        it "should run a build (failure)" do
            testRunFailure runner

        it "should share workspace between steps" do
            testSharedWorkspace docker runner

        it "should collect logs" do
            testLogCollection runner

        it "should run server and agent" do
            testServerAndAgent runner
