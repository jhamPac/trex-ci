import           Core
import qualified Docker
import           RIO
import qualified RIO.Map              as Map
import           RIO.NonEmpty.Partial as NE
import           Runner
import           System.Process.Typed as Process
import           Test.Hspec

makeStep :: Text -> Text -> [Text] -> Step
makeStep name image commands = Step {
        name = StepName name,
        image = Docker.Image image,
        commands = NE.fromList commands
    }

makePipeline :: [Step] -> Pipeline
makePipeline steps = Pipeline {
        steps = NE.fromList steps
    }

cleanUpDocker :: IO ()
cleanUpDocker = void do
    Process.readProcessStdout "docker rm -f $(docker ps -aq --filter \"label=trex\")"

testRunSuccess :: Runner.Service -> IO ()
testRunSuccess runner = do
    build <- runner.prepareBuild $ makePipeline [
                    makeStep "First step" "ubuntu" ["date"],
                    makeStep "Second step" "ubuntu" ["uname -r"]
                ]

    result <- runner.runBuild build

    result.state `shouldBe` BuildFinished BuildSucceeded
    Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]

testRunFailure :: Runner.Service -> IO ()
testRunFailure runner = do
    build <- runner.prepareBuild $ makePipeline [
                    makeStep "Should fail" "ubuntu" ["exit 1"]
                ]

    result <- runner.runBuild build

    result.state `shouldBe` BuildFinished BuildFailed
    Map.elems result.completedSteps `shouldBe` [StepFailed (Docker.ContainerExitCode 1)]

testSharedWorkspace :: Docker.Service -> Runner.Service -> IO ()
testSharedWorkspace docker runner = do
    build <- runner.prepareBuild $ makePipeline [
                    makeStep "Create file" "ubuntu" ["echo testing > test.txt"],
                    makeStep "Read file" "ubuntu" ["cat test.txt"]
                ]
    result <- runner.runBuild build
    result.state `shouldBe` BuildFinished BuildSucceeded
    Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]

main :: IO ()
main = hspec do
    docker <- runIO Docker.createService
    runner <- runIO $ Runner.createService docker
    afterAll_ cleanUpDocker $ describe "T-Rex CI" do
        it "should run a build (success)" do
            testRunSuccess runner

        it "should run a build (failure)" do
            testRunFailure runner

        it "should share workspace between steps (share)" do
            testSharedWorkspace docker runner
