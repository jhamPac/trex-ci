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

runBuild :: Docker.Service -> Build -> IO Build
runBuild ds build = do
    newBuild <- Core.progress ds build
    case newBuild.state of
        BuildFinished _ ->
            pure newBuild
        _ -> do
            threadDelay (1 * 1000 * 1000)
            runBuild ds newBuild

testRunSuccess :: Runner.Service -> IO ()
testRunSuccess runner = do
    build <- runner.prepareBuild $ makePipeline [
                    makeStep "First step" "ubuntu" ["date"],
                    makeStep "Second step" "ubuntu" ["uname -r"]
                ]

    result <- runner.runBuild build

    result.state `shouldBe` BuildFinished BuildSucceeded
    Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]

cleanUpDocker :: IO ()
cleanUpDocker = void do
    Process.readProcessStdout "docker rm -f $(docker ps -aq --filter \"label=trex\")"

main :: IO ()
main = hspec do
    docker <- runIO Docker.createService
    runner <- runIO $ Runner.createService docker
    beforeAll cleanUpDocker $ describe "T-Rex CI" do
        it "should run a build (success)" do
            testRunSuccess runner
