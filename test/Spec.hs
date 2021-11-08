import           Core
import qualified Docker
import           RIO
import           RIO.NonEmpty.Partial as NE
import           Test.Hspec

makeStep :: Text -> Text -> [Text] -> Step
makeStep name image commands = Step {
    name = StepName name,
    image = Docker.Image image,
    commands = NE.fromList commands
    }

makePipeline :: [Step] -> Pipeline
makePipeline steps = Pipeline { steps = NE.fromList steps }

testPipeline :: Pipeline
testPipeline = makePipeline
    [
        makeStep "Step 1" "ubuntu" ["date"],
        makeStep "Step 2" "ubuntu" ["uname -r"]
    ]

testBuild :: Build
testBuild = Build {
    pipeline = testPipeline,
    state = BuildReady,
    completedSteps = mempty
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

main :: IO ()
main = hspec do
    describe "T-Rex CI" do
        it "should run a build (success)" do
            1 `shouldBe` 1
