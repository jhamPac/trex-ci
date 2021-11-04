import           Core
import           RIO
import           RIO.NonEmpty.Partial as NE

makeStep :: Text -> Text -> [Text] -> Step
makeStep name image commands = Step {
    name = StepName name,
    image = Image image,
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
testBuild = Build { pipeline = testPipeline, state = BuildReady}

main :: IO ()
main = pure ()
