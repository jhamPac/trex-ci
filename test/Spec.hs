import           Core
import           RIO

makeStep :: Text -> Text -> [Text] -> Step
makeStep name image commands = Step {
    name = StepName name,
    image = Image image,
    commands = commands
}

makePipeline :: [Step] -> Pipeline
makePipeline steps = Pipeline { steps = steps }

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
