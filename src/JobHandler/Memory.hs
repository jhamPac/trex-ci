module JobHandler.Memory where

import qualified Control.Concurrent.STM as STM
import           Core
import qualified JobHandler
import           RIO
import qualified RIO.Map                as Map

createService :: IO JobHandler.Service
createService = do
    state <- STM.newTVarIO State {
                jobs = mempty,
                nextBuild = 1
            }

    pure JobHandler.Service {
                queueJob = \pipeline -> STM.atomically do
                    STM.stateTVar state $ queueJob' pipeline,
                findJob = \_ -> undefined,
                dispatchCmd = pure undefined,
                processMsg = \_ -> undefined
            }

data State = State {
        jobs      :: Map BuildNumber JobHandler.Job ,
        nextBuild :: Int
    } deriving (Eq, Show)

queueJob' :: Pipeline -> State -> (BuildNumber, State)
queueJob' pipeline state = (number, updatedState)
    where
        number = BuildNumber state.nextBuild
        job = JobHandler.Job {
                pipeline = pipeline,
                state = JobHandler.JobQueued
            }
        updatedState = state {
                jobs = Map.insert number job state.jobs,
                nextBuild = state.nextBuild + 1
            }

