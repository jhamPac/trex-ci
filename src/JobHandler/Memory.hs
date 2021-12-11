module JobHandler.Memory where

import           Core
import           RIO
import qualified RIO.Map    as Map

import qualified JobHandler

createService :: IO JobHandler.Service
createService = do
    pure JobHandler.Service {
                queueJob = \_ -> undefined,
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

