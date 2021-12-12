module JobHandler.Memory where

import qualified Agent
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

                findJob = \number -> STM.atomically do
                    s <- STM.readTVar state
                    pure $ findJob' number s,

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

findJob' :: BuildNumber -> State -> Maybe JobHandler.Job
findJob' number state = Map.lookup number state.jobs

dispatchCmd' :: State -> (Maybe Agent.Cmd, State)
dispatchCmd' state =
    case List.find queued $ Map.toList state.jobs of
        Just (number, job) ->
            let updatedJob = job { state = JobHandler.JobAssigned }
                updatedState = Map.insert number updatedJob state.jobs
                cmd = Just $ Agent.StartBuild number job.pipeline
            in (cmd, state { jobs = updatedState })

        _ -> (Nothing, state)
    where
        queued (_, job) = job.state == JobHandler.JobQueued
