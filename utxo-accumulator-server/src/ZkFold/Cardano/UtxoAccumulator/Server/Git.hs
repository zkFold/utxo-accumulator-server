module ZkFold.Cardano.UtxoAccumulator.Server.Git (
  createConfigUpdatePR,
  isValidGitRepo,
) where

import Control.Exception (SomeException, catch)
import Control.Monad (when)
import Data.Char (isAlphaNum)
import Data.List (isInfixOf)
import Data.Time.Clock (getCurrentTime)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

-- | Check if we're in a valid git repository with the correct origin
isValidGitRepo :: IO Bool
isValidGitRepo = do
  result <- catch checkGitRepo (\(_ :: SomeException) -> return False)
  return result
 where
  checkGitRepo = do
    -- Check if we're in a git repository
    (exitCode, _, _) <- readProcessWithExitCode "git" ["rev-parse", "--git-dir"] ""
    case exitCode of
      ExitFailure _ -> return False
      ExitSuccess -> do
        -- Check if the origin is the correct repository
        (exitCode', stdout, _) <- readProcessWithExitCode "git" ["remote", "get-url", "origin"] ""
        case exitCode' of
          ExitFailure _ -> return False
          ExitSuccess -> return $ "github.com/zkFold/utxo-accumulator-server" `isInfixOf` stdout

-- | Create a PR for config file updates
createConfigUpdatePR :: FilePath -> (String -> IO ()) -> IO ()
createConfigUpdatePR configPath logInfoS = do
  isValid <- isValidGitRepo
  if isValid
    then do
      logInfoS "Creating PR for config file update..."
      result <-
        catch
          createPR
          ( \(e :: SomeException) -> do
              logInfoS $ "Failed to create PR: " ++ show e
              return False
          )
      when result $ logInfoS "Successfully created PR for config file update"
    else logInfoS "Not in a valid git repository or incorrect origin, skipping PR creation"
 where
  createPR = do
    -- Get current timestamp for branch naming
    timestamp <- show <$> getCurrentTime
    let branchName = "config-update-" ++ filter isAlphaNum timestamp

    -- Create and switch to new branch
    (exitCode1, _, _) <- readProcessWithExitCode "git" ["checkout", "-b", branchName] ""
    case exitCode1 of
      ExitFailure _ -> return False
      ExitSuccess -> do
        -- Add the config file
        (exitCode2, _, _) <- readProcessWithExitCode "git" ["add", configPath] ""
        case exitCode2 of
          ExitFailure _ -> return False
          ExitSuccess -> do
            -- Commit the changes
            let commitMsg = "Update config with thread token reference after accumulator initialization"
            (exitCode3, _, _) <- readProcessWithExitCode "git" ["commit", "-m", commitMsg] ""
            case exitCode3 of
              ExitFailure _ -> return False
              ExitSuccess -> do
                -- Push the branch
                (exitCode4, _, _) <- readProcessWithExitCode "git" ["push", "-u", "origin", branchName] ""
                case exitCode4 of
                  ExitFailure _ -> return False
                  ExitSuccess -> do
                    -- Create PR using GitHub CLI if available
                    createGitHubPR branchName

  createGitHubPR branchName = do
    (exitCode, _, _) <- readProcessWithExitCode "gh" ["--version"] ""
    case exitCode of
      ExitFailure _ -> do
        -- GitHub CLI not available, just log the branch info
        logInfoS $ "Branch '" ++ branchName ++ "' pushed. Please create PR manually at: https://github.com/zkFold/utxo-accumulator-server/compare/" ++ branchName
        return True
      ExitSuccess -> do
        -- Create PR using GitHub CLI
        let prTitle = "Update config with thread token reference"
        let prBody = "Automated update of configuration file with thread token reference after UTxO accumulator initialization.\n\nThis PR was automatically created by the server during accumulator initialization."
        (exitCode', _, _) <-
          readProcessWithExitCode
            "gh"
            ["pr", "create", "--title", prTitle, "--body", prBody, "--head", branchName]
            ""
        case exitCode' of
          ExitFailure _ -> do
            logInfoS $ "Failed to create PR via GitHub CLI. Branch '" ++ branchName ++ "' pushed. Please create PR manually."
            return True
          ExitSuccess -> return True
