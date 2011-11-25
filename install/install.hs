import Shellish

-- bind some arguments to run
-- command :: String -> [String] -> [String] -> ShIO String
-- command com args more_args = run com (args ++ more_args)

command1 :: String ->  [String] ->  String -> ShIO String
command1 com args one_more_arg = run com (args ++ [one_more_arg])

-- log the string and apply it to the action
echoAp :: String -> (String -> ShIO a) -> ShIO a
echoAp str action = echo str >> (action str)

apEcho :: (String -> ShIO a) -> String -> ShIO a
apEcho action str = echo str >> (action str)

chdir :: FilePath -> ShIO a -> ShIO a
chdir dir action = chdir1 dir (\_ -> action)

chdirP :: FilePath -> ShIO a -> ShIO a
chdirP dir action = chdir1 dir (\d -> liftIO (print d) >> action)

chdir1 :: FilePath -> (FilePath -> ShIO a) -> ShIO a
chdir1 dir action = do
  d <- pwd
  cd dir
  r <- action dir
  cd d
  return r

addSource :: String -> ShIO String
addSource = command1 "cabal-dev" ["add-source"]

main :: IO ()
main = do
  shellish $ do
    verbosely $ do
      clone_yesod
      install_yesod

install_yesod :: ShIO ()
install_yesod = do
  chdirP "../build" $ do
    _<- cabal_install "cabal-dev"
    -- clone_yesod_repo "cabal-dev" -- need add-source-file branch
    -- cabal_dev "add-source-file" "build/sources.txt"
    localDeps <- liftIO $ fmap lines $ readFile "../build/sources.txt"
    mapM_ (apEcho addSource) localDeps 
    echo ""
    echo "Doing a sandboxed build. This can take a while"
    echo "It isn't strictly necessary, but helps everyone know that things are in working order"
    echo "You can start hacking on and using the Yesod repos now."
    readme
    _<- cabal_dev "install" []
    echo ""
    echo "Yesod successfully built from HEAD on your machine."
    readme
  where
    readme = echo "please see the yesod README for instructions on hacking and using with your application."

cabal_install :: String -> ShIO String
cabal_install = command1 "cabal" ["install"]

git :: String ->  [String] -> ShIO String
git = subCommand "git"

cabal_dev :: String ->  [String] -> ShIO String
cabal_dev = subCommand "cabal-dev"

subCommand :: String ->  String ->  [String] -> ShIO String
subCommand com subCom args = run com ([subCom] ++ args)

clone_yesod_repo :: String -> ShIO ()
clone_yesod_repo repo = do
    b <-  echoAp repo test_d
    _<- if b
          then chdir repo $ git "pull" ["origin","master"]
          else do
            _<-  git "clone" ["http://github.com/yesodweb/" ++ repo]
            chdir repo $ git "submodule" ["update","--init"]
    return ()

clone_yesod :: ShIO ()
clone_yesod =
  flip mapM_ ["hamlet","persistent","wai","yesod"] clone_yesod_repo
