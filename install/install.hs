import Shellish
import System.Environment (getArgs)

-- bind some arguments to run
-- command :: String -> [String] -> [String] -> ShIO String
-- command com args more_args = run com (args ++ more_args)
{-apEcho :: (String -> ShIO a) -> String -> ShIO a-}
{-apEcho action str = echo str >> (action str)-}

{-command1 :: String ->  [String] ->  String -> ShIO String-}
{-command1 com args one_more_arg = run com (args ++ [one_more_arg])-}

-- log the string and apply it to the action
echoAp :: String -> (String -> ShIO a) -> ShIO a
echoAp str action = echo str >> (action str)

chdir :: FilePath -> ShIO a -> ShIO a
chdir dir action = chdir1 dir (\_ -> action)

{-chdirP :: FilePath -> ShIO a -> ShIO a-}
{-chdirP dir action = chdir1 dir (\d -> liftIO (print d) >> action)-}

chdir1 :: FilePath -> (FilePath -> ShIO a) -> ShIO a
chdir1 dir action = do
  d <- pwd
  cd dir
  r <- action dir
  cd d
  return r

main :: IO ()
main = do
  args <- getArgs
  shellish $ do
    verbosely $ do
      if args == ["--install"] then install_yesod
        else do
          clone_yesod
          echo "please update your global cabal infrastructure:"
          echo "    cabal update"
          echo "    cabal install Cabal cabal-install cabal-dev cabal-src virthualenv"
          echo ""
          echo "You can optionally (recommended) use virthualenv with:"
          echo "    virthualenv --name=yesod"
          echo "    ./virthualenv/bin/activate"
          echo ""
          echo "Then run:"
          echo "    install/dist/build/install/yesodweb-install --install"

install_yesod :: ShIO ()
install_yesod = do
  flip mapM_ yesodRepos $ \repo -> do
    chdir repo $ run "script/install" []

{-cabal_install :: [String] -> ShIO String-}
{-cabal_install = cabal "install"-}

{-cabal :: String ->  [String] -> ShIO String-}
{-cabal = subCommand "cabal"-}

git :: String ->  [String] -> ShIO String
git = subCommand "git"

yesodweb_clone :: String -> ShIO String
yesodweb_clone repo = git "clone" ["http://github.com/yesodweb/" ++ repo]

subCommand :: String ->  String ->  [String] -> ShIO String
subCommand com subCom args = run com ([subCom] ++ args)

clone_yesod_repo :: String -> ShIO ()
clone_yesod_repo repo = do
    b <-  echoAp repo test_d
    _<- if b
          then chdir repo $ git "status" [] -- git "pull" ["origin","master"]
          else do
            _<-  yesodweb_clone repo
            chdir repo $ git "submodule" ["update","--init"]
    return ()

yesodRepos :: [String]
yesodRepos = ["hamlet","persistent","wai","yesod"]

clone_yesod :: ShIO ()
clone_yesod =
  flip mapM_ yesodRepos clone_yesod_repo
