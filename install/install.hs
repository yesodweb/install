{-# LANGUAGE OverloadedStrings #-}
import Shelly
import System.Environment (getArgs)
import Data.Text.Lazy (Text)
import Control.Monad (forM_)
import Data.Monoid ((<>))

-- log the string and apply it to the action
echoAp :: Text -> (Text -> ShIO a) -> ShIO a
echoAp str action = echo str >> action str

main :: IO ()
main = do
  args <- getArgs
  shelly $
    verbosely $
      if args == ["--install"] then installYesod
        else do
          cloneYesod
          echo ""
          echo ""
          echo "please update your global cabal infrastructure:"
          echo "    cabal update"
          echo "    cabal install Cabal cabal-install cabal-dev cabal-src virthualenv"
          echo ""
          echo "You can optionally (recommended) use virthualenv with:"
          echo "    virthualenv --name=yesod"
          echo "    ./virthualenv/bin/activate"
          echo ""
          echo "Then run:"
          echo "    install/dist/build/yesodweb-install/yesodweb-install --install"

installYesod :: ShIO ()
installYesod =
  forM_ yesodRepos $ \repo -> do
    echo repo
    chdir repo $ run "scripts/install" []

{-cabal_install :: [String] -> ShIO String-}
{-cabal_install = cabal "install"-}

{-cabal :: String ->  [String] -> ShIO String-}
{-cabal = subCommand "cabal"-}

git :: Text ->  [Text] -> ShIO Text
git = subCommand "git"

yesodwebClone :: Text -> ShIO Text
yesodwebClone repo = git "clone" ["http://github.com/yesodweb/" <> repo]

subCommand :: Text ->  Text ->  [Text] -> ShIO Text
subCommand com subCom args = run com (subCom:args)

yesodRepos :: [Text]
yesodRepos = ["hamlet","persistent","wai","yesod"]

cloneYesod :: ShIO ()
cloneYesod =
  forM_ yesodRepos cloneYesodRepo
  where
    cloneYesodRepo :: Text -> ShIO ()
    cloneYesodRepo repo = do
        b <-  echoAp repo test_d
        _<- if b
              then chdir repo $ git "status" [] -- git "pull" ["origin","master"]
              else do
                _<-  yesodwebClone repo
                chdir repo $ git "submodule" ["update","--init"]
        return ()

{-apEcho :: (String -> ShIO a) -> String -> ShIO a-}
{-apEcho action str = echo str >> (action str)-}

{-command1 :: String ->  [String] ->  String -> ShIO String-}
{-command1 com args one_more_arg = run com (args ++ [one_more_arg])-}


{-chdirP :: FilePath -> ShIO a -> ShIO a-}
{-chdirP dir action = chdir1 dir (\d -> liftIO (print d) >> action)-}
