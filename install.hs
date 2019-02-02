#!/usr/bin/env runhaskell
{-# Language GADTSyntax, LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}

import Data.Foldable (traverse_)
import Data.Functor (void)
import GHC.IO.Handle (Handle)
import System.Process (createProcess, proc, ProcessHandle)
import System.Environment
import System.FilePath

symlink
  :: FilePath
  -> FilePath
  -> Link
  -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
symlink dotfiles config s
  = createProcess (proc "ln" ["-s", source, target])
  where
  (source, target) = spec s
  spec :: Link -> (FilePath, FilePath)
  spec = \case
    Config p         -> (dotfiles </> p, config)
    Absolute trg src -> (trg, src)

getHome :: IO FilePath
getHome = getEnv "HOME"

-- TODO Make configurable/more portable.
getDotfiles :: FilePath -> FilePath
getDotfiles home = home </> "git/fredefox/dotfiles"

getConfig :: FilePath -> FilePath
getConfig home = home </> ".config"


data Link where
  Config :: FilePath -> Link
  Absolute :: FilePath -> FilePath -> Link

getLinks :: FilePath -> [] Link
getLinks home = std <> absolute
  where
  std = Config <$>
    [ "git"
    , "i3"
    , "urxvt"
    , "xinit"
    , "Xresources"
    , "zsh"
    ]
  absolute =
    [ ".emacs.d"    |> ".config/emacs"
    , ".gitconfig"  |> ".config/git/config"
    , ".urxvt"      |> ".config/urxvt"
    , ".xinitrc"    |> ".config/xinit/xinitrc"
    , ".xsession"   |> ".xinitrc"
    , ".xsessionrc" |> ".config/xinit/xinitrc"
    , ".zshrc"      |> ".config/zsh/init"
    ]
  src |> trg = Absolute trg (home </> src)

main :: IO ()
main = do
  home <- getHome
  let
    config   = getConfig home
    dotfiles = getDotfiles home
    links    = getLinks home
  void $ createProcess (proc "mkdir" [config])
  traverse_ (symlink dotfiles config) links

