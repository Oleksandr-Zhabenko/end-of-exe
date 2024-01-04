-- |
-- Module      :  EndOfExe2
-- Copyright   :  (c) Oleksandr Zhabenko 2019-2024
-- License     :  MIT
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- A small library to deal with executable endings. Uses a Maybe data representation inside an IO monad.
--
-- Poorly tested for Windows (just Windows 7). Well, it can be extended so that it can basically support also other versions and OSes.
--
-- It is a fork of now deprecated library [mmsyn3](https://hackage.haskell.org/package/mmsyn3).

{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK -show-extensions #-}

module EndOfExe2 where

import GHC.Base
import GHC.List
import qualified System.Directory as D (findExecutable)
import Data.Maybe (isJust,isNothing)
import System.IO.Unsafe (unsafePerformIO,unsafeDupablePerformIO)

-- | Can be used instead of 'System.Info.os' to check whether the executable ends in \".exe\". The function returns 'IO' 'Nothing' if there is neither 
-- @ys@ nor @(ys ++ ".exe")@ names for executables in the variable @PATH@. It can also search in other locations and its behaviour is OS dependent. For more information, please, refer to the link: https://hackage.haskell.org/package/directory-1.3.4.0/docs/System-Directory.html#v:findExecutable
maybeEndOfExecutable :: String -> IO (Maybe String)
maybeEndOfExecutable ys = do
  xs <- D.findExecutable ys
  if isJust xs 
    then return $ fmap (ys ++) (Just "")
    else do
      zs <- D.findExecutable (ys ++ ".exe")
      if isJust zs
        then return $ fmap (ys ++) (Just ".exe")
        else error ("EndOfExe2.maybeEndOfExecutable: Please, install the executable " ++ ys ++ " into the directory in the PATH variable!")

-- | The function 'endOfExe' returns 'IO' \"\" if no executable found by 'D.findExecutable'. Otherwise, it returns its path.
endOfExe :: String -> IO String
endOfExe ys = do
  xs <- D.findExecutable ys
  if isJust xs 
    then return . fromJust $ ys
    else do
      zs <- D.findExecutable (ys ++ ".exe")
      if isJust zs
        then return $ (ys ++ ".exe")
        else error ("EndOfExe2.endOfExe: Please, install the executable " ++ ys ++ " into the directory in the PATH variable!")
                                  
-- | Gets the proper name of the executable in the system (it must be seen in the directories in the @PATH@ variable). 
-- You can use 'showE' \"nameOfExecutable\" to get 'Just' \"nameOfExecutable\"@ if it is present on the system. Further you can adopt it to be used 
-- inside the 'System.Process.callCommand' as the name of the executable
showE :: String -> Maybe String
showE xs 
  | null xs = error "EndOfExe2.showE: No executable specified!"
  | otherwise = unsafePerformIO . endOfExecutable $ xs
{-# INLINE showE #-}

-- | Similar to 'showE' but uses 'unsafeDupablePerformIO', which is more efficient, but for the multiprocessor can lead to executing the IO action multiple times.
showEDup :: String -> Maybe String
showEDup xs 
  | null xs = error "EndOfExe2.showEDup: No executable specified!"
  | otherwise = unsafeDupablePerformIO . endOfExecutable $ xs
{-# INLINE showEDup #-}

