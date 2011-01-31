-- Copyright 2011 Chris Forno

-- This file is part of Vocabulink.

-- Vocabulink is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option)
-- any later version.

-- Vocabulink is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
-- for more details.

-- You should have received a copy of the GNU Affero General Public License
-- along with Vocabulink. If not, see <http://www.gnu.org/licenses/>.

module Vocabulink.Link.Pronunciation (pronounceable, addPronunciation, deletePronunciation) where

import Vocabulink.App
import Vocabulink.CGI
import Vocabulink.Link
import Vocabulink.Member
import Vocabulink.Utils

import Prelude hiding (writeFile)

pronounceable :: Link -> App Bool
pronounceable l = isJust <$> $(queryTuple'
  "SELECT format FROM link_pronunciation \
  \WHERE link_no = {linkNumber l}")

-- TODO: Would this be safe from race conditions with a transaction?
addPronunciation :: Integer -> App CGIResult
addPronunciation linkNo = do
  link <- getLink linkNo
  case link of
    Nothing -> outputNotFound
    Just l  -> do
      editable <- canEdit l
      if not editable
        then outputUnauthorized
        else do
          -- Make sure that a pronunciation doesn't already exist.
          exists <- pronounceable l
          if exists
            then error "Pronunciation already exists."
            else do
              formFile <- getInputFilename "qqfile"
              (filename, content) <- case formFile of
                                       -- Old browsers will send as a file input.
                                       Just f   -> do
                                         content' <- getRequiredInputFPS "qqfile"
                                         return (f, content')
                                       -- New browsers will send as the POST body.
                                       Nothing  -> do
                                         f <- getRequiredInput "qqfile"
                                         content' <- getBodyFPS
                                         return (f, content')
              let format = map toLower $ safeTail $ takeExtension filename
              if format `notElem` allowedExtensions
                then error $ "Unsupported file format: " ++ format
                else do
                  dir <- (</> "upload" </> "audio" </> "pronunciation") <$> asks appDir
                  let filepath = dir </> (show linkNo) <.> format
                  liftIO $ writeFile filepath content
                  liftIO $ prepAudio format filepath
                  $(execute' "INSERT INTO link_pronunciation (link_no, format) \
                                                     \VALUES ({linkNo}, {format})")
                  outputJSON [("success", True)]
 where allowedExtensions = ["flac", "wav", "ogg", "mp3"]

deletePronunciation :: Integer -> App CGIResult
deletePronunciation linkNo = do
  $(execute' "DELETE FROM link_pronunciation WHERE link_no = {linkNo}")
  dir <- (</> "upload" </> "audio" </> "pronunciation") <$> asks appDir
  liftIO $ unsafeSystem $ "rm " ++ dir ++ "/" ++ show linkNo ++ ".*"
  outputNothing

prepAudio :: String -> FilePath -> IO ()
prepAudio "flac" f = do
  unsafeSystem $ "flac -d " ++ f ++ " &> /dev/null"
  prepAudio "wav" $ replaceExtension f ".wav"
  unsafeSystem $ "rm " ++ replaceExtension f ".wav"
prepAudio "wav"  f = do
  unsafeSystem $ "oggenc " ++ f ++ " &> /dev/null"
  unsafeSystem $ "lame " ++ f ++ " " ++ (replaceExtension f ".mp3") ++ " &> /dev/null"
prepAudio "ogg"  f = do
  unsafeSystem $ "oggdec " ++ f ++ " &> /dev/null"
  unsafeSystem $ "lame " ++ replaceExtension f ".wav" ++ " " ++ replaceExtension f ".mp3" ++ " &> /dev/null"
  unsafeSystem $ "rm " ++ replaceExtension f ".wav"
prepAudio "mp3"  f = do
  unsafeSystem $ "lame --decode " ++ f ++ " " ++ replaceExtension f ".wav" ++ " &> /dev/null"
  unsafeSystem $ "oggenc " ++ replaceExtension f ".wav" ++ " &> /dev/null"
  unsafeSystem $ "rm " ++ replaceExtension f ".wav"
prepAudio _      _ = error "Unsupported file format"
