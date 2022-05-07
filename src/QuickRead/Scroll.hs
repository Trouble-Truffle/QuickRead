{-# LANGUAGE LambdaCase #-}
-- | Controls scrolling changes in the file text and the filequeue
module QuickRead.Scroll where

import           QuickRead.Scroll.File         as QSF
import           QuickRead.Scroll.Text         as QST
import           QuickRead.Types               as QT

import           Brick
import           Control.Lens

import           Control.Monad.IO.Class


textStep :: Reader -> QT.Direction -> EventM Name (Next Reader)
textStep reader' lr = if reader' ^. paused && not (reader' ^. manualAction)
  then continue reader'
  else case spin reader' lr of
    (reader, QST.Normal) -> continue $ reader & manualAction .~ False
    (reader, QST.Empty ) -> error "TODO: handle empty files"
    (reader, QST.SOF   ) -> error "TODO: handle file ends"
    (reader, QST.EOF   ) -> error "TODO: handle file starts"

fileQueueStep :: Reader -> QT.Direction -> EventM Name (Next Reader)
fileQueueStep reader' lr = liftIO ( advanceQueue reader' lr) >>= \case
  (reader, QSF.Normal       ) -> continue reader 
  (reader, QSF.Empty        ) -> error "TODO: Handle empty queue"
  (reader, QSF.SOQ          ) -> error "TODO: Handle start of queue"
  (reader, QSF.EOQ          ) -> error "TODO: Handle end of queue"
  (reader, QSF.ReadError err) -> error "TODO: Handle end of queue"
