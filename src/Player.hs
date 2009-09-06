module Player (play) where
import System.Process

play :: IO ()
play = do x <-runInteractiveCommand "mpg321 song.mp3"
          return ()
