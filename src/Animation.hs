module Animation where

import Codec.Picture
import Codec.Picture.Png
import Codec.Picture.Gif
import Control.Monad
import qualified Data.ByteString as B
import Graphics.EasyPlot
import System.Directory


animatePlots :: Plot a => FilePath -> GifDelay -> [a] -> IO ()
animatePlots path delay plots = do
    let names = take (length plots) $ fmap (\i -> path ++ "-" ++ show i) [0..]

    forM_ (plots `zip` names) $ \(p, n) -> plot (PNG n) p

    imgs <- forM names $ \n -> do
        file <- B.readFile n
        let Right png = decodePng file
            ImageRGB8 img = png
        return img

    let Right writeGif = writeGifAnimation path delay LoopingForever imgs
    writeGif

    forM_ names removeFile

