import Control.Lens
import Control.Monad.State
import Control.Comonad
import Graphics.UI.FreeGame
import Data.Karakuri

main = runGame def $ flip evalStateT [draggablePoint (V2 200 200)
    , draggablePoint (V2 280 200)
    , draggablePoint (V2 280 280)
    , draggablePoint (V2 200 280)]
    $ foreverTick $ do
    get >>= mapM (lift . step) >>= put
    ps <- use (to (map extract))
    colored red $ polygonOutline ps

draggablePoint :: (Monad m, Figure2D m, Mouse m) => Vec2 -> Karakuri m Vec2
draggablePoint _p = fst <$> stateful go (_p, Nothing)  where
    rect = BoundingBox (-6) (-6) 6 6
    go = do
        (q, s) <- get
        p <- mousePosition
        colored (blue & _Alpha .~ 0.5) $ translate q $ polygon $ rect ^.. _Corners
        case s of
            Nothing -> whenM mouseButtonL $ when (inBoundingBox (p - q) rect) $ put (q, Just (q - p))
            Just r -> do
                unlessM mouseButtonL $ _2 .= Nothing
                _1 .= p + r
