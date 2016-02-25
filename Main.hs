{-# LANGUAGE Arrows #-}
module Main where

import FRP.Yampa
import System.IO
import Graphics.UI.GLUT
import Foreign.C.Types
import Data.IORef


data GameElement = GameElement {
						x :: GLfloat,
						y :: GLfloat,
						vx:: GLfloat,
						vy:: GLfloat,
						w :: GLfloat,
						h :: GLfloat,
						col :: Color3 GLfloat
					} deriving (Show)

type GameScene = [ GameElement ]

data Game = Game {
				mario :: GameElement,
				world :: GameScene
			} deriving (Show)

main :: IO ()
main = do {
	let {myGame = Game {mario=GameElement{x=20, y=20, vx=0, vy=0, w=10, h=20, col=Color3 1 0 (0::GLfloat)}, 
		 				world = [GameElement {x=200, y=400, vx=0, vy=0, w=300, h=200, col=Color3 0 0 (1::GLfloat)} , GameElement {x=100, y=50, vx=0, vy=0, w=100, h=50, col=Color3 0 0 (1::GLfloat)}]
		 				}
		};
	(_progName, _args) <- Graphics.UI.GLUT.getArgsAndInitialize;
	_window <- Graphics.UI.GLUT.createWindow "Hello World";
	Graphics.UI.GLUT.displayCallback $= (display myGame); 
	reshapeCallback $= Just reshape;
	mainLoop;
	}

reshape :: ReshapeCallback
reshape size = do
	viewport $= (Position 0 0, size)
	postRedisplay Nothing

color3f r g b = color $ Color3 r g (b :: GLfloat)
vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)


my_translate :: GameElement -> GameElement
my_translate g = g {x=(x g)-500, y=(y g)-500}
-- my_translate g = g {x=x-(500 :: GLfloat), y=y-(500 :: GLfloat)}
-- my_translate g = GameElement {x=(x g)-500, y=(y g)-500, vx = (vx g), vy = (vy g), w = (w g), h = (h g)}

my_normalize :: GameElement -> GameElement
-- my_normalize g = g {x = (x :: CFloat )/1000, y=y/1000, w=w/1000, h=h/1000}
my_normalize g = g {x = (x g)/1000, y=(y g)/1000, w=(w g)/1000, h=(h g)/1000}
-- my_normalize g = GameElement {x=(x g)/1000, y=(y g)/1000, vx = (vx g), vy = (vy g), w = (w g)/1000, h = (h g)/1000}


renderGameElement :: GameElement -> IO ()
renderGameElement myge = do {
								let {g = my_normalize $ my_translate myge};
								-- let g = GameElement {x=20, y=40, vx=0, vy=0, w=10, h=10};
								-- let a = 3;
								color (col g);
								vertex3f (x g - (w g) / 2 ) (y g - (h g) / 2 ) 0;
								vertex3f (x g + (w g) / 2 ) (y g - (h g) / 2 ) 0;
								vertex3f (x g + (w g) / 2 ) (y g + (h g) / 2 ) 0;
								vertex3f (x g - (w g) / 2 ) (y g + (h g) / 2 ) 0;
							}
 
display :: Game -> DisplayCallback
display myGame = do
	clear [ ColorBuffer ]
	-- let myGame = [GameElement {x=20, y=40, vx=0, vy=0, w=10, h=10, col=Color3 0 0 (1::GLfloat)} , GameElement {x=100, y=50, vx=0, vy=0, w=100, h=50, col=Color3 1 0 (0::GLfloat)}]
	-- renderPrimitive Quads (do {
	-- 							    color3f 1 0 0;
	-- 							    vertex3f 0 0 0;
	-- 							    vertex3f 0 0.2 0;
	-- 							    vertex3f 0.2 0.2 0;
	-- 							    vertex3f 0.2 0 0; 
	-- 							})
	(renderPrimitive Quads) (renderGameElement (mario myGame))
	foldl (\a x -> a >> x) (return ()) (map (renderPrimitive Quads) (map renderGameElement (world myGame) ))
	flush
