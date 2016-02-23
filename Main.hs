module Main where

import System.IO
import Graphics.UI.GLUT

data GameElement = GameElement {
						x :: GLfloat,
						y :: GLfloat,
						vx:: GLfloat,
						vy:: GLfloat,
						w :: GLfloat,
						h :: GLfloat
						-- col :: Color3
					} deriving (Show)

type Game = [ GameElement ]

-- main =  putStr "Provide name :: " >> hFlush stdout >> getLine >>= \l -> putStr ("Hello " ++ l ++ "!\n")
main :: IO ()
main = do
	(_progName, _args) <- Graphics.UI.GLUT.getArgsAndInitialize
	_window <- Graphics.UI.GLUT.createWindow "Hello World"
	Graphics.UI.GLUT.displayCallback $= display
	mainLoop

color3f r g b = color $ Color3 r g (b :: GLfloat)
vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)

renderGame :: GameElement -> IO ()
renderGame g = do {
					color3f 1 0 0;
					vertex3f (x g - (w g) / 2 ) (y g - (h g) / 2 ) 0;
					vertex3f (x g + (w g) / 2 ) (y g - (h g) / 2 ) 0;
					vertex3f (x g + (w g) / 2 ) (y g + (h g) / 2 ) 0;
					vertex3f (x g - (w g) / 2 ) (y g + (h g) / 2 ) 0;
					}
 
display :: DisplayCallback
display = do
	clear [ ColorBuffer ]
	let myGame = [GameElement {x=0, y=0, vx=0, vy=0, w=1, h=1} , GameElement {x=1.0, y=1.0, vx=0, vy=0, w=1, h=1}]
	-- renderPrimitive Quads (do {
	-- 							    color3f 1 0 0;
	-- 							    vertex3f 0 0 0;
	-- 							    vertex3f 0 0.2 0;
	-- 							    vertex3f 0.2 0.2 0;
	-- 							    vertex3f 0.2 0 0; 
	-- 							})
	foldl (\a x -> a >> x) (return ()) (map (renderPrimitive Quads) (map renderGame myGame))
	flush
