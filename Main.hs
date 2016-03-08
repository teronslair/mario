{-# LANGUAGE Arrows, BangPatterns, NamedFieldPuns #-}
module Main where

import FRP.Yampa
import System.IO
import Graphics.UI.GLUT
import Foreign.C.Types
import Data.IORef
import Data.Time.Clock.POSIX
import Unsafe.Coerce

---------------------------------- general types for the game ---------------

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

---------------------------------- game logic -------------------------------
myInitGL :: IO ()
myInitGL = do
	(_progName, _args) <- Graphics.UI.GLUT.getArgsAndInitialize;
	_window <- Graphics.UI.GLUT.createWindow "Mario";
	return ();	

idle :: IORef (Event Input) -> IORef Int -> 
        ReactHandle (Event Input) (IO ()) -> IO ()
idle newInput oldTime rh = do
    newInput' <- readIORef newInput
    newTime'  <- get elapsedTime
    oldTime'  <- get oldTime
    let dt = let dt' = (fromIntegral $ newTime' - oldTime')/50
             in if dt' < 0.8 then dt' else 0.8
    react rh (dt, Just newInput')
    writeIORef oldTime newTime'
    return ()

-- testsf :: Game -> SF () Game
-- testsf game0 = constant game0
testsf :: SF Game Game
-- testsf = arr (\g->g)
testsf = proc game -> do
	vhoriz <- integral -< vx $ mario $ game
	returnA -< game {mario = (mario game) {vx = vhoriz}}
	-- where newgame = game {mario = (mario game) {vx = vhoriz}}

-- integr' :: SF GLfloat GLfloat
-- -- integr' = proc in -> do

-- integr' = (iPre zeroVector &&& time) >>> sscan f (zeroVector, 0) >>> arr fst
--     where f (!prevVal, !prevTime) (!val, !time) = (prevVal ^+^ (realToFrac $ time - prevTime) *^ val, time)


mainSF = parseInput >>> update >>> draw

update = undefined

draw :: SF Game (IO ())
draw = arr $ (\game -> do
        clear [ ColorBuffer ]
        displayGame game
        flush)
        -- swapBuffers)

main :: IO ()
main = do {
	let {myGame = Game {mario=GameElement{x=20, y=20, vx=2, vy=0, w=10, h=20, col=Color3 1 0 (0::GLfloat)}, 
		 				world = [GameElement {x=200, y=400, vx=0, vy=0, w=300, h=200, col=Color3 0 0 (1::GLfloat)} , GameElement {x=100, y=50, vx=0, vy=0, w=100, h=50, col=Color3 0 0 (1::GLfloat)}]
		 				}
		};
	newInputRef <- newIORef NoEvent;   -- IORef to pass keyboard event between yampa components
    oldTimeRef  <- newIORef (0 :: Int); -- IORef to pass time delta between yampa components

    rh <- reactInit (myInitGL >> return NoEvent) 
     				(\_ _ b -> b >> return False) 
                    mainSF;
	Graphics.UI.GLUT.displayCallback $= return ();
	Graphics.UI.GLUT.idleCallback $= Just (idle newInputRef oldTimeRef rh);
	Graphics.UI.GLUT.keyboardMouseCallback $= Just (\k ks m _ -> writeIORef newInputRef (Event $ Keyboard k ks m));
	oldTime' <- get elapsedTime;
    writeIORef oldTimeRef oldTime';
	mainLoop;
	}

color3f r g b = color $ Color3 r g (b :: GLfloat)
vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)


my_translate :: GameElement -> GameElement
my_translate g = g {x=(x g)-500, y=(y g)-500}

my_normalize :: GameElement -> GameElement
my_normalize g = g {x = (x g)/1000, y=(y g)/1000, w=(w g)/1000, h=(h g)/1000}


renderGameElement :: GameElement -> IO ()
renderGameElement myge = do {
								let {g = my_normalize $ my_translate myge};
								color (col g);
								vertex3f (x g - (w g) / 2 ) (y g - (h g) / 2 ) 0;
								vertex3f (x g + (w g) / 2 ) (y g - (h g) / 2 ) 0;
								vertex3f (x g + (w g) / 2 ) (y g + (h g) / 2 ) 0;
								vertex3f (x g - (w g) / 2 ) (y g + (h g) / 2 ) 0;
							}
 
displayGame :: Game -> DisplayCallback
displayGame myGame = do
	(renderPrimitive Quads) (renderGameElement (mario myGame))
	foldl (\a x -> a >> x) (return ()) (map (renderPrimitive Quads) (map renderGameElement (world myGame) ))
	flush


-------------------------------------- the module that deals with input follows ----

data Input = Keyboard { key       :: Key,
                        keyState  :: KeyState,
                        modifiers :: Modifiers }

data ParsedInput = 
    ParsedInput { wCount :: Double, aCount :: Double, 
                  sCount :: Double, dCount :: Double,
                  upEvs  :: Event Input, downEvs :: Event Input, 
                  rightEvs :: Event Input, leftEvs :: Event Input }


-- Event Definition:
filterKeyDowns :: SF (Event Input) (Event Input)
filterKeyDowns = arr $ filterE ((==Down) . keyState)

keyIntegral :: Double -> SF (Event a) Double
keyIntegral a = let eventToSpeed (Event _) = a
                    eventToSpeed NoEvent   = 0 
                in arr eventToSpeed >>> integral 
                       
-- Input
parseInput :: SF (Event Input) ParsedInput
parseInput = proc i -> do
    down     <- filterKeyDowns                  -< i
    wCount   <- countKey 'w'                    -< down
    aCount   <- countKey 'a'                    -< down
    sCount   <- countKey 's'                    -< down
    dCount   <- countKey 'd'                    -< down
    upEvs    <- filterKey (SpecialKey KeyUp)    -< down
    downEvs  <- filterKey (SpecialKey KeyDown)  -< down
    rightEvs <- filterKey (SpecialKey KeyRight) -< down
    leftEvs  <- filterKey (SpecialKey KeyLeft)  -< down
    returnA -< ParsedInput wCount aCount sCount dCount 
                           upEvs downEvs rightEvs leftEvs
    where countKey c  = filterE ((==(Char c)) . key) ^>> keyIntegral 1
          filterKey k = arr $ filterE ((==k) . key)
