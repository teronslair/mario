{-# LANGUAGE Arrows, BangPatterns, NamedFieldPuns #-}
module Main where

import FRP.Yampa
import FRP.Yampa.Vector3
import FRP.Yampa.Utilities
import System.IO
-- import Graphics.UI.GLUT
import Graphics.UI.GLUT hiding (Level,Vector3(..),normalize)
import qualified Graphics.UI.GLUT as G(Vector3(..))
import Foreign.C.Types
import Data.IORef
import Data.Time.Clock.POSIX
import Unsafe.Coerce
import Debug.Trace

---------------------------------- general types for the game ---------------

data GameElement = GameElement {
						x :: GLfloat,
						y :: GLfloat,
						vx:: GLfloat,
						vy:: GLfloat,
						ay:: GLfloat,
						w :: GLfloat,
						h :: GLfloat,
						moving :: Bool,
						col :: Color3 GLfloat
					} deriving (Show)
data PlayerBounds = PlayerBounds {
						highx :: GLfloat,
						lowx :: GLfloat,
						highy :: GLfloat,
						lowy :: GLfloat
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
    -- let dt = let dt' = (fromIntegral $ newTime' - oldTime')/50
             -- in if dt' < 0.8 then dt' else 0.8
    let dt = ((fromIntegral ( newTime' - oldTime'))/50)
    _ <- if (dt > 0.1) then do 
    							react rh (dt, Just newInput')
    							writeIORef oldTime newTime' 
    				else return()
    -- react rh (dt, Just newInput')
    -- writeIORef oldTime newTime'
    return ()


-- testsf :: Game -> SF () Game
-- testsf game0 = constant game0
testsf_with_end :: SF Game (Game , Event (Bool))
testsf_with_end = proc game -> do
	vert_speed <- integral -< ((realToFrac (ay $ mario $ game)) :: Float)
	vert_movement <- integral -< vert_speed + ((realToFrac (vy $ mario $ game)) :: Float)
	-- newy <- ((realToFrac (vert_movement + ((realToFrac ( y $ mario $ game)) :: Float) ) ) :: GLfloat)
	-- ev <- NoEvent
	let newy = ((realToFrac (vert_movement + ((realToFrac ( y $ mario $ game)) :: Float) ) ) :: GLfloat)
	-- ev <- edge -< newy <= 0
	let ev = if ( newy > 0) then NoEvent else Event (True) 
	returnA -< (game {mario = (mario game) {y = newy }}, ev ) 
	-- returnA -< (game {mario = (mario game) {y = ((realToFrac (vert_movement + ((realToFrac ( y $ mario $ game)) :: Float) ) ) :: GLfloat) }}, if ( ((realToFrac (vert_movement + ((realToFrac ( y $ mario $ game)) :: Float) ) ) :: GLfloat) > 0) then NoEvent else Event () ) 
	-- where newy = ((realToFrac (vert_movement + ((realToFrac ( 0.0)) :: Float) ) ) :: GLfloat)
	-- where newy = ((realToFrac (vert_movement + ((realToFrac ( y $ mario $ game)) :: Float) ) ) :: GLfloat)
		  -- ev = if (1 > 0) then NoEvent else Event ()
	-- where x = newy



testsf :: SF Game Game
-- testsf = arr (\g->g)
testsf = proc game -> do
	vert_speed <- integral -< ((realToFrac (ay $ mario $ game)) :: Float)
	vert_movement <- integral -< vert_speed + ((realToFrac (vy $ mario $ game)) :: Float)
	returnA -< game {mario = (mario game) {y = ((realToFrac (vert_movement + ((realToFrac ( y $ mario $ game)) :: Float) ) ) :: GLfloat) }}

-- move_mario :: Game -> SF ParsedInput Game
-- -- testsf = arr (\g->g)
-- move_mario game = proc pi@(ParsedInput{ wEvs, aEvs, sEvs, dEvs }) -> do
-- 	-- vert_speed <- integral -< ((realToFrac (ay $ mario $ game)) :: Float)
-- 	let horiz_movement = 15 * realToFrac(dEvs - aEvs) :: Float
-- 	returnA -< game {mario = (mario game) {x = ((realToFrac (horiz_movement + ((realToFrac ( x $ mario $ game)) :: Float) ) ) :: GLfloat) }}

-- integr' :: SF GLfloat GLfloat
-- -- integr' = proc in -> do

-- integr' = (iPre zeroVector &&& time) >>> sscan f (zeroVector, 0) >>> arr fst
--     where f (!prevVal, !prevTime) (!val, !time) = (prevVal ^+^ (realToFrac $ time - prevTime) *^ val, time)

-- Snapping integral 
integral_1 = (iPre 0 &&& time) >>> sscan f (0, 0) >>> arr fst
    where f (!prevVal, !prevTime) (!val, !time) 
            | val == 0 = 
                (0, time)
                -- (prevlVal, time)
            | otherwise         = 
                ((realToFrac $ time - prevTime) * val, time)

integral_2 = (iPre 0 &&& time) >>> sscan f (0, 0) >>> arr fst
    where f (!prevVal, !prevTime) (!val, !time) 
            | val == 0 = 
                (prevVal, time)
                -- (prevVal, time)
            | otherwise         = 
                ((realToFrac $ time - prevTime) * val, time)

update_player_from_keys_logging = False
update_player_from_keys :: SF (ParsedInput, Game) Game
update_player_from_keys = proc (i_pi@(ParsedInput{ wEvs, aEvs, sEvs, dEvs }), i_game) -> do
	-- let horiz_speed = if (dEvs > 0 || aEvs > 0) then realToFrac(dEvs - aEvs) :: Float else 0
	-- let (i_aEvs, i_dEvs) = if update_player_from_keys_logging then (trace ("In update_player_from_keys input: " ++ "\naCount: " ++ show aEvs ++ " dEvs: " ++ show dEvs ++ "\n") (aEvs, dEvs)) 
															  -- else (aEvs, dEvs)
	let there_is_a = isEvent aEvs
	let there_is_d = isEvent dEvs
	let there_is_w = isEvent wEvs
	let key_col = [there_is_a, there_is_d, there_is_w]

	let game = if update_player_from_keys_logging then (trace ("In update_player_from_keys input: " ++ "\n" ++ show i_game ++ "\n") i_game) else i_game
	let [is_a, is_d, is_w] = if update_player_from_keys_logging then (trace ("In update_player_from_keys keys [a,d,w]: " ++ "\n" ++ show key_col ++ "\n") key_col) else key_col
	let horiz_speed 
				| (is_d && not is_a) = 10
				| (not is_d && is_a) = -10
				| (not is_d && not is_a) = 0
				| (is_d && is_a) = 0
	let vertical_speed = if (is_w && not (moving $ mario $ game)) then 50.0 else  (vy $ mario $ game)
	let new_moving = if (is_w && not (moving $ mario $ game)) then True else (moving $ mario $ game)
	let new_ay = if new_moving then -4.0 else (ay $ mario $ game)
	let po_game = game {mario = (mario game) {vx = ((realToFrac (horiz_speed )) :: GLfloat), vy = ((realToFrac (vertical_speed )) :: GLfloat), moving = new_moving, ay = new_ay }}
	let o_game = if update_player_from_keys_logging then (trace ("In update_player_from_keys output: " ++ "\n" ++ show po_game ++ "\n") po_game) else po_game
	returnA -< o_game

player_phsx :: SF Game Game
player_phsx = proc game -> do
	vert_speed <- integral_1 -< ((realToFrac (ay $ mario $ game)) :: Float)
	vert_movement <- integral_1 -< vert_speed + ((realToFrac (vy $ mario $ game)) :: Float)
	horiz_movement <- integral_2 -< ((realToFrac (vx $ mario $ game)) :: Float)
	let newvy = ((realToFrac (vert_speed +  ((realToFrac ( vy $ mario $ game)) :: Float) ) ) :: GLfloat)
	let newy = ((realToFrac (vert_movement +  ((realToFrac ( y $ mario $ game)) :: Float) ) ) :: GLfloat)
	let newx = ((realToFrac (horiz_movement + ((realToFrac ( x $ mario $ game)) :: Float) ) ) :: GLfloat)
	returnA -< game {mario = (mario game) {y = newy , x = newx, vy = newvy}}

player_update = update_player_from_keys >>> player_phsx

bounds_updater ::SF Game Game
bounds_updater = proc game -> do
	let sb = update_bounds_from_game game

	let new_y = if (y $ mario $ game) < (lowy sb)  then (lowy sb) else (if (y $ mario $ game) > (highy sb) then (highy sb) else (y $ mario $ game) )
	let new_ay = if (y $ mario $ game) < (lowy sb) then 0.0 else (ay $ mario $ game)
	let new_moving = if (y $ mario $ game) < (lowy sb) then False else (moving $ mario $ game)
	let new_vy = if not new_moving then 0 else (if new_y == (highy sb) then 0 else (vy $ mario $ game) )
	-- let newy = if (y $ mario $ game) > (highy sb) then (highy sb) else (y $ mario $ game)

	let new_x = if (x $ mario $ game) < (lowx sb)  then (lowx sb) else ( if (x $ mario $ game) > (highx sb) then (highx sb) else (x $ mario $ game) )

	-- let new_vx = if ((new_x == (highx sb)) || (new_x == (lowx sb))) then 0 else (vx $ mario $ game)
	-- let newvy = if ((newy == (highy sb)) || (newy == (lowy sb))) then 0 else (vy $ mario $ game)
	returnA -< game {mario = (mario game) {y = new_y , x = new_x, vx = 0, vy = new_vy , ay = new_ay, moving = new_moving}}
	-- where update_bounds_from_game g = PlayerBounds {highx = 1000.0, lowx = 0.0, highy = 1000.0, lowy = 0.0}


update_bounds_from_game :: Game -> PlayerBounds
update_bounds_from_game g = PlayerBounds {
								 highx = minimum $ map (\ge -> (x ge)) $ filter (\ge -> ((x ge) >  (x $ mario g)) && ((y ge) - (h ge) / 2 < (y $ mario g)) && ((y ge) + (h ge) / 2 > (y $ mario g)) ) (world g)
								,lowx  = maximum $ map (\ge -> (x ge)) $ filter (\ge -> ((x ge) <= (x $ mario g)) && ((y ge) - (h ge) / 2 < (y $ mario g)) && ((y ge) + (h ge) / 2 > (y $ mario g)) ) (world g)
								,highy = minimum $ map (\ge -> (y ge)) $ filtewr (\ge -> ((y ge) >  (y $ mario g)) && ((x ge) - (w ge) / 2 < (x $ mario g)) && ((x ge) + (w ge) / 2 > (x $ mario g)) ) (world g)
								,lowy  = maximum $ map (\ge -> (y ge)) $ filter (\ge -> ((y ge) <= (y $ mario g)) && ((x ge) - (w ge) / 2 < (x $ mario g)) && ((x ge) + (w ge) / 2 > (x $ mario g)) ) (world g)
							}
-- update_bounds_from_game :: Game -> PlayerBounds
-- update_bounds_from_game g = let hbx = minimum $ map (\ge -> (x ge)) $ filter (\ge -> ((x ge) > (x $ mario g)) && ((y ge) - (h ge) / 2 < (y $ mario g)) && ((y ge) + (h ge) / 2 > (y $ mario g)) ) (world g)
-- 								lbx = maximum $ map (\ge -> (x ge)) $ filter (\ge -> ((x ge) < (x $ mario g)) && ((y ge) - (h ge) / 2 < (y $ mario g)) && ((y ge) + (h ge) / 2 > (y $ mario g)) ) (world g)
-- 								hby = minimum $ map (\ge -> (y ge)) $ filter (\ge -> ((y ge) > (y $ mario g)) && ((x ge) - (w ge) / 2 < (x $ mario g)) && ((x ge) + (w ge) / 2 > (x $ mario g)) ) (world g)
-- 								lby = maximum $ map (\ge -> (y ge)) $ filter (\ge -> ((y ge) < (y $ mario g)) && ((x ge) - (w ge) / 2 < (x $ mario g)) && ((x ge) + (w ge) / 2 > (x $ mario g)) ) (world g)
-- 								-- in PlayerBounds {highx = hbx, lowx = 0, highy = 0, lowy = 0}
-- 								in PlayerBounds {highx = hbx, lowx = lbx, highy = hby, lowy = lby}

master_combine :: SF ParsedInput Game
master_combine = proc pi -> do
	-- rec cgg <- (player_update >>> (dSwitch seeder bounds_updater) >>> iPre initial_game) -< (pi, cgg)
	rec cgg <- player_update >>> (initial_game --> bounds_updater) >>> iPre initial_game -< (pi, cgg)
	-- rec cgg <- player_update >>> (initial_game --> bounds_updater) >>> delay 0.1 initial_game -< (pi, cgg)
	-- rec gg2 <- dSwitch seeder bounds_updater -< gg1
		-- cgg <- pre -< gg2
		-- gg1 <- update_player_from_keys -< (pi, cgg)
		-- gg2 <- returnA -< gg1
			-- g2 	<- player_phsx -< g1
		-- cgg <- returnA -< gg2
		
		-- cgg <- pre -< gg2
	-- returnA -< gg1
	returnA -< cgg

seeder :: SF Game (Game , Event (Bool))
seeder = proc game -> do
	returnA -< (initial_game, Event (True))


-- mainSF = parseInput >>> update >>> draw
initial_game = Game {mario =   GameElement {x = 0.0, y = 0.0, vx = 0.0, vy = 0.0, ay = 0.0, w = 10.0, h = 20.0, moving = False, col = Color3 1.0 0.0 0.0}, 
	                 world = [ GameElement {x = 500.0, y = -2.0, vx = 0.0, vy = 0.0, ay = 0.0, w = 1002.0, h = 1.0, moving = False, col = Color3 0.0 1.0 0.0}
	                 		  ,GameElement {x = 500.0, y = 1000.0, vx = 0.0, vy = 0.0, ay = 0.0, w = 1002.0, h = 1.0, moving = False, col = Color3 0.0 1.0 0.0}
	                 		  ,GameElement {x = -2.0, y = 500.0, vx = 0.0, vy = 0.0, ay = 0.0, w = 1.0, h = 1002.0, moving = False, col = Color3 0.0 1.0 0.0}
	                 		  ,GameElement {x = 1000.0, y = 500.0, vx = 0.0, vy = 0.0, ay = 0.0, w = 1.0, h = 1002.0, moving = False, col = Color3 0.0 1.0 0.0}
	                 		  ,GameElement {x = 300.0, y = 200.0, vx = 0.0, vy = 0.0, ay = 0.0, w = 100.0, h = 20.0, moving = False, col = Color3 0.0 1.0 1.0}
	                 		  ,GameElement {x = 600.0, y = 400.0, vx = 0.0, vy = 0.0, ay = 0.0, w = 100.0, h = 20.0, moving = False, col = Color3 0.0 1.0 1.0}
	                 		  ,GameElement {x = 900.0, y = 600.0, vx = 0.0, vy = 0.0, ay = 0.0, w = 100.0, h = 20.0, moving = False, col = Color3 0.0 1.0 1.0}
	                 		  ]
	                 }
-- mainSF = constant initial_game >>> testsf  >>> draw
-- mainSF = constant initial_game >>> switch testsf_with_end testsf_cont  >>> draw
-- mainSF = parseInput >>> move_mario initial_game >>> switch testsf_with_end testsf_cont  >>> draw
mainSF = parseInput >>> master_combine >>> draw

mainSF1 = constant initial_game >>> testsf

-- testsf_cont :: Event (Bool) -> SF Game Game
-- testsf_cont (_) = testsf
testsf_cont _ = switch testsf_with_end testsf_cont

update = undefined

draw :: SF Game (IO ())
draw = arr $ (\game -> do
        clear [ ColorBuffer ]
        displayGame game
        flush)
        -- swapBuffers)

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing

main :: IO ()
main = do {
	newInputRef <- newIORef NoEvent;   -- IORef to pass keyboard event between yampa components
    oldTimeRef  <- newIORef (0 :: Int); -- IORef to pass time delta between yampa components

    rh <- reactInit (myInitGL >> return NoEvent) 
     				(\_ _ b -> b >> return False) 
                    mainSF;
	Graphics.UI.GLUT.displayCallback $= return ();
	reshapeCallback $= Just reshape;
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

displayGame_logging = False 
displayGame :: Game -> DisplayCallback
displayGame myGame = do
	if displayGame_logging then print "Display" >> print myGame else return ()
	(renderPrimitive Quads) (renderGameElement (mario myGame))
	foldl (\a x -> a >> x) (return ()) (map (renderPrimitive Quads) (map renderGameElement (world myGame) ))
	flush


-------------------------------------- the module that deals with input follows ----

data Input = Keyboard { key       :: Key,
                        keyState  :: KeyState,
                        modifiers :: Modifiers }

data ParsedInput = 
    ParsedInput { wEvs :: Event Input, aEvs :: Event Input, 
                  sEvs :: Event Input, dEvs :: Event Input,
                  upEvs  :: Event Input, downEvs :: Event Input, 
                  rightEvs :: Event Input, leftEvs :: Event Input }
-- data ParsedInput = 
--     ParsedInput { wEvs :: Double, aEvs :: Double, 
--                   sEvs :: Double, dEvs :: Double,
--                   upEvs  :: Event Input, downEvs :: Event Input, 
--                   rightEvs :: Event Input, leftEvs :: Event Input }


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
    down   <- filterKeyDowns                  -< i
    wEvs   <- senseKey 'w'                    -< down
    aEvs   <- senseKey 'a'                    -< down
    sEvs   <- senseKey 's'                    -< down
    dEvs   <- senseKey 'd'                    -< down
    -- wEvs   <- countKey 'w'                    -< down
    -- aEvs   <- countKey 'a'                    -< down
    -- sEvs   <- countKey 's'                    -< down
    -- dEvs   <- countKey 'd'                    -< down
    upEvs    <- filterKey (SpecialKey KeyUp)    -< down
    downEvs  <- filterKey (SpecialKey KeyDown)  -< down
    rightEvs <- filterKey (SpecialKey KeyRight) -< down
    leftEvs  <- filterKey (SpecialKey KeyLeft)  -< down
    returnA -< ParsedInput wEvs aEvs sEvs dEvs 
                           upEvs downEvs rightEvs leftEvs
    where countKey c  = filterE ((==(Char c)) . key) ^>> keyIntegral 1
    	  senseKey c = arr $ filterE ((==(Char c)) . key)
          filterKey k = arr $ filterE ((==k) . key)
