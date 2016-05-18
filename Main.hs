{-# LANGUAGE Arrows, BangPatterns, NamedFieldPuns , ScopedTypeVariables , TypeOperators #-}
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
import Data.Maybe
import Data.Time.Clock.POSIX
import Data.Time.Clock
import Unsafe.Coerce
import Debug.Trace

import System.CPUTime
import Text.Printf
import Control.Monad

time :: IO a -> IO (Double, a)
time act = do
  start <- getTime
  result <- act
  end <- getTime
  let !delta = end - start
  return (delta, result)

time_ :: IO a -> IO Double
time_ act = do
  start <- getTime
  _ <- act
  end <- getTime
  return $! end - start

getTime :: IO Double
getTime = realToFrac `fmap` getPOSIXTime

runForAtLeast :: Double -> Int -> (Int -> IO a) -> IO (Double, Int, a)
runForAtLeast howLong initSeed act = loop initSeed (0::Int) =<< getTime
  where
    loop !seed !iters initTime = do
      now <- getTime
      when (now - initTime > howLong * 10) $
        fail (printf "took too long to run: seed %d, iters %d" seed iters)
      (elapsed,result) <- Main.time (act seed)
      if elapsed < howLong
        then loop (seed * 2) (iters+1) initTime
        else return (elapsed, seed, result)

secs :: Double -> String
secs k
    | k < 0      = '-' : secs (-k)
    | k >= 1     = k        `with` "s"
    | k >= 1e-3  = (k*1e3)  `with` "ms"
    | k >= 1e-6  = (k*1e6)  `with` "us"
    | k >= 1e-9  = (k*1e9)  `with` "ns"
    | k >= 1e-12 = (k*1e12) `with` "ps"
    | otherwise  = printf "%g s" k
     where with (t :: Double) (u :: String)
               | t >= 1e9  = printf "%.4g %s" t u
               | t >= 1e6  = printf "%.0f %s" t u
               | t >= 1e5  = printf "%.1f %s" t u
               | t >= 1e4  = printf "%.2f %s" t u
               | t >= 1e3  = printf "%.3f %s" t u
               | t >= 1e2  = printf "%.4f %s" t u
               | t >= 1e1  = printf "%.5f %s" t u
               | otherwise = printf "%.6f %s" t u

---------------------------------- general types for the game ---------------

data GameElement = GameElement {
                                                x :: GLfloat,
                                                y :: GLfloat,
                                                vx:: GLfloat,
                                                vy:: GLfloat,
                                                ay:: GLfloat,
                                                w :: GLfloat,
                                                h :: GLfloat,
                                                moving_v :: Bool,
                                                moving_h :: Bool,
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
data Direction = Upd | Downd | Leftd | Rightd | Nodir deriving (Eq, Show)

---------------------------------- game logic -------------------------------
myInitGL :: IO ()
myInitGL = do
        (_progName, _args) <- Graphics.UI.GLUT.getArgsAndInitialize;
        _window <- Graphics.UI.GLUT.createWindow "Mario";
        return ();      

idle :: IORef (Event Input) -> IORef POSIXTime -> 
        ReactHandle (Event Input) (IO ()) -> IO ()
idle newInput oldTime rh = do 
    newInput' <- readIORef newInput
    -- newTime'  <- get elapsedTime
    newTime'  <- getPOSIXTime
    oldTime'  <- get oldTime
    -- let dt = ((fromIntegral ( newTime' - oldTime'))/1000)
    let dt = ((realToFrac newTime') - (realToFrac oldTime'))
    -- let dt = trace ("In idle: " ++ show dt_ ++ "\n") dt_
    -- let dt = ((fromIntegral ( newTime' - oldTime')))
    _ <- if (dt > 0.03) then do 
    -- _ <- if (dt > 0.1) then do 
                                                        -- secs <$> time_ (react rh (dt, Just newInput')) >>= print
                                                        react rh (dt, Just newInput')
                                                        writeIORef oldTime newTime' 
                                else return()
    return ()

-- Snapping integral 
integral_1 = (iPre 0 &&& FRP.Yampa.time) >>> sscan f (0, 0) >>> arr fst
    where f (!prevVal, !prevTime) (!val, !time) 
            | val == 0 = 
                (0, time)
                -- (prevlVal, time)
            | otherwise         = 
                ((realToFrac $ time - prevTime) * val, time)

integral_2 = (iPre 0 &&& FRP.Yampa.time) >>> sscan f (0, 0) >>> arr fst
    where f (!prevVal, !prevTime) (!val, !time) 
            | val == 0 = 
                (prevVal, time)
                -- (prevVal, time)
            | otherwise         = 
                ((realToFrac $ time - prevTime) * val, time)

update_player_from_keys_logging = False
update_player_from_keys :: SF (ParsedInput, Game) Game
update_player_from_keys = proc (i_pi@(ParsedInput{ wEvs, aEvs, sEvs, dEvs }), i_game) -> do

        let there_is_a = isEvent aEvs
        let there_is_d = isEvent dEvs
        let there_is_w = isEvent wEvs
        let key_col = [there_is_a, there_is_d, there_is_w]

        let game = if update_player_from_keys_logging then (trace ("In update_player_from_keys input: " ++ "\n" ++ show i_game ++ "\n") i_game) else i_game
        let [is_a, is_d, is_w] = if update_player_from_keys_logging then (trace ("In update_player_from_keys keys [a,d,w]: " ++ "\n" ++ show key_col ++ "\n") key_col) else key_col
        let horiz_speed 
                                | (is_d && not is_a) = 150
                                | (not is_d && is_a) = -150
                                | (not is_d && not is_a) = 0
                                | (is_d && is_a) = 0
        let vertical_speed = if (is_w && not (moving_v $ mario $ game)) then 800.0 else  (vy $ mario $ game)
        let new_moving_v = if (is_w && not (moving_v $ mario $ game)) then True else (moving_v $ mario $ game)
        let new_ay = if new_moving_v then -901.0 else (ay $ mario $ game)
        let po_game = game {mario = (mario game) {vx = ((realToFrac (horiz_speed )) :: GLfloat), vy = ((realToFrac (vertical_speed )) :: GLfloat), moving_v = new_moving_v, ay = new_ay }}
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

bounds_updater :: SF (Game, Game) Game
bounds_updater = proc (game, prev_game) -> do
        let sb = update_bounds_from_game game

        let new_y = if (y $ mario $ game) < (lowy sb)  then (lowy sb) else (if (y $ mario $ game) > (highy sb) then (highy sb) else (y $ mario $ game) )
        let new_ay = if (y $ mario $ game) < (lowy sb) then 0.0 else (ay $ mario $ game)
        let new_moving_v = if (y $ mario $ game) < (lowy sb) then False else (moving_v $ mario $ game)
        let new_vy = if not new_moving_v then 0 else (if new_y == (highy sb) then 0 else (vy $ mario $ game) )

        let new_x = if (x $ mario $ game) < (lowx sb)  then (lowx sb) else ( if (x $ mario $ game) > (highx sb) then (highx sb) else (x $ mario $ game) )

        returnA -< game {mario = (mario game) {y = new_y , x = new_x, vx = 0, vy = new_vy , ay = new_ay, moving_v = new_moving_v}}
        where update_bounds_from_game g = PlayerBounds {highx = 1000.0, lowx = 0.0, highy = 1000.0, lowy = 0.0}


update_bounds_from_game :: Game -> PlayerBounds
update_bounds_from_game g = PlayerBounds {
                                                         highx = minimum $ map (\ge -> (x ge)) $ filter (\ge -> ((x ge) >  (x $ mario g)) && ((y ge) - (h ge) / 2 < (y $ mario g)) && ((y ge) + (h ge) / 2 > (y $ mario g)) ) (world g)
                                                        ,lowx  = maximum $ map (\ge -> (x ge)) $ filter (\ge -> ((x ge) <= (x $ mario g)) && ((y ge) - (h ge) / 2 < (y $ mario g)) && ((y ge) + (h ge) / 2 > (y $ mario g)) ) (world g)
                                                        ,highy = minimum $ map (\ge -> (y ge)) $ filter (\ge -> ((y ge) >  (y $ mario g)) && ((x ge) - (w ge) / 2 < (x $ mario g)) && ((x ge) + (w ge) / 2 > (x $ mario g)) ) (world g)
                                                        ,lowy  = maximum $ map (\ge -> (y ge)) $ filter (\ge -> ((y ge) <= (y $ mario g)) && ((x ge) - (w ge) / 2 < (x $ mario g)) && ((x ge) + (w ge) / 2 > (x $ mario g)) ) (world g)
                                                }
collision_detector_logging = False
collision_detector :: SF (Game, Game) Game
collision_detector = proc (game, prev_game) -> do
        let (collx, colly, colldir) = detect_player_collisions (mario prev_game) (mario game) (world game)
        let (coll_x, coll_y, coll_dir) = if collision_detector_logging then (trace ("In collision_detector : " ++ "\n" ++ show (collx, colly, colldir) ++ "\n") (collx, colly, colldir)) else (collx, colly, colldir)
        let new_y = if ((elem Upd coll_dir) || (elem Downd coll_dir)) then coll_y else (y $ mario $ game)
        let new_ay = (ay $ mario $ game)
        let new_moving_v = if (elem Downd coll_dir) then False else True
        let new_vy = if not new_moving_v then 0 else (if (elem Upd coll_dir) then 0 else (vy $ mario $ game) )
        let new_x = if ((elem Leftd coll_dir) || (elem Rightd coll_dir)) then coll_x else (x $ mario $ game)
        returnA -< game {mario = (mario game) {y = new_y , x = new_x, vx = 0, vy = new_vy , ay = new_ay, moving_v = new_moving_v}}

-- return ax + by + c form of a line from 2 points defining it
epsilon = 1 / 1000000
line x1 y1 x2 y2 = 
        let     a = if (abs (y1 - y2) < epsilon) then 0 else 1
                b = if (abs (y1 - y2) < epsilon) then 1 else if (abs (x1 - x2) < epsilon) then 0 else (x1 - x2)/(y2 - y1)
                c = if (abs (y1 - y2) < epsilon) then (-y1) else if (abs (x1 - x2) < epsilon) then (-x1) else (x1 * y2 - x2 * y1) / (y1 -y2)
        in (a, b, c)

between a b c = ((a <= b ) && (b <= c)) || ((a >= b) && (b >= c))

segment_intersect_log = False
segment_intersect :: GLfloat -> GLfloat->  GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> (Maybe GLfloat, Maybe GLfloat)
segment_intersect x1i y1i x2i y2i u1i v1i u2i v2i = 
        let     (x1, y1, x2, y2, u1, v1, u2, v2) = if (segment_intersect_log && y1i <= 20 && y1i >= (-20)) then (trace ("In segment_intersect coordinates: " ++ "\n" ++ show (x1i, y1i, x2i, y2i, u1i, v1i, u2i, v2i) ++ "\n") (x1i, y1i, x2i, y2i, u1i, v1i, u2i, v2i)) else (x1i, y1i, x2i, y2i, u1i, v1i, u2i, v2i)
                (a1, b1, c1) = if (segment_intersect_log && y1i <= 20 && y1i >= (-20)) then (trace ("In segment_intersect line 1: " ++ "\n" ++ show (line x1 y1 x2 y2) ++ "\n") (line x1 y1 x2 y2)) else (line x1 y1 x2 y2)
                (a2, b2, c2) = if (segment_intersect_log && y1i <= 20 && y1i >= (-20)) then (trace ("In segment_intersect line 2: " ++ "\n" ++ show (line u1 v1 u2 v2) ++ "\n") (line u1 v1 u2 v2)) else (line u1 v1 u2 v2)
                (ix, iy) =      if ( (a1 == 0 && a2 == 0) || (b1 == 0 && b2 == 0) ) then (Nothing :: Maybe GLfloat, Nothing :: Maybe GLfloat) else
                                if (a1 == 0 && b2 == 0) then (if (between x1 (-c2/a2) x2) && (between v1 (-c1/b1) v2) then (Just (-c2/a2),Just (-c1/b1)) else (Nothing, Nothing)) else
                                if (b1 == 0 && a2 == 0) then (if (between u1 (-c1/a1) u2) && (between y1 (-c2/b2) y2) then (Just (-c1/a1),Just (-c2/b2)) else (Nothing, Nothing)) else 
                                if (a1 == 0)            then (if (between x1 ( (b2*c1-b1*c2)/(b1*a2) ) x2) && (between v1 (-c1/b1) v2) then (Just ( (b2*c1-b1*c2)/(b1*a2) ), Just (-c1/b1)) else (Nothing, Nothing)) else
                                if (b1 == 0)            then (if (between y1 ( (a2*c1-a1*c2)/(b2*a1) ) y2) && (between u1 (-c1/a1) u2) then (Just (-c1/a1), Just( (a2*c1-a1*c2)/(b2*a1) ) ) else (Nothing, Nothing)) else
                                if (a2 == 0)            then (if (between u1 ( (b1*c2-b2*c1)/(b2*a1) ) u2) && (between y1 (-c2/b2) y2) then (Just ( (b1*c2-b2*c1)/(b2*a1) ), Just (-c2/b2)) else (Nothing, Nothing)) else
                                if (b2 == 0)            then (if (between v1 ( (a1*c2-a2*c1)/(b1*a2) ) v2) && (between x1 (-c2/a2) x2) then (Just (-c2/a2), Just( (a1*c2-a2*c1)/(b1*a2) ) ) else (Nothing, Nothing)) else (Nothing :: Maybe GLfloat, Nothing :: Maybe GLfloat)
                (l_ix, l_iy) = if (segment_intersect_log && y1i <= 20 && y1i >= (-20)) then (trace ("In segment_intersect intersection: " ++ "\n" ++ show ix ++ " " ++ show iy ++ "\n") (ix, iy)) else (ix, iy)
        in (l_ix, l_iy)

collision_logging = False
detect_player_collisions :: GameElement -> GameElement -> GameScene -> (GLfloat, GLfloat, [Direction])
detect_player_collisions prev_mario mario world =
        let     m_world = map (\ge -> ge { w = (w ge) + (w mario), h = (h ge) + (h mario)}) world
                collisions1 = map (collide_player_with_rectangle prev_mario mario) m_world
                collisions2 = if (collision_logging && (y mario) <= 20 && (y mario) >= (-20)) then (trace ("In detect_player_collisions raw collisions: " ++ "\n" ++ show collisions1 ++ "\n") collisions1) else collisions1
                collisions3 = filter (\(x,y,coll_dir) -> coll_dir /= Nodir) collisions2
                collisions4 = if (collision_logging && (y mario) <= 20 && (y mario) >= (-20)) then (trace ("In detect_player_collisions filtered: " ++ "\n" ++ show collisions3 ++ "\n") collisions3) else collisions3
                (fx, fy, fDirs) = if (collision_logging && (y mario) <= 20 && (y mario) >= (-20)) then (trace ("In detect_player_collisions folded: " ++ "\n" ++ show (foldl dir_fold (0 , 0 , [Nodir]) collisions4) ++ "\n") (foldl dir_fold (0 , 0 , [Nodir]) collisions4)) else (foldl dir_fold (0 , 0 , [Nodir]) collisions4)
        in (fx, fy, fDirs)

dir_fold :: (GLfloat, GLfloat, [Direction]) -> (GLfloat, GLfloat, Direction) -> (GLfloat, GLfloat, [Direction])
dir_fold (ax, ay, dirs) (x, y, coll_dir) =
                case (x, y, coll_dir) of
                        (_, _, Upd)    -> (ax, y, dirs ++ [Upd])
                        (_, _, Downd)  -> (ax, y, dirs ++ [Downd])
                        (_, _, Leftd)  -> (x, ay, dirs ++ [Leftd])
                        (_, _, Rightd) -> (x, ay, dirs ++ [Rightd])

collide_player_with_rectangle prev_mario mario platform = 
        let     (ux, uy) = segment_intersect (x prev_mario) (y prev_mario) (x mario) (y mario) ((x platform) - ((w platform)/2)) ((y platform) + ((h platform)/2)) ((x platform) + ((w platform)/2)) ((y platform) + ((h platform)/2))
                (dx, dy) = segment_intersect (x prev_mario) (y prev_mario) (x mario) (y mario) ((x platform) - ((w platform)/2)) ((y platform) - ((h platform)/2)) ((x platform) + ((w platform)/2)) ((y platform) - ((h platform)/2))
                (lx, ly) = segment_intersect (x prev_mario) (y prev_mario) (x mario) (y mario) ((x platform) - ((w platform)/2)) ((y platform) + ((h platform)/2)) ((x platform) - ((w platform)/2)) ((y platform) - ((h platform)/2))
                (rx, ry) = segment_intersect (x prev_mario) (y prev_mario) (x mario) (y mario) ((x platform) + ((w platform)/2)) ((y platform) + ((h platform)/2)) ((x platform) + ((w platform)/2)) ((y platform) - ((h platform)/2))
                (ix, iy, coll_dir) =    if ( (ux, uy) /= (Nothing, Nothing) && (y prev_mario) >= ((y platform) + ((h platform)/2)) && (y prev_mario) > (y mario)) then (fromJust ux, fromJust uy, Downd) else
                                        if ( (dx, dy) /= (Nothing, Nothing) && (y prev_mario) <= ((y platform) - ((h platform)/2)) && (y prev_mario) < (y mario)) then (fromJust dx, fromJust dy, Upd) else
                                        if ( (lx, ly) /= (Nothing, Nothing) && (x prev_mario) <= ((x platform) - ((w platform)/2)) && (x prev_mario) < (x mario)) then (fromJust lx, fromJust ly, Rightd) else  
                                        if ( (rx, ry) /= (Nothing, Nothing) && (x prev_mario) >= ((x platform) + ((w platform)/2)) && (x prev_mario) > (x mario)) then (fromJust rx, fromJust ry, Leftd) else (0 ,0 , Nodir)
        in (ix, iy, coll_dir)   

master_combine :: SF ParsedInput Game
master_combine = proc pi -> do
        rec 
                g_pu    <- player_update -< (pi, g_bu_d)
                g_bu    <- (initial_game --> collision_detector) -< (g_pu, g_bu_d)
                g_bu_d  <- iPre initial_game -< g_bu
        returnA -< g_bu

seeder :: SF Game (Game , Event (Bool))
seeder = proc game -> do
        returnA -< (initial_game, Event (True))


initial_game = Game {mario =   GameElement {x = 100.0, y = 100.0, vx = 0.0, vy = 0.0, ay = -901.0, w = 10.0, h = 20.0, moving_v = False, moving_h = False, col = Color3 1.0 0.0 0.0}, 
                         world = [ GameElement {x = 500.0, y = -2.0, vx = 0.0, vy = 0.0, ay = 0.0, w = 1002.0, h = 10.0, moving_v = False, moving_h = False, col = Color3 0.0 1.0 0.0}
                                          ,GameElement {x = 500.0, y = 1000.0, vx = 0.0, vy = 0.0, ay = 0.0, w = 1002.0, h = 10.0, moving_v = False, moving_h = False, col = Color3 0.0 1.0 0.0}
                                          ,GameElement {x = -2.0, y = 500.0, vx = 0.0, vy = 0.0, ay = 0.0, w = 10.0, h = 1002.0, moving_v = False, moving_h = False, col = Color3 0.0 1.0 0.0}
                                          ,GameElement {x = 1000.0, y = 500.0, vx = 0.0, vy = 0.0, ay = 0.0, w = 10.0, h = 1002.0, moving_v = False, moving_h = False, col = Color3 0.0 1.0 0.0}
                                          ,GameElement {x = 300.0, y = 200.0, vx = 0.0, vy = 0.0, ay = 0.0, w = 100.0, h = 20.0, moving_v = False, moving_h = False, col = Color3 0.0 1.0 1.0}
                                          ,GameElement {x = 600.0, y = 400.0, vx = 0.0, vy = 0.0, ay = 0.0, w = 100.0, h = 20.0, moving_v = False, moving_h = False, col = Color3 0.0 1.0 1.0}
                                          ,GameElement {x = 900.0, y = 600.0, vx = 0.0, vy = 0.0, ay = 0.0, w = 100.0, h = 20.0, moving_v = False, moving_h = False, col = Color3 0.0 1.0 1.0}
                                          ]
                         }
mainSF = parseInput >>> master_combine >>> draw


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
                oldTimeRef  <- newIORef (0 :: POSIXTime); -- IORef to pass time delta between yampa components
                -- oldTimeRef  <- newIORef (0 :: Int); -- IORef to pass time delta between yampa components

                rh <- reactInit (myInitGL >> return NoEvent) 
                                (\_ _ b -> b >> return False) 
                                mainSF;
                Graphics.UI.GLUT.displayCallback $= return ();
                reshapeCallback $= Just reshape;
                Graphics.UI.GLUT.idleCallback $= Just (idle newInputRef oldTimeRef rh);
                Graphics.UI.GLUT.keyboardMouseCallback $= Just (\k ks m _ -> writeIORef newInputRef (Event $ Keyboard k ks m));
                actionOnWindowClose $= MainLoopReturns;
                -- oldTime' <- get elapsedTime;
                oldTime' <- getPOSIXTime;
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


-- Event Definition:
filterKeyDowns :: SF (Event Input) (Event Input)
filterKeyDowns = arr $ filterE ((== Down ) . keyState)

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
    upEvs    <- filterKey (SpecialKey KeyUp)    -< down
    downEvs  <- filterKey (SpecialKey KeyDown)  -< down
    rightEvs <- filterKey (SpecialKey KeyRight) -< down
    leftEvs  <- filterKey (SpecialKey KeyLeft)  -< down
    returnA -< ParsedInput wEvs aEvs sEvs dEvs 
                           upEvs downEvs rightEvs leftEvs
    where countKey c  = filterE ((==(Char c)) . key) ^>> keyIntegral 1
          senseKey c = arr $ filterE ((==(Char c)) . key)
          filterKey k = arr $ filterE ((==k) . key)
