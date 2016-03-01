module Input where

import FRP.Yampa

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