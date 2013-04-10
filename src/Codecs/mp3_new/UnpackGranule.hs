module UnpackGranule where

--
-- Unpacking code for a granule. This includes:
-- * Decoding the huffman values
-- * Decoding scale factors
--
-- ...and nothing more!
--

--
-- First, we do huffman decoding.
-- 

huffDecode :: [(Int, (Int, MP3Huffman (Int,Int)))] -> Bool ->  BitGetT (ST s) (STUArray s Int Int)
huffDecode [(r0,t0), (r1,t1), (r2,t2)] count1Table = do
    arr <- lift $ newArray (0,575) 0
    i0  <- huffDecodeXY t0 (r0 `div` 2) arr 0
    i1  <- huffDecodeXY t1 (r1 `div` 2) arr i0
    i2  <- huffDecodeXY t2 (r2 `div` 2) arr i1
    len <- liftM fi getLength
    huffDecodeVWXY (getQuadrTable count1Table) len arr i2
    return arr

huffDecodeXY :: (Int, MP3Huffman (Int,Int)) -> Int -> STUArray s Int Int -> Int -> BitGetT (ST s) Int
huffDecodeXY _ 0 _ i                 = return i
huffDecodeXY (linbits, huff) c arr i = do
    (l,(x,y)) <- lookAhead $ lookupHuff huff
    skip $ fi l
    x' <- linsign x
    y' <- linsign y
    lift $ writeArray arr i x'
    lift $ writeArray arr (i+1) y'
    huffDecodeXY (linbits,huff) (c-1) arr (i+2)
  where linsign :: Int -> BitGetT (ST s) Int
        linsign c
            | c == 15 && linbits > 0 = do
                res  <- liftM (+15) $ getInt (fi linbits)
                liftM (\s -> if s then negate res else res) getBit
            | c > 0 = liftM (\s -> if s then negate c else c) getBit
            | otherwise = return c

huffDecodeVWXY :: MP3Huffman (Int,Int,Int,Int) -> Int -> STUArray s Int Int -> Int -> BitGetT (ST s) ()
huffDecodeVWXY huff len arr i = do
    (l,(v,w,x,y)) <- lookAhead $ lookupHuff huff
    skip $ fi l
    v' <- setSign v
    w' <- setSign w
    x' <- setSign x
    y' <- setSign y
    if (i < 571) then do
        lift $ writeArray arr i v'
        lift $ writeArray arr (i+1) w'
        lift $ writeArray arr (i+2) x'
        lift $ writeArray arr (i+3) y'
        let len' = len - (fi l)
        if len' > 0 then huffDecodeVWXY huff len' arr (i+4) else return ()
        else return ()
  where setSign 0 = return 0
        setSign c = liftM (\s -> if s then negate c else c) getBit


-- 
-- Now, here are functions for decoding scale factors
--

-- Preflag is simply a predefined table that may be set to give the
-- higher scale factors a larger range than 4 bits.
mp3UnpackScaleFactors :: ([Int], [[Int]]) -> Bool -> Bool -> Scales
mp3UnpackScaleFactors (large, small) preflag scalefacbit =
    let large'  = if not preflag  then large
                          else zipWith (+) large tablePretab
        large'' = map floatFunc large'
        small'  = map (map floatFunc) small
    in Scales large'' small'
  where
    floatFunc = mp3FloatRep3 (fromEnum scalefacbit)
