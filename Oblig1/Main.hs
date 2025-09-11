{-# OPTIONS -Wno-missing-signatures -Wno-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Main
-- Description : Morse code encoder and decoder with PCM audio signal processing
--
-- This module provides functionality for encoding text messages into Morse code
-- audio signals and decoding Morse code audio signals back into text. It uses
-- PCM (Pulse Code Modulation) format for audio representation and includes
-- signal processing capabilities for filtering and noise reduction.
module Main where

import System.IO
import Foreign.Marshal.Array
import Data.Int
import Control.Monad.Fix
import Control.Arrow ((&&&))

import Data.List
import Data.Char
import Data.Maybe
import Text.Read

import Control.Monad

-- | Frequency of a signal in Hz
type Frequency = Double

-- | Duration in milliseconds
type Length = Int

-- | Sample rate in samples per millisecond
type Rate = Int

-- | Audio signal represented as a list of amplitude samples (floating point values)
type Signal = [Double]

-- | PCM (Pulse Code Modulation) format - 16-bit signed integer samples
type PCM = [Int16]

-- | Scale a frequency from Hz to the per-sample domain based on the sample rate.
-- The result represents the frequency relative to the sampling frequency.
frequencyScale :: Rate -> Frequency -> Double
frequencyScale r = (/fromIntegral (r*1000))


{- Morse code conversion -}

-- | Morse code dot representation (short signal)
dot :: [Bool]
dot = [True]

-- | Morse code dash representation (long signal)
dash :: [Bool]
dash = [True,True,True]

-- | Convert a single character to its Morse code representation.
-- Returns a list of dot/dash patterns, or Nothing if the character
-- is not supported in Morse code.
char2morse :: Char -> Maybe [[Bool]]
char2morse 'A' = Just [dot, dash]
char2morse 'B' = Just [dash, dot, dot, dot]
char2morse 'C' = Just [dash, dot, dash, dot]
char2morse 'D' = Just [dash, dot, dot]
char2morse 'E' = Just [dot]
char2morse 'F' = Just [dot, dot, dash, dot]
char2morse 'G' = Just [dash, dash, dot]
char2morse 'H' = Just [dot, dot, dot, dot]
char2morse 'I' = Just [dot, dot]
char2morse 'J' = Just [dot, dash, dash, dash]
char2morse 'K' = Just [dash, dot, dash]
char2morse 'L' = Just [dot, dash, dot, dot]
char2morse 'M' = Just [dash, dash]
char2morse 'N' = Just [dash, dot]
char2morse 'O' = Just [dash, dash, dash]
char2morse 'P' = Just [dot, dash, dash, dot]
char2morse 'Q' = Just [dash, dash, dot, dash]
char2morse 'R' = Just [dot, dash, dot]
char2morse 'S' = Just [dot, dot, dot]
char2morse 'T' = Just [dash]
char2morse 'U' = Just [dot, dot, dash]
char2morse 'V' = Just [dot, dot, dot, dash]
char2morse 'W' = Just [dot, dash, dash]
char2morse 'X' = Just [dash, dot, dot, dash]
char2morse 'Y' = Just [dash, dot, dash, dash]
char2morse 'Z' = Just [dash, dash, dot, dot]
char2morse '0' = Just [dash, dash, dash, dash, dash]
char2morse '1' = Just [dot, dash, dash, dash, dash]
char2morse '2' = Just [dot, dot, dash, dash, dash]
char2morse '3' = Just [dot, dot, dot, dash, dash]
char2morse '4' = Just [dot, dot, dot, dot, dash]
char2morse '5' = Just [dot, dot, dot, dot, dot]
char2morse '6' = Just [dash, dot, dot, dot, dot]
char2morse '7' = Just [dash, dash, dot, dot, dot]
char2morse '8' = Just [dash, dash, dash, dot, dot]
char2morse '9' = Just [dash, dash, dash, dash, dot]
char2morse _ = Nothing

-- | Prepare a message string for Morse encoding by splitting into words
-- and converting to uppercase.
prepareMessage :: String -> [[Char]]
prepareMessage = words . map toUpper

-- | Encode a single letter to Morse code with single dot-length spacing between
-- dots and dashes. Returns Nothing for unsupported characters.
encodeLetter :: Char -> Maybe [Bool]
encodeLetter c = case char2morse c of
    Nothing -> Nothing
    Just patterns -> Just $ intercalate [False] patterns

-- | Encode a word into Morse code with 3 dot-lengths inter-letter spacing.
-- Unsupported characters are skipped.
encodeWord :: [Char] -> [Bool]
encodeWord word = intercalate [False, False, False] $ mapMaybe encodeLetter word

-- | Encode a sentence (list of words) into Morse code with 7 dot-lengths
-- inter-word spacing.
encodeSentence :: [[Char]] -> [Bool]
encodeSentence = intercalate (replicate 7 False) . map encodeWord

-- | Convert a complete text message to Morse code boolean signal.
-- True represents signal on, False represents signal off.
toMorse :: String -> [Bool]
toMorse = encodeSentence . prepareMessage

-- | Morse symbol types for decoding
data MSymbol = Dot          -- ^ Short signal (dit)
             | Dash         -- ^ Long signal (dah)
             | ShortSpace   -- ^ Space between dots/dashes within a letter
             | LongSpace    -- ^ Space between words
             | DivSpace     -- ^ Other spaces
             deriving (Show, Eq)

-- | Convert a list of Morse symbols back to the corresponding character.
-- Returns '?' for unrecognized patterns and ' ' for word spaces.
morseToChar :: [MSymbol] -> Char
morseToChar [Dot,Dash]                 = 'A'
morseToChar [Dash,Dot,Dot,Dot]         = 'B'
morseToChar [Dash, Dot, Dash, Dot]     = 'C'
morseToChar [Dash, Dot, Dot]           = 'D'
morseToChar [Dot]                      = 'E'
morseToChar [Dot,Dot,Dash,Dot]         = 'F'
morseToChar [Dash,Dash,Dot]            = 'G'
morseToChar [Dot,Dot,Dot,Dot]          = 'H'
morseToChar [Dot,Dot]                  = 'I'
morseToChar [Dot, Dash,Dash,Dash]      = 'J'
morseToChar [Dash,Dot,Dash]            = 'K'
morseToChar [Dot,Dash,Dot,Dot]         = 'L'
morseToChar [Dash,Dash]                = 'M'
morseToChar [Dash,Dot]                 = 'N'
morseToChar [Dash,Dash,Dash]           = 'O'
morseToChar [Dot,Dash,Dash,Dot]        = 'P'
morseToChar [Dash,Dash,Dot,Dash]       = 'Q'
morseToChar [Dot,Dash,Dot]             = 'R'
morseToChar [Dot,Dot,Dot]              = 'S'
morseToChar [Dash]                     = 'T'
morseToChar [Dot,Dot,Dash]             = 'U'
morseToChar [Dot,Dot,Dot,Dash]         = 'V'
morseToChar [Dot,Dash,Dash]            = 'W'
morseToChar [Dash,Dot,Dot,Dash]        = 'X'
morseToChar [Dash,Dot,Dash,Dash]       = 'Y'
morseToChar [Dash,Dash,Dot,Dot]        = 'Z'
morseToChar [Dash,Dash,Dash,Dash,Dash] = '0'
morseToChar [Dot,Dash,Dash,Dash,Dash]  = '1'
morseToChar [Dot,Dot,Dash,Dash,Dash]   = '2'
morseToChar [Dot,Dot,Dot,Dash,Dash]    = '3'
morseToChar [Dot,Dot,Dot,Dot,Dash]     = '4'
morseToChar [Dot,Dot,Dot,Dot,Dot]      = '5'
morseToChar [Dash,Dot,Dot,Dot,Dot]     = '6'
morseToChar [Dash,Dash,Dot,Dot,Dot]    = '7'
morseToChar [Dash,Dash,Dash,Dot,Dot]   = '8'
morseToChar [Dash,Dash,Dash,Dash,Dot]  = '9'
morseToChar [LongSpace]                = ' '
morseToChar _ = '?'

-- | Group Morse symbols into letters and words based on spacing.
-- Removes spacing symbols and groups remaining symbols into meaningful units.
groupSymbols :: [MSymbol] -> [[MSymbol]]
groupSymbols = filter (notElem ShortSpace) . groupBy (\a b -> notLong a b && notShort a b) . filter (/= DivSpace)
  where notLong a b  = a /= LongSpace && b /= LongSpace
        notShort a b = a /= ShortSpace && b /= ShortSpace

-- | Generate an infinite sinusoidal carrier wave signal at the specified
-- frequency and sample rate.
carrier :: Frequency -> Rate -> Signal
carrier freq rate = [sin (2*pi*freq*t) | t <- [0,1/(1000*fromIntegral rate)..]]

-- | Apply amplitude modulation to a signal using a boolean gate signal.
-- True values pass the signal through, False values mute it.
gate :: [Bool] -> Signal -> Signal
gate [] _ = []
gate _ [] = []
gate (g:gs) (s:ss) = (if g then s else 0) : gate gs ss

-- | Stretch each element of a list to span a specified duration in milliseconds
-- at the given sample rate.
stretch :: Length -> Rate -> [a] -> [a]
stretch l r = concatMap (replicate (l * r))

-- | Encode a boolean Morse signal into an audio signal by modulating
-- a carrier wave.
--
-- Parameters:
--
--   * dot length in milliseconds
--   * carrier frequency in Hz
--   * sample rate
--   * boolean Morse code signal
encodeInSignal :: Length -> Frequency -> Rate -> [Bool] -> Signal
encodeInSignal dotlen freq rate code = gate (stretch dotlen rate code) (carrier freq rate)

-- | Apply post-processing to a signal including volume adjustment,
-- normalization, and band-pass filtering to reduce clicking artifacts.
--
-- Parameters:
--
--   * padding length in milliseconds
--   * center frequency for filtering
--   * sample rate
--   * input signal
postprocess :: Length -> Frequency -> Rate -> Signal -> Signal
postprocess padLen f rate = map (*0.5) -- Set volume
                   . normalise
                   -- Remove clicking:
                   . bandPass (frequencyScale rate (f-25)) (frequencyScale rate (f+25))
                   . pad padLen rate

-- | Convert a floating-point signal sample to a bounded integer representation.
-- Clips values to the valid range for the target type.
convertSample :: Double -> Int16
convertSample x
  | x > 1.0 = maxBound
  | x < -1.0 = minBound
  | otherwise = round (x * fromIntegral (maxBound :: Int16))

-- | Convert a floating-point signal to PCM format (16-bit integers).
encodePCM :: Signal -> PCM
encodePCM = map convertSample

-- The function should convert the PCM values to a normalized floating-point
-- signal in one step without a second normalization.
decodePCM :: PCM -> Signal
decodePCM pcm = normalise $ map fromIntegral pcm



-- | Add silent padding to both ends of a signal.
--
-- Parameters:
--
--   * padding length in milliseconds
--   * sample rate
--   * input signal
pad :: Length -> Rate -> Signal -> Signal
pad l r signal = padding <> signal <> padding
  where padding = replicate (l * r) 0

-- | Complete pipeline for encoding a text message to PCM audio.
--
-- Parameters:
--
--   * padding length in milliseconds
--   * dot length in milliseconds
--   * carrier frequency in Hz
--   * sample rate
--   * message text
outputPipeline :: Length -> Length -> Frequency -> Rate -> String -> PCM
outputPipeline padLen dotLen freq rate
                       = encodePCM
                       . postprocess padLen freq rate
                       . encodeInSignal dotLen freq rate
                       . toMorse

{-- Processing the input signal --}

-- | Calculate the median value of a list.
-- For even-length lists, returns the average of the two middle values.
median :: (Fractional a, Ord a) => [a] -> a
median [] = error "median of empty list"
median xs =
  let sorted = sort xs
      n = length sorted
      mid = n `div` 2
  in if odd n
     then sorted !! mid
     else (sorted !! (mid - 1) + sorted !! mid) / 2

{-- Filtering --}

-- | Apply convolution to a signal using a kernel.
convolve :: [Double] -> Signal -> Signal
convolve kernel signal =
  let k = length kernel
      padded = replicate (k `div` 2) 0 ++ signal ++ replicate (k `div` 2) 0
      convAt i = sum $ zipWith (*) kernel (take k (drop i padded))
  in map convAt [0..length signal - 1]

-- | Normalized sinc.
sinc :: Double -> Double
sinc 0 = 1
sinc x = sin (pi * x) / (pi * x)

-- | Generate a Hamming window of the specified length.
-- Used for windowing filter kernels to reduce ripple.
hamming :: Int -> [Double]
hamming n = [0.54 - 0.46 * cos (2 * pi * fromIntegral i / fromIntegral (n - 1)) | i <- [0..n-1]]

-- | Generate a low-pass filter kernel.
--
-- Parameters:
--
--   * kernel length
--   * cutoff frequency (scaled to samplingrate)
lowPassKernel :: Int -> Double -> [Double]
lowPassKernel n fc = zipWith (*) window kernel
  where
    kernel = [ 2 * fc * sinc (2 * fc * (fromIntegral i - fromIntegral (n - 1) / 2)) | i <- [0..(n - 1)] ]
    window = hamming n

-- | Apply a low-pass filter to a signal.
-- Removes frequency components above the cutoff frequency.
lowPass :: Frequency -> Signal -> Signal
lowPass cutoff = convolve (lowPassKernel 181 cutoff)

-- | Generate a band-pass filter kernel.
--
-- Parameters:
--
--   * kernel length
--   * low cutoff frequency (scaled to samplingrate)
--   * high cutoff frequency (scaled to samplingrate)
bandPassKernel :: Int -> Double -> Double -> [Double]
bandPassKernel n low high = zipWith (-) hp lp
  where
    hp = lowPassKernel n high
    lp = lowPassKernel n low

-- | Apply a high-pass filter to a signal.
-- Attenuates frequency components below the cutoff frequency.
highPass :: Frequency -> Signal -> Signal
highPass cutoff vs = zipWith (-) vs (lowPass cutoff vs)

-- | Apply a band-pass filter to a signal.
-- Lowers frequency components outside the band between the low and high cutoff frequencies.
bandPass :: Frequency -> Frequency -> Signal -> Signal
bandPass low high = convolve (bandPassKernel 181 low high)

-- | Normalize a signal to have maximum absolute value of 1.0.
normalise :: Signal -> Signal
normalise [] = []
normalise signal =
  let maxAbs = maximum (map abs signal)
  in if maxAbs == 0
     then signal
     else map (/ maxAbs) signal

-- | Pre-process an input signal for Morse decoding by applying
-- band-pass filtering around the carrier frequency.
--
-- Parameters:
--
--   * expected carrier frequency
--   * sample rate
--   * input signal
preprocess :: Frequency -> Rate -> Signal -> Signal
preprocess f rate = normalise . bandPass (frequencyScale rate (f+5)) (frequencyScale rate (f-5))

-- | Detect on/off gating in a signal by comparing the envelope
-- to a threshold. Returns a boolean signal indicating gate state.
--
-- Parameters:
--
--   * sample rate
--   * detection threshold (0 to 1)
--   * input signal
detectGate :: Rate -> Double -> Signal -> [Bool]
detectGate rate threshold signal =
  let rectified = map abs signal
      envelope = normalise $ lowPass (frequencyScale rate 26) rectified
  in map (>= threshold) envelope

-- | Determine the length of a "dit" (dot) from detected gate lengths
-- using statistical analysis.
--
-- Parameters:
--
--   * sensitivity factor (typically around 0.8-1.0)
--   * list of gate lengths
detectGateLength :: Double -> [Int] -> Int
detectGateLength sensitivity lengths
  | null lengths = 1
  | null candidates = 1
  | otherwise = minimum candidates
  where
    med = median (map fromIntegral lengths)
    threshold = sensitivity * med
    candidates = filter (\n -> fromIntegral n >= threshold) lengths



-- | Convert a gated signal to Morse symbols by analyzing the duration
-- of on and off periods.
--
-- Parameters:
--
--   * sample rate
--   * gate detection threshold
--   * sensitivity factor for timing detection
--   * input signal
tokenize :: Rate -> Double -> Double -> Signal -> [MSymbol]
tokenize rate threshold sensitivity signal = map toSymbol significantGates
  where toSymbol (True,n)  | n < 3 * ditLength = Dot
                           | otherwise         = Dash
        toSymbol (False,n) | n > 5*ditLength   = LongSpace
                           | n > 2*ditLength   = ShortSpace
                           | otherwise         = DivSpace

        gateSignal = detectGate rate threshold signal
        groupedGates = map (head &&& length) $ group gateSignal
        trues = map snd $ filter fst groupedGates
        ditLength = detectGateLength sensitivity trues
        significantGates = filter ((>=sensitivity*1.01*fromIntegral ditLength) . fromIntegral . snd) groupedGates

-- | Complete pipeline for decoding PCM audio to text message.
--
-- Parameters:
--
--   * expected carrier frequency in Hz
--   * sample rate
--   * PCM audio data
inputPipeline :: Frequency -> Rate -> PCM -> String
inputPipeline freq rate
              = map morseToChar
              . groupSymbols
              . tokenize rate 0.5 (5/6)
              . preprocess freq rate
              . decodePCM

{- PCM functions -}

-- | Write PCM data to a binary file.
-- The file format is raw 16-bit little-endian samples.
writePCM :: FilePath -> PCM -> IO ()
writePCM path pcm = withBinaryFile path WriteMode $ \h ->
  allocaArray (length pcm) $ \ptr -> do
    pokeArray ptr pcm
    hPutBuf h ptr $ 2 * length pcm

-- | Convert a list of bytes (little-endian) to 16-bit integers.
-- The input list must have even length.
packLE :: [Int8] -> [Int16]
packLE [] = []
packLE [_] = error "Can only deal with even lengthed lists"
packLE (lsb:msb:xs) = fromIntegral lsb + (2^8) * fromIntegral msb : packLE xs

-- | Read PCM data from a binary file.
-- The file format is raw 16-bit little-endian samples.
readPCM :: FilePath -> IO PCM
readPCM path = withBinaryFile path ReadMode $ \h -> fix $ \loop -> do
  (acc,l) <- allocaArray 2048 $ \ptr -> do
    l <- hGetBuf h ptr 2048
    got <- packLE <$> peekArray l ptr
    return (got,l)
  if l < 2048
    then return acc
    else do
      rest <- loop
      return $ acc ++ rest

-- Interactive helpers and main

-- | Display a prompt and read a line of user input.
prompt :: String -> IO String
prompt msg = putStr msg >> hFlush stdout >> getLine

-- | Parse a string with a default value if parsing fails.
readDef :: Read a => a -> String -> a
readDef def = fromMaybe def . readMaybe

-- | Prompt for user input and parse it, using a default if parsing fails.
promptRead :: Read a => String -> a -> IO a
promptRead msg def = do
  s <- prompt msg
  pure (readDef def s)

-- | Interactive flow for encoding a message to PCM file.
encodeFlow :: Frequency -> Rate -> IO ()
encodeFlow freq rate = do
  outPath <- prompt "Output .pcm file path: "
  msg     <- prompt "Message to encode: "
  ditMs   <- promptRead "Dit length in ms [75]: " (75 :: Int)
  padMs   <- promptRead "Pad (silence) in ms at both ends [30]: " (30 :: Int)
  let pcm = outputPipeline padMs ditMs freq rate msg
  writePCM outPath pcm
  putStrLn $ "Wrote " <> show (length pcm) <> " samples to " <> outPath

-- | Interactive flow for decoding a PCM file to text.
decodeFlow :: Frequency -> Rate -> IO ()
decodeFlow freq rate = do
  inPath <- prompt "Input .pcm file path: "
  pcm    <- readPCM inPath
  let msg = inputPipeline freq rate pcm
  putStrLn "Decoded message:"
  putStrLn msg

-- | Main entry point providing an interactive interface for
-- encoding and decoding Morse code audio files.
main :: IO ()
main = do
  putStrLn "Morse↔PCM interface"
  freq <- promptRead "Carrier frequency in Hz [375]: " (375 :: Double)
  rate <- promptRead "Sample rate in kHz [8kHz]: " (8 :: Int)
  mode <- prompt "Write (w) message to .pcm, or Read (r) from .pcm? [w/r]: "
  case map toLower mode of
    ('r':_) -> decodeFlow freq rate
    ('w':_) -> encodeFlow freq rate
    _ -> putStrLn "Invalid response."