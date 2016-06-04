module Main where

import           Data.Maybe
import           Control.Monad
import           Haste (JSType)
import           Haste.App (MonadIO)
import           Haste.DOM (Elem)
import qualified Haste as H
import qualified Haste.DOM as HD
import qualified Haste.Events as HE

-- Consts

expMaterialHomo =
  [80, 240, 600, 1600, 6400]

expMaterialHetero =
  map (`div`4) expMaterialHomo

expFamiliar =
  [690, 2798, 6386, 11958, 20961, 32000, 64000, 128000]

maxLvFamiliar =
  [30, 40, 50, 60, 70, 60, 70, 80]

medalRateMaterial =
  [1.0, 2.0, 4.0, 8.0, 2.5]

medalRateFamiliar =
  [[0.8, 1.0, 1.2],
  [1.0, 1.2, 1.4],
  [1.6, 1.8, 2.0],
  [2.0, 2.4, 3.0],
  [3.0, 3.6, 4.0],
  [0.0, 0.0, 0.0],
  [0.0, 0.0, 0.0],
  [0.0, 0.0, 0.0]]

familiarElems =
  ["familiarRarity",
   "familiarEvolution",
   "familiarCurrentLv",
   "familiarCurrentExp"]

materialElems =
  ["materialHomo1",
   "materialHomo2",
   "materialHomo3",
   "materialHomo4",
   "materialHomo5",
   "materialHetero1",
   "materialHetero2",
   "materialHetero3",
   "materialHetero4",
   "materialHetero5"]

resultElems =
  ["familiarLv",
   "familiarMaxRemains",
   "compositionCost",
   "gainedExp"] 

clearElems = ["familiarClear", "materialClear"]

-- IO Actions

main = do
  HD.withElems (familiarElems++materialElems) calc
  HD.withElems clearElems clear

clear [fc, mc] = do
  HE.onEvent fc HE.Click $ const familiarClear
  HE.onEvent mc HE.Click $ const materialClear

familiarClear = do
  [frareE, fevoE, fclvE, fexpE] <- map fromJust <$> forM familiarElems HD.elemById
  HD.setProp frareE "value"   "0"
  HD.setProp fevoE  "checked" "true"
  HD.setProp fclvE  "value"   "1"
  HD.setProp fexpE  "value"   "0"
  recalc

materialClear = do
  m <- map fromJust <$> forM materialElems HD.elemById
  forM_ m $ \x -> HD.setProp x "value" "0"
  recalc

calc es = forM es $ \x -> HE.onEvent x HE.Change $ const recalc

recalc :: MonadIO m => m ()
recalc = do
  familiarE <- elemsFromList familiarElems
  materialE <- elemsFromList materialElems
  [flvE, fmrE, costE, gainE] <- elemsFromList resultElems
  [frare, fevo, fclv, fcexp] <- valuesFromList familiarE
  mhomo   <- valuesFromList $ take 5 materialE
  mhetero <- valuesFromList $ drop 5 materialE
  let gain = gainedExp mhomo mhetero
  HD.setProp flvE  "innerHTML" (H.toString $ familiarLv frare fclv fcexp gain)
  HD.setProp fmrE  "innerHTML" (H.toString $ familiarMaxRemains frare fclv fcexp gain)
  HD.setProp gainE "innerHTML" (H.toString gain)

-- Calc

familiarLv :: Int -> Int -> Int -> Int -> Int
familiarLv rare clv cexp gain =
  expToLv rare $ currentSumExp rare clv cexp + gain

familiarMaxRemains :: Int -> Int -> Int -> Int -> Int
familiarMaxRemains rare clv cexp gain =
  expFamiliar!!rare - currentSumExp rare clv cexp - gain

-- compositionCost
onceCost rare lv evo mrare =
  (100+3*(lv-1))*(medalRateFamiliar!!rare!!evo)*(medalRateMaterial!!mrare)

gainedExp :: [Int] -> [Int] -> Int
gainedExp mhomo mhetero =
  sum
  $ zipWith (*) mhomo expMaterialHomo
  ++ zipWith (*) mhetero expMaterialHetero

-- Utils

elemsFromList :: MonadIO m => [String] -> m [Elem]
elemsFromList xs = map fromJust <$> forM xs HD.elemById

valuesFromList :: (MonadIO m, JSType a) => [Elem] -> m [a]
valuesFromList xs = map fromJust <$> forM xs HD.getValue 

currentSumExp r n e = exp1ToN r n + expKToK1 r n * e `div` 100

exp1ToN r n = 
  round'
  $ fromIntegral(expFamiliar!!r)
  * (fromIntegral(n-1)/fromIntegral(maxLvFamiliar!!r-1))**2.3

expKToK1 r k = exp1ToN r (k+1) - exp1ToN r k

expToLv 0 0 = 1
expToLv 0 1 = 2
expToLv r e =
  length . takeWhile (<=e) $ map (exp1ToN r) [1..]

round' x = 
  if x - fromIntegral(floor x) < 0.5
  then floor x else ceiling x