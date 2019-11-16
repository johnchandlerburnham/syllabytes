module Syllabytes where

import           Data.Bits
import           Data.Char
import           Numeric
import Text.ParserCombinators.ReadP

main :: IO ()
main = do
  traverse (\x -> putStrLn $ concat ["0x",showHex x "",": ", syllabytes x]) d8
  traverse (\x -> putStrLn $ concat ["0x",showHex x "",": ", syllabytes x]) d40
  return ()

syll :: [String]
syll = [  "a",  "an",  "i",  "in",  "u",  "un",  "e",  "en"
         ,  "o",  "on",  "ya",  "yan",  "yu",  "yun",  "yo",  "yon"
       , "ka", "kan", "ki", "kin", "ku", "kun", "ke", "ken"
         , "ko", "kon", "kya", "kyan", "kyu", "kyun", "kyo", "kyon"
       , "sa", "san", "shi", "shin", "su", "sun", "se", "sen"
         , "so", "son", "sha", "shan", "shu", "shun", "sho", "shon"
       , "ta", "tan", "chi", "chin", "tsu", "tsun", "te", "ten"
         ,"to", "ton", "cha", "chan", "chu", "chun", "cho", "chon"
       , "na", "nan", "ni", "nin", "nu", "nun", "ne", "nen"
         , "no", "non", "nya", "nyan", "nyu", "nyun", "nyo", "nyon"
       , "ha", "han", "hi", "hin", "hu", "hun", "he", "hen"
         , "ho", "hon", "hya", "hyan", "hyu", "hyun", "hyo", "hyon"
       , "fa", "fan", "fi", "fin", "fu", "fun", "fe", "fen"
         , "fo", "fon", "fya", "fyan", "fyu", "fyun", "fyo", "fyon"
       , "ma", "man", "mi", "min", "mu", "mun", "me", "men"
         , "mo", "mon", "mya", "myan", "myu", "myun", "myo", "myon"
       , "ra", "ran", "ri", "rin", "ru", "run", "re", "ren"
         , "ro", "ron", "rya", "ryan", "ryu", "ryun", "ryo", "ryon"
       , "ga", "gan", "gi", "gin", "gu", "gun", "ge", "gen"
         , "go", "gon", "gya", "gyan", "gyu", "gyun", "gyo", "gyon"
       , "za", "zan", "zi", "zin", "zu", "zun", "ze", "zen"
         , "zo", "zon", "zya", "zyan", "zyu", "zyun", "zyo", "zyon"
       , "da", "dan", "di", "din", "du", "dun", "de", "den"
         , "do", "don", "dya", "dyan", "dyu", "dyun", "dyo", "dyon"
       , "ja", "jan", "ji", "jin", "ju", "jun", "je", "jen"
         , "jo", "jon", "jya", "jyan", "jyu", "jyun", "jyo", "jyon"
       , "ba", "ban", "bi", "bin", "bu", "bun", "be", "ben"
         , "bo", "bon", "bya", "byan", "byu", "byun", "byo", "byon"
       , "pa", "pan", "pi", "pin", "pu", "pun", "pe", "pen"
         , "po", "pon", "pya", "pyan", "pyu", "pyun", "pyo", "pyon"
       , "wa", "wan", "wi", "win", "wu", "wun", "we", "wen"
         , "wo", "won", "wya", "wyan", "wu", "wyun", "wyo", "wyon"
       ]

read_syll :: [ReadP Integer]
read_syll = (\(x,y) -> string x >> pure y) <$> (zip syll [0..256])

read_syllabytes :: ReadP [Integer]
read_syllabytes = many $ choice read_syll

from_syllabytes :: String -> Integer
from_syllabytes = go . fst . head . reverse . readP_to_S read_syllabytes
  where
    go = int_from_bytes . reverse

int_from_bytes :: [Integer] -> Integer
int_from_bytes (x:[]) = x
int_from_bytes (x:xs) = x + (lsh (int_from_bytes xs) 8)

showBin :: (Show a, Integral a) => a -> String
showBin a = showIntAtBase 2 intToDigit a ""

lsh :: Integer -> Int -> Integer
lsh x n = shift x n

rsh :: Integer -> Int -> Integer
rsh x n = shift x (negate n)

syllabytes n = syllabyte_go n False

syllabyte_go :: Integer -> Bool -> String
syllabyte_go n b
  | n == 0 = ""
  | otherwise = syllabyte_go (n `rsh` 8) (not b) ++ (syll !! x)
  where
    x = fromIntegral $ n .&. (2^8 - 1)

d40 = [ 0x334820d375f6b485a034911a386644faf7d9b259
      , 0x84295d5e054d8cff5a22428b195f5a1615bd644f
      , 0x1695740538b7775067f900d859c4b49ce3bf529d
      , 0xcad9786d1fab8148dc0ec65a33737174809c0b60
      , 0x1ce108a850e54a67f2fc5d9732e8ced0774b0ba7
      , 0x6c45790549bf78810c06ccfadd208cd2df3b1c4c
      , 0x31b6002a767e1001676797587cf6b09a59e0de1f
      , 0x85ec0c993ce97e59f1fdc8f452e30f95b09159b8
      , 0x60daeb00911f3bbcd305f238ab918b15e0830502
      , 0xab987019b810b8e354a2625b63e19b11ee5f8ceb
      , 0x07b7044cca816afe46aba54bc30f3eb5bdb71f81
      , 0xba53fd4e4ded26a8a0bb754bd224f413ca62e025
      , 0x133c10f83b82ba39bb4c3d6717f0bb6327432251
      , 0x0c61b2a1193f5ab7e993e6e3e8467a1e311f6474
      , 0xaf9092dbf43eea5d0b59976c019ec4cd9b683331
      , 0xf8a0b1a00b3e9e2ad255f9b216ee3e4ee2549d00
      , 0xd0a98ff68f2ca6b178fb35427263136021c194f2
      , 0xed1271fcc2dd24e7dd07318b2b1c274ab2509e0c
      , 0x7b3b026e9c5c08df78db132c060ff75b9aa9bdb8
      , 0xea5548b75981e03e88ddf3cb4f79b5b1f86e6a66
      , 0x539bf144128ea9c6289bb6944c9a4c55e530e328
      , 0xc76a3a89b175caa59bac794cfe3fbf35e63a32a7
      ]

d8 = [ 0x00bc6139d3
     , 0x0086d5bc93
     , 0x00a30f81c2
     , 0x002e82536b
     , 0x0095a2ed91
     , 0x00dde02ff4
     , 0x00738f69a4
     , 0x0016810a3d
     , 0x0034139975
     , 0x00515d1d2b
     , 0x00964db755
     , 0x00feafb260
     , 0x004875ec33
     , 0x00eb70a4f9
     , 0x00e9f502cd
     , 0x00e9fe9e16
     , 0x00e68bf5f2
     , 0x0020df1f9e
     , 0x00def3d52f
     , 0x0039b80401
     ]

