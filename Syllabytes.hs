module Syllabytes where

import           Data.Bits
import           Data.Char
import           Data.List
import           Numeric
import Text.ParserCombinators.ReadP

main :: IO ()
main = do
  traverse test_syllabytes d8
  traverse test_syllabytes d40
  return ()

print_syllabytes :: Integer -> IO ()
print_syllabytes n = do
  putStrLn $ concat 
    ["0x",showHex n "",":\n  "
    , syllabytes n, "\n  "
    , syllabytes_jp n, "\n"
    ]

test_syllabytes :: Integer -> IO ()
test_syllabytes n = do
  let s = syllabytes n
  let n' = from_syllabytes s
  let s' = syllabytes n'
  let sj = syllabytes_jp n
  let nj' = from_syllabytes_jp sj
  let sj' = syllabytes_jp nj'
  if n  == n' &&
     s  == s' &&
     sj == sj' &&
     n == nj'
  then
    print_syllabytes n
  else
   putStrLn $ concat
    ["BAD:\n"
    ,"  n =  0x", showHex n "","\n"
    ,"  syllabytes(n) = ", s,"\n"
    ,"  syllabytes_jp(n) = ", sj, "\n"
    ,"  from_syllabytes(syllabytes(n)) = 0x", showHex n' "", "\n"
    ,"  syllabytes(from_syllabytes(syllabytes(n)) = ", s', "\n"
    ,"  from_syllabytes_jp(syllabytes_jp(n)) = 0x", showHex nj' "", "\n"
    ,"  syllabytes_jp(from_syllabytes_jp(syllabytes_jp(n)) = ", s', "\n"
    ]

syll :: [String]
syll = riffle syll0 syll1

syll_jp :: [String]
syll_jp = riffle syll_jp0 syll_jp1

riffle :: [a] -> [a] -> [a]
riffle [] _ = []
riffle (x:xs) (y:ys) = x:y:(riffle xs ys)

syll0 =
  [  "a",  "i",   "u",   "e",  "o",  "ya",  "yu",  "yo"
  , "ka",  "ki",  "ku", "ke", "ko", "kya", "kyu", "kyo"
  , "sa", "shi",  "su", "se", "so", "sha", "shu", "sho"
  , "ta", "chi", "tsu", "te", "to", "cha", "chu", "cho"
  , "na",  "ni",  "nu", "ne", "no", "nya", "nyu", "nyo"
  , "ha",  "hi",  "hu", "he", "ho", "hya", "hyu", "hyo"
  , "fa",  "fi",  "fu", "fe", "fo", "fya", "fyu", "fyo"
  , "ma",  "mi",  "mu", "me", "mo", "mya", "myu", "myo"
  , "ra",  "ri",  "ru", "re", "ro", "rya", "ryu", "ryo"
  , "ga",  "gi",  "gu", "ge", "go", "gya", "gyu", "gyo"
  , "za",  "zi",  "zu", "ze", "zo",  "ja",  "ju",  "jo"
  , "da",  "di",  "du", "de", "do", "dya", "dyu", "dyo"
  , "ba",  "bi",  "bu", "be", "bo", "bya", "byu", "byo"
  , "pa",  "pi",  "pu", "pe", "po", "pya", "pyu", "pyo"
  , "wa",  "wi",  "wu", "we", "wo", "wya", "wyu", "wyo"
  , "va",  "vi",  "vu", "ve", "vo", "vya", "vyu", "vyo"
  ]

syll1 =
  [  "an",  "in",    "un",  "en",  "on",  "yan",  "yun",  "yon"
  , "kan",  "kin",  "kun", "ken", "kon", "kyan", "kyun", "kyon"
  , "san", "shin",  "sun", "sen", "son", "shan", "shun", "shon"
  , "tan", "chin", "tsun", "ten", "ton", "chan", "chun", "chon"
  , "nan",  "nin",  "nun", "nen", "non", "nyan", "nyun", "nyon"
  , "han",  "hin",  "hun", "hen", "hon", "hyan", "hyun", "hyon"
  , "fan",  "fin",  "fun", "fen", "fon", "fyan", "fyun", "fyon"
  , "man",  "min",  "mun", "men", "mon", "myan", "myun", "myon"
  , "ran",  "rin",  "run", "ren", "ron", "ryan", "ryun", "ryon"
  , "gan",  "gin",  "gun", "gen", "gon", "gyan", "gyun", "gyon"
  , "zan",  "zin",  "zun", "zen", "zon",  "jan",  "jun", "jon"
  , "dan",  "din",  "dun", "den", "don", "dyan", "dyun", "dyon"
  , "ban",  "bin",  "bun", "ben", "bon", "byan", "byun", "byon"
  , "pan",  "pin",  "pun", "pen", "pon", "pyan", "pyun", "pyon"
  , "wan",  "win",  "wun", "wen", "won", "wyan", "wyun", "wyon"
  , "van",  "vin",  "vun", "ven", "von", "vyan", "vyun", "vyon"
  ]

syll_jp0 =
  [  "ア",  "イ",    "ウ",  "エ",  "オ",  "ヤ",  "ユ",  "ヨ"
  ,  "カ",  "キ",    "ク",  "ケ",  "コ","キャ","キュ","キョ"
  ,  "サ",  "シ",    "ス",  "セ",  "ソ","シャ","シュ","ショ"
  ,  "タ",  "チ",    "ツ",  "テ",  "ト","チャ","チュ","チョ"
  ,  "ナ",  "ニ",    "ヌ",  "ネ",  "ノ","ニャ","ニュ","ニョ"
  ,  "ハ",  "ヒ",  "ホゥ",  "ヘ",  "ホ","ヒャ","ヒュ","ヒョ"
  ,"ファ","フィ",    "フ","フェ","フォ","フャ","フュ","フョ"
  ,  "マ",  "ミ",    "ム",  "メ",  "モ","ミャ","ミュ","ミョ"
  ,  "ラ",  "リ",    "ル",  "レ",  "ロ","リャ","リュ","リョ"
  ,  "ガ",  "ギ",    "グ",  "ゲ",  "ゴ","ギャ","ギュ","ギョ"
  ,  "ザ",  "ジ",    "ズ",  "ゼ",  "ゾ","ジャ","ジュ","ジョ"
  ,  "ダ",  "ヂ",    "ヅ",  "デ",  "ド","ヂャ","ヂュ","ヂョ"
  ,  "バ",  "ビ",    "ブ",  "ベ",  "ボ","ビャ","ビュ","ビョ"
  ,  "パ",  "ピ",    "プ",  "ペ",  "ポ","ピャ","ピュ","ピョ"
  ,  "ワ",  "ヰ",  "ウゥ",  "ヱ",  "ヲ","ウャ","ウュ","ウョ"
  ,  "ヷ",  "ヸ",    "ヴ",  "ヹ",  "ヺ","ヴャ","ヴュ","ヴョ"
  ]

syll_jp1 =
  [  "アン",  "イン",    "ウン",  "エン",  "オン",  "ヤン",  "ユン",  "ヨン"
  ,  "カン",  "キン",    "クン",  "ケン",  "コン","キャン","キュン","キョン"
  ,  "サン",  "シン",    "スン",  "セン",  "ソン","シャン","シュン","ション"
  ,  "タン",  "チン",    "ツン",  "テン",  "トン","チャン","チュン","チョン"
  ,  "ナン",  "ニン",    "ヌン",  "ネン",  "ノン","ニャン","ニュン","ニョン"
  ,  "ハン",  "ヒン",  "ホゥン",  "ヘン",  "ホン","ヒャン","ヒュン","ヒョン"
  ,"ファン","フィン",    "フン","フェン","フォン","フャン","フュン","フョン"
  ,  "マン",  "ミン",    "ムン",  "メン",  "モン","ミャン","ミュン","ミョン"
  ,  "ラン",  "リン",    "ルン",  "レン",  "ロン","リャン","リュン","リョン"
  ,  "ガン",  "ギン",    "グン",  "ゲン",  "ゴン","ギャン","ギュン","ギョン"
  ,  "ザン",  "ジン",    "ズン",  "ゼン",  "ゾン","ジャン","ジュン","ジョン"
  ,  "ダン",  "ヂン",    "ヅン",  "デン",  "ドン","ヂャン","ヂュン","ヂョン"
  ,  "バン",  "ビン",    "ブン",  "ベン",  "ボン","ビャン","ビュン","ビョン"
  ,  "パン",  "ピン",    "プン",  "ペン",  "ポン","ピャン","ピュン","ピョン"
  ,  "ワン",  "ヰン",  "ウゥン",  "ヱン",  "ヲン","ウャン","ウュン","ウョン"
  ,  "ヷン",  "ヸン",    "ヴン",  "ヹン",  "ヺン","ヴャン","ヴュン","ヴョン"
  ]


showBin :: (Show a, Integral a) => a -> String
showBin a = showIntAtBase 2 intToDigit a ""

lsh :: Int -> Integer -> Integer
lsh n x = shift x n

rsh :: Int -> Integer -> Integer
rsh n x = shift x (negate n)

from_bytes :: [Int] -> Integer
from_bytes (x:[]) = fromIntegral $ x
from_bytes (x:xs) = fromIntegral x + (lsh 8 (from_bytes xs))

to_bytes :: Integer -> [Int]
to_bytes n
  | n == 0 = [0]
  | n < 256 = [fromIntegral n]
  | otherwise = (fromIntegral $ n .&. (2^8 - 1)) : (to_bytes (rsh 8 n))

isVowel :: Int -> Bool
isVowel n = n >= 0 && n <= 16

isN :: Int -> Bool
isN n = n `mod` 2 == 1

syllabytes :: Integer -> String
syllabytes n = go bytes ""
  where
    bytes = to_bytes n
    go :: [Int] -> String -> String
    go (a:b:xs) s
      | isVowel a && isN b = go (b:xs) ("'" ++ (syll !! a) ++ s)
      | otherwise = go (b:xs) ((syll !! a) ++ s)
    go (a:[]) s = (syll !! a) ++ s
    go [] s = s

syllabytes_jp :: Integer -> String
syllabytes_jp n = go bytes ""
  where
    bytes = to_bytes n
    go :: [Int] -> String -> String
    go (a:b:xs) s
      | isVowel a && isN b = go (b:xs) ((syll_jp !! a) ++ s)
      | otherwise = go (b:xs) ((syll_jp !! a) ++ s)
    go (a:[]) s = (syll_jp !! a) ++ s
    go [] s = s

split :: (Char -> Bool) -> String -> [String]
split f s = lines $ (\x -> if f x then '\n' else x) <$> s

sanitize :: String -> [String]
sanitize s = filter (\x -> x /= "") $ split (not . isAsciiLower) (toLower <$> s)

read_syll :: [ReadP Int]
read_syll = (\(x,y) -> string x >> pure y) <$> (zip syll [0..256])

read_syllabytes :: ReadP [Int]
read_syllabytes = (many $ choice $ reverse $ read_syll)

from_syllabytes :: String -> Integer
from_syllabytes s = from_bytes $ reverse $ concatMap id $ parse <$> (sanitize s)
  where
    parse = fst .last . readP_to_S read_syllabytes

read_syll_jp :: [ReadP Int]
read_syll_jp = (\(x,y) -> string x >> pure y) <$> (zip syll_jp [0..256])

read_syllabytes_jp :: ReadP [Int]
read_syllabytes_jp = (many $ choice $ reverse $ read_syll_jp)

from_syllabytes_jp :: String -> Integer
from_syllabytes_jp s = from_bytes $ reverse $ parse s
  where
    parse = fst .last . readP_to_S read_syllabytes_jp

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

