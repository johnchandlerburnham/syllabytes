//const syllabytes = {
//  // 0000 : ""
//  "00": "a",
//  "01": "an",
//  "02": "i",
//  "03": "in",
//  "04": "u",
//  "05": "un",
//  "06": "e",
//  "07": "en",
//  "08": "o",
//  "09": "on",
//  "0a": "ya",
//  "0b": "yan",
//  "0c": "yu",
//  "0d": "yun",
//  "0e": "yo",
//  "0f": "yon",
//  // 0001 : "k"
//  "10": "ka",
//  "11": "kan",
//  "12": "ki",
//  "13": "kin",
//  "14": "ku",
//  "15": "kun",
//  "16": "ke",
//  "17": "ken",
//  "18": "ko",
//  "19": "kon",
//  "1a": "kya",
//  "1b": "kyan",
//  "1c": "kyu",
//  "1d": "kyun",
//  "1e": "kyo",
//  "1f": "kyon",
//  // 0010 : "s"
//  "20": "sa",
//  "21": "san",
//  "22": "shi",
//  "23": "shin",
//  "24": "su",
//  "25": "sun",
//  "26": "se",
//  "27": "sen",
//  "28": "so",
//  "29": "son",
//  "2a": "sya",
//  "2b": "syan",
//  "2c": "syu",
//  "2d": "syun",
//  "2e": "syo",
//  "2f": "syon",
//  // 0011 : "t"
//  "30": "ta",
//  "31": "tan",
//  "32": "chi",
//  "33": "chin",
//  "34": "tsu",
//  "35": "tsun",
//  "36": "te",
//  "37": "ten",
//  "38": "to",
//  "39": "ton",
//  "3a": "cha",
//  "3b": "chan",
//  "3c": "chu",
//  "3d": "chun",
//  "3e": "cho",
//  "3f": "chon",
//  // 0100 : "n"
//  "40": "na",
//  "41": "nan",
//  "42": "ni",
//  "43": "nin",
//  "44": "nu",
//  "45": "nun",
//  "46": "ne",
//  "47": "nen",
//  "48": "no",
//  "49": "non",
//  "4a": "nya",
//  "4b": "nyan",
//  "4c": "nyu",
//  "4d": "nyun",
//  "4e": "nyo",
//  "4f": "nyon",
//  // 0101 : "h"
//  "50": "ha",
//  "51": "han",
//  "52": "hi",
//  "53": "hin",
//  "54": "hu",
//  "55": "hun",
//  "56": "he",
//  "57": "hen",
//  "58": "ho",
//  "59": "hon",
//  "5a": "hya",
//  "5b": "hyan",
//  "5c": "hyu",
//  "5d": "hyun",
//  "5e": "hyo",
//  "5f": "hyon",
//  // 0110 : "f"
//  "60": "fa",
//  "61": "fan",
//  "62": "fi",
//  "63": "fin",
//  "64": "fu",
//  "65": "fun",
//  "66": "fe",
//  "67": "fen",
//  "68": "fo",
//  "69": "fon",
//  "6a": "fya",
//  "6b": "fyan",
//  "6c": "fyu",
//  "6d": "fyun",
//  "6e": "fyo",
//  "6f": "fyon",
//  // 0111 : "m"
//  "70": "ma",
//  "71": "man",
//  "72": "mi",
//  "73": "min",
//  "74": "mu",
//  "75": "mun",
//  "76": "me",
//  "77": "men",
//  "78": "mo",
//  "79": "mon",
//  "7a": "mya",
//  "7b": "myan",
//  "7c": "myu",
//  "7d": "myun",
//  "7e": "myo",
//  "7f": "myon",
// // 1000 : "r"
//  "80": "ra",
//  "81": "ran",
//  "82": "ri",
//  "83": "rin",
//  "84": "ru",
//  "85": "run",
//  "86": "re",
//  "87": "ren",
//  "88": "ro",
//  "89": "ron",
//  "8a": "rya",
//  "8b": "ryan",
//  "8c": "ryu",
//  "8d": "ryun",
//  "8e": "ryo",
//  "8f": "ryon",
//  // 1001 : "g"
//  "90": "ga",
//  "91": "gan",
//  "92": "gi",
//  "93": "gin",
//  "94": "gu",
//  "95": "gun",
//  "96": "ge",
//  "97": "gen",
//  "98": "go",
//  "99": "gon",
//  "9a": "gya",
//  "9b": "gyan",
//  "9c": "gyu",
//  "9d": "gyun",
//  "9e": "gyo",
//  "9f": "gyon",
//  // 1010 : "z"
//  "a0": "za",
//  "a1": "zan",
//  "a2": "zi",
//  "a3": "zin",
//  "a4": "zu",
//  "a5": "zun",
//  "a6": "ze",
//  "a7": "zen",
//  "a8": "zo",
//  "a9": "zon",
//  "aa": "zya",
//  "ab": "zyan",
//  "ac": "zyu",
//  "ad": "zyun",
//  "ae": "zyo",
//  "af": "zyon",
//  // 1011 : "d"
//  "b0": "da",
//  "b1": "dan",
//  "b2": "di",
//  "b3": "din",
//  "b4": "du",
//  "b5": "dun",
//  "b6": "de",
//  "b7": "den",
//  "b8": "do",
//  "b9": "don",
//  "ba": "dya",
//  "bb": "dyan",
//  "bc": "dyu",
//  "bd": "dyun",
//  "be": "dyo",
//  "bf": "dyon",
//  // 1100 : "j"
//  "c0": "ja",
//  "c1": "jan",
//  "c2": "ji",
//  "c3": "jin",
//  "c4": "ju",
//  "c5": "jun",
//  "c6": "je",
//  "c7": "jen",
//  "c8": "jo",
//  "c9": "jon",
//  "ca": "jya",
//  "cb": "jyan",
//  "cc": "jyu",
//  "cd": "jyun",
//  "ce": "jyo",
//  "cf": "jyon",
//  // 1101 : "b"
//  "d0": "ba",
//  "d1": "ban",
//  "d2": "bi",
//  "d3": "bin",
//  "d4": "bu",
//  "d5": "bun",
//  "d6": "be",
//  "d7": "ben",
//  "d8": "bo",
//  "d9": "bon",
//  "da": "bya",
//  "db": "byan",
//  "dc": "byu",
//  "dd": "byun",
//  "de": "byo",
//  "df": "byon",
//  // 1110 : "p"
//  "e0": "pa",
//  "e1": "pan",
//  "e2": "pi",
//  "e3": "pin",
//  "e4": "pu",
//  "e5": "pun",
//  "e6": "pe",
//  "e7": "pen",
//  "e8": "po",
//  "e9": "pon",
//  "ea": "pya",
//  "eb": "pyan",
//  "ec": "pyu",
//  "ed": "pyun",
//  "ee": "pyo",
//  "ef": "pyon",
//  // 1111 : "w"
//  "f0": "wa",
//  "f1": "wan",
//  "f2": "wi",
//  "f3": "win",
//  "f4": "wu",
//  "f5": "wun",
//  "f6": "we",
//  "f7": "wen",
//  "f8": "wo",
//  "f9": "won",
//  "fa": "wya",
//  "fb": "wyan",
//  "fc": "wyu",
//  "fd": "wyun",
//  "fe": "wyo",
//  "ff": "wyon",
//};
//
////console.log(syllabytes);
//
//for (var key in syllabytes) {
//  syllabytes[syllabytes[key]] = key;
//}
//
////console.log(syllabytes);
//
//const hexToSyllabytes = (hex) => {
//  var bits = hex;
//
//  while (bits.length % 2 !== 0) {
//    bits = "0" + bits;
//  }
//  console.log(bits);
//
//  var syll = "";
//
//  for (var i = 0; i < hex.length / 2; ++i) {
//    syll += syllabytes[bits.slice(i * 2 , i * 2 + 2)];
//  }
//
//  return syll;
//};
//
//console.log(hexToSyllabytes("52809100"));
//console.log(hexToSyllabytes("0"));
//
//module.exports = {hexToSyllabytes};
