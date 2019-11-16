# Syllabytes
A Syllabic Byte Representation inspired by Japanese Kana (仮名)

Syllabytes is designed to be a pronounceable format for binary data which
maximizes human meaningfulness. We take advantage of the fact that Japanese
[gairango](https://en.wikipedia.org/wiki/List_of_gairaigo_and_wasei-eigo_terms)
already compress many non-Japanese words to a well-behaved and easily
pronounceable syllabary.

For example:

```haskell
*Syllabytes> showHex (from_syllabytes "hiragana") ""
"52809100"
*Syllabytes> showHex (from_syllabytes "katakana") ""
"10301100"
*Syllabytes> syllabytes 0x52809100
"hiragana"
*Syllabytes> syllabytes 0x10301100
"katakana"
```

And for [a fun example](https://www.youtube.com/watch?v=nGM375qYhN0):

```
*Syllabytes> syllabytes 0xc49674c49674981908248212861002c08224029f0824029e70358002703464800270341506843818890224743818880ad48018c308d48018c2e002e8e002e8e002e9082c83912c8391489483b0029483b00308e9e818e308e9e81901083e1c7603083e2416

"jugemujugemugokonosurikirekaijarisuigyonosuigyomatsunraimatsufuraimatsukunerutokoronisumutokoroyaburakojinoburakojipaipopaipopaiponoshuringanshuringannogurindaigurindainoponpokopinoponpokonanochokyumeinochosuke"
```

## Bytes

Even parity:

 | 0x | 0  | 2   | 4   | 6  | 8  | A   | C   | E   |
 |----|----|-----|-----|----|----|-----|-----|-----|
 | 0  | a  | i   | u   | e  | o  | ya  | yu  | yo  |
 | 1  | ka | ki  | ku  | ke | ko | kya | kyu | kyo |
 | 2  | sa | shi | su  | se | so | sha | shu | sho |
 | 3  | ta | chi | tsu | te | to | cha | chu | cho |
 | 4  | na | ni  | nu  | ne | no | nya | nyu | nyo |
 | 5  | ha | hi  | hu  | he | ho | hya | hyu | hyo |
 | 6  | fa | fi  | fu  | fe | fo | fya | fyu | fyo |
 | 7  | ma | mi  | mu  | me | mo | mya | myu | myo |
 | 8  | ra | ri  | ru  | re | ro | rya | ryu | ryo |
 | 9  | ga | gi  | gu  | ge | go | gya | gyu | gyo |
 | a  | za | zi  | zu  | ze | zo | zya | zyu | zyo |
 | b  | da | di  | du  | de | do | dya | dyu | dyo |
 | c  | ja | ji  | ju  | je | jo | jya | jyu | jyo |
 | d  | ba | bi  | bu  | be | bo | bya | byu | byo |
 | e  | pa | pi  | pu  | pe | po | pya | pyu | pyo |
 | f  | wa | wi  | wu  | we | wo | wya | wyu | wyo |

Odd parity:

 | 0x | 1   | 3    | 5    | 7   | 9   | B    | D    | F    |
 |----|-----|------|------|-----|-----|------|------|------|
 | 0  | an  | in   | un   | en  | on  | yan  | yun  | yon  |
 | 1  | kan | kin  | kun  | ken | kon | kyan | kyun | kyon |
 | 2  | san | shin | sun  | sen | son | shan | shun | shon |
 | 3  | tan | chin | tsun | ten | ton | chan | chun | chon |
 | 4  | nan | nin  | nun  | nen | non | nyan | nyun | nyon |
 | 5  | han | hin  | hun  | hen | hon | hyan | hyun | hyon |
 | 6  | fan | fin  | fun  | fen | fon | fyan | fyun | fyon |
 | 7  | man | min  | mun  | men | mon | myan | myun | myon |
 | 8  | ran | rin  | run  | ren | ron | ryan | ryun | ryon |
 | 9  | gan | gin  | gun  | gen | gon | gyan | gyun | gyon |
 | a  | zan | zin  | zun  | zen | zon | zyan | zyun | zyon |
 | b  | dan | din  | dun  | den | don | dyan | dyun | dyon |
 | c  | jan | jin  | jun  | jen | jon | jyan | jyun | jyon |
 | d  | ban | bin  | bun  | ben | bon | byan | byun | byon |
 | e  | pan | pin  | pun  | pen | pon | pyan | pyun | pyon |
 | f  | wan | win  | wun  | wen | won | wyan | wyun | wyon |

## More Examples:

```
*Syllabytes> main
0xbc6139d3: dyufantonbin
0x86d5bc93: rebundyugin
0xa30f81c2: zinyonranji
0x2e82536b: shorihinfyan
0x95a2ed91: gunzipyungan
0xdde02ff4: byunpashonwu
0x738f69a4: minryonfonzu
0x16810a3d: keranyachun
0x34139975: tsukingonmun
0x515d1d2b: hanhyunkyunshan
0x964db755: genyundenhun
0xfeafb260: wyozyondifa
0x4875ec33: nomunpyuchin
0xeb70a4f9: pyanmazuwon
0xe9f502cd: ponwunijyun
0xe9fe9e16: ponwyogyoke
0xe68bf5f2: peryanwunwi
0x20df1f9e: sabyonkyongyo
0xdef3d52f: byowinbunshon
0x39b80401: tondouan
0x334820d375f6b485a034911a386644faf7d9b259: chinnosabinmunwedurunzatsugankyatofenuwyawenbondihon
0x84295d5e054d8cff5a22428b195f5a1615bd644f: rusonhyunhyounnyunryuwyonhyashiniryankonhyonhyakekundyunfunyon
0x1695740538b7775067f900d859c4b49ce3bf529d: kegunmuuntodenmenhafenwonabohonjudugyupindyonhigyun
0xcad9786d1fab8148dc0ec65a33737174809c0b60: jyabonmofyunkyonzyanrannobyuyojehyachinminmanmuragyuyanfa
0x1ce108a850e54a67f2fc5d9732e8ced0774b0ba7: kyupanozohapunnyafenwiwuhyungenchipojyobamennyanyanzen
0x6c45790549bf78810c06ccfadd208cd2df3b1c4c: fyununmonunnondyonmoranyuejyuwyabyunsaryubibyonchankyunyu
0x31b6002a767e1001676797587cf6b09a59e0de1f: tandeashamemyokaanfenfengenhomyuwedagyahonpabyokyon
0x85ec0c993ce97e59f1fdc8f452e30f95b09159b8: runpyuyugonchuponmyohonwanwyunjowuhipinyongundaganhondo
0x60daeb00911f3bbcd305f238ab918b15e0830502: fabyapyanagankyonchandyubinunwitozyanganryankunparinuni
0xab987019b810b8e354a2625b63e19b11ee5f8ceb: zyangomakondokadopinhuzifihyanfinpangyankanpyohyonryupyan
0x7b7044cca816afe46aba54bc30f3eb5bdb71f81: endenunyujyaranfyawyonezyanzunnyanjinyonchodundyundenkyonran
0xba53fd4e4ded26a8a0bb754bd224f413ca62e025: dyahinwyunnyonyunpyunsezozadyanmunnyanbisuwukinjyafipasun
0x133c10f83b82ba39bb4c3d6717f0bb6327432251: kinchukawochanridyatondyannyuchunfenkenwadyanfinsenninshihan
0xc61b2a1193f5ab7e993e6e3e8467a1e311f6474: yufandizankonchonhyadenponginpepinponemyakyotankyonfumu
0xaf9092dbf43eea5d0b59976c019ec4cd9b683331: zyongagibyanwuchopyahyunyanhongenfyuangyojujyungyanfochintan
0xf8a0b1a00b3e9e2ad255f9b216ee3e4ee2549d00: wozadanzayanchogyoshabihunwondikepyochonyopihugyuna
0xd0a98ff68f2ca6b178fb35427263136021c194f2: bazonryonweryonshuzedanmowyantsunnimifinkinfasanjanguwi
0xed1271fcc2dd24e7dd07318b2b1c274ab2509e0c: pyunkimanwujibyunsupenbyunentanryanshankyusennyadihagyoyu
0x7b3b026e9c5c08df78db132c060ff75b9aa9bdb8: myanchanifyogyuhyuobyonmobyankinshueyonwenhyangyazondyundo
0xea5548b75981e03e88ddf3cb4f79b5b1f86e6a66: pyahunnodenhonranpachorobyunwinjyannyonmondundanwofyofyafe
0x539bf144128ea9c6289bb6944c9a4c55e530e328: hingyanwannukiryozonjesogyandegunyugyanyuhunpuntapinso
0xc76a3a89b175caa59bac794cfe3fbf35e63a32a7: jenfyacharondanmunjyazungyanzyumonnyuwyochondyontsunpechachizen
```
