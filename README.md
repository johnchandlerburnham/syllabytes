# Syllabytes
A Syllabic Byte Representation inspired by Japanese Kana (仮名)

Syllabytes is designed to be a pronounceable format for binary data which
maximizes human meaningfulness. We take advantage of the fact that Japanese
[gairango](https://en.wikipedia.org/wiki/List_of_gairaigo_and_wasei-eigo_terms)
already compress many non-Japanese words to a well-behaved and easily
pronounceable syllabary.

For example:

```haskell
*Syllabytes> print_syllabytes $ from_syllabytes "hiragana"
0x52809040:
  hiragana
  ヒラガナ

*Syllabytes> print_syllabytes $ from_syllabytes "katakana"
0x10301040:
  katakana
  カタカナ
```

Syllabytes ignores case and treats non-alphabetic characters as syllable
spacing, which is only required for disambiguation when a terminal `n` precedes
a vowel or `y`:

```haskell
*Syllabytes> print_syllabytes $ from_syllabytes "nu nya"
0x444a:
  nunya
  ヌニャ

*Syllabytes> print_syllabytes $ from_syllabytes "nun'ya"
0x450a:
  nun'ya
  ヌンヤ
```

And for [a fun example](https://www.youtube.com/watch?v=nGM375qYhN0):

```haskell
*Syllabytes> print_syllabytes $ from_syllabytes "jugemu jugemu goko no surikire kaijarisuigyo no suigyomatsu unraimatsu furaimatsu ku neru tokoro ni sumu tokoro yabura koji no bura koji paipo-paipo paipo no shuringan shuringan no gurindai gurindai no ponpokopi no ponpokona no chokyumei no chosuke"
0xac9674ac9674981848248212861002aa8224029e4824029e7034058002703464800270341446843818884224743818880ac4801848c48018d002d8d002d8d002d8482c83912c8391489483b0029483b00248d9d818d248d9d81840483e1c7602483e2416:
  jugemujugemugokonosurikirekaijarisuigyonosuigyomatsuunraimatsufuraimatsukunerutokoronisumutokoroyaburakonoburakopaipopaipopaiponoshuringanshuringannogurindaigurindainoponpokopinoponpokonanochokyumeinochosuke
  ジュゲムジュゲムゴコノスリキレカイジャリスイギョノスイギョマツウンライマツフライマツクネルトコロニスムトコロヤブラコノブラコパイポパイポパイポノシュリンガンシュリンガンノグリンダイグリンダイノポンポコピノポンポコナノチョキュメイノチョスケ
```

## Byte Syllabary

**Even parity**:

|0x| 0     | 2     | 4     | 6     | 8     | a      | c      | e       |
|--|-------|-------|-------|-------|-------|--------|--------|--------|
|0 |a  ア  |i  イ  |u  ウ  |e  エ  |o  オ  |ya  ヤ  |yu  ユ  |yo  ヨ  |
|1 |ka カ  |ki キ  |ku ク  |ke ケ  |ko コ  |kya キャ|kyu キュ|kyo キョ|
|2 |sa サ  |shi シ |su ス  |se セ  |so ソ  |sha シャ|shu シュ|sho ショ|
|3 |ta タ  |chi チ |tsu ツ |te テ  |to ト  |cha チャ|chu チュ|cho チョ|
|4 |na ナ  |ni ニ  |nu ヌ  |ne ネ  |no ノ  |nya ニャ|nyu ニュ|nyo ニョ|
|5 |ha ハ  |hi ヒ  |hu ホゥ|he ヘ  |ho ホ  |hya ヒャ|hyu ヒュ|hyo ヒョ|
|6 |fa ファ|fi フィ|fu フ  |fe フェ|fo フォ|fya フャ|fyu フュ|fyo フョ|
|7 |ma マ  |mi ミ  |mu ム  |me メ  |mo モ  |mya ミャ|myu ミュ|myo ミョ|
|8 |ra ラ  |ri リ  |ru ル  |re レ  |ro ロ  |rya リャ|ryu リュ|ryo リョ|
|9 |ga ガ  |gi ギ  |gu グ  |ge ゲ  |go ゴ  |gya ギャ|gyu ギュ|gyo ギョ|
|a |za ザ  |zi ジ  |zu ズ  |ze ゼ  |zo ゾ  |ja  ジャ|ju  ジュ|jo  ジョ|
|b |da ダ  |di ヂ  |du ヅ  |de デ  |do ド  |dya ヂャ|dyu ヂュ|dyo ヂョ|
|c |ba バ  |bi ビ  |bu ブ  |be ベ  |bo ボ  |bya ビャ|byu ビュ|byo ビョ|
|d |pa パ  |pi ピ  |pu プ  |pe ペ  |po ポ  |pya ピャ|pyu ピュ|pyo ピョ|
|e |wa ワ  |wi ヰ  |wu ウゥ|we ヱ  |wo ヲ  |wya ウャ|wyu ウュ|wyo ウョ|
|f |va ヷ  |vi ヸ  |vu ヴ  |ve ヹ  |vo ヺ  |vya ヴャ|vyu ヴュ|vyo ヴョ|

**Odd parity**:

|0x| 1        | 3        | 5        | 7        | 9        | b         | d         | f         |
|--|----------|----------|----------|----------|----------|-----------|-----------|-----------|
|0 |an  アン  |in  イン  |un  ウン  |en  エン  |on  オン  |yan  ヤン  |yun  ユン  |yon  ヨン  |
|1 |kan カン  |kin キン  |kun クン  |ken ケン  |kon コン  |kyan キャン|kyun キュン|kyon キョン|
|2 |san サン  |shin シン |sun スン  |sen セン  |son ソン  |shan シャン|shun シュン|shon ション|
|3 |tan タン  |chin チン |tsun ツン |ten テン  |ton トン  |chan チャン|chun チュン|chon チョン|
|4 |nan ナン  |nin ニン  |nun ヌン  |nen ネン  |non ノン  |nyan ニャン|nyun ニュン|nyon ニョン|
|5 |han ハン  |hin ヒン  |hun ホゥン|hen ヘン  |hon ホン  |hyan ヒャン|hyun ヒュン|hyon ヒョン|
|6 |fan ファン|fin フィン|fun フン  |fen フェン|fon フォン|fyan フャン|fyun フュン|fyon フョン|
|7 |man マン  |min ミン  |mun ムン  |men メン  |mon モン  |myan ミャン|myun ミュン|myon ミョン|
|8 |ran ラン  |rin リン  |run ルン  |ren レン  |ron ロン  |ryan リャン|ryun リュン|ryon リョン|
|9 |gan ガン  |gin ギン  |gun グン  |gen ゲン  |gon ゴン  |gyan ギャン|gyun ギュン|gyon ギョン|
|a |zan ザン  |zin ジン  |zun ズン  |zen ゼン  |zon ゾン  |jan ジャン |jun ジュン |jon ジョン |
|b |dan ダン  |din ヂン  |dun ヅン  |den デン  |don ドン  |dyan ヂャン|dyun ヂュン|dyon ヂョン|
|c |ban バン  |bin ビン  |bun ブン  |ben ベン  |bon ボン  |byan ビャン|byun ビュン|byon ビョン|
|d |pan パン  |pin ピン  |pun プン  |pen ペン  |pon ポン  |pyan ピャン|pyun ピュン|pyon ピョン|
|e |wan ワン  |win ヰン  |wun ウン  |wen ヱン  |won ヲン  |wyan ウャン|wyun ウュン|wyon ウョン|
|f |van ヷン  |vin ヸン  |vun ヴン  |ven ヹン  |von ヺン  |vyan ヴャン|vyun ヴュン|vyon ヴョン|


## Information Density

### ASCII
Single bytes are represented with between 1 `a` to 4 `hyun` characters (possibly
5 counting mandatory spacing). On average, assuming every byte appears with
equal probability we can measure the bits per ascii character by taking 256 * 8
bits divided by the sum of character lengths in all 256 syllabytes:

```
256 * 8 / 720
2.844
```

Compared with other formats:

| Hex | Hex (sp) | Dec  | Spacing | Proquint | Proquint (spacing) |
|-----|----------|------|---------|----------|--------------------|
| 4   | 3.2      | 3.23 | 2.65    | 3.2      | 2.86               |

However, Syllabytes has 25 symbols of 2 ascii characters and 5 symbols of 1
character. This means that for a a 32-bit namespace there are 120 4 character
names (8 bits per character) and 2193360 names of 8 characters or fewer (>4 bits
per character). So by sacrificint some average efficiency we significantly
improve our outlier efficiency.

## More Examples:

```haskell
*Syllabytes> main
0xbc6139d3:
  dyufantonpin
  ヂュファントンピン

0x86d5bc93:
  repundyugin
  レプンヂュギン

0xa30f81c2:
  zin'yonranbi
  ジンヨンランビ

0x2e82536b:
  shorihinfyan
  ショリヒンフャン

0x95a2ed91:
  gunziwyungan
  グンジウュンガン

0xdde02ff4:
  pyunwashonvu
  ピュンワションヴ

0x738f69a4:
  minryonfonzu
  ミンリョンフォンズ

0x16810a3d:
  keran'yachun
  ケランヤチュン

0x34139975:
  tsukingonmun
  ツキンゴンムン

0x515d1d2b:
  hanhyunkyunshan
  ハンヒュンキュンシャン

0x964db755:
  genyundenhun
  ゲニュンデンホゥン

0xfeafb260:
  vyojondifa
  ヴョジョンヂファ

0x4875ec33:
  nomunwyuchin
  ノムンウュチン

0xeb70a4f9:
  wyanmazuvon
  ウャンマズヺン

0xe9f502cd:
  wonvun'ibyun
  ヲンヴンイビュン

0xe9fe9e16:
  wonvyogyoke
  ヲンヴョギョケ

0xe68bf5f2:
  weryanvunvi
  ヱリャンヴンヸ

0x20df1f9e:
  sapyonkyongyo
  サピョンキョンギョ

0xdef3d52f:
  pyovinpunshon
  ピョヸンプンション

0x39b80401:
  tondouan
  トンドウアン

0x334820d375f6b485a034911a386644faf7d9b259:
  chinnosapinmunvedurunzatsugankyatofenuvyavenpondihon
  チンノサピンムンヹヅルンザツガンキャトフェヌヴャヹンポンヂホン

0x84295d5e054d8cff5a22428b195f5a1615bd644f:
  rusonhyunhyounnyunryuvyonhyashiniryankonhyonhyakekundyunfunyon
  ルソンヒュンヒョウンニュンリュヴョンヒャシニリャンコンヒョンヒャケクンヂュンフニョン

0x1695740538b7775067f900d859c4b49ce3bf529d:
  kegunmuuntodenmenhafenvon'apohonbudugyuwindyonhigyun
  ケグンムウントデンメンハフェンヺンアポホンブヅギュヰンヂョンヒギュン

0xcad9786d1fab8148dc0ec65a33737174809c0b60:
  byaponmofyunkyonjanrannopyuyobehyachinminmanmuragyuyanfa
  ビャポンモフュンキョンジャンランノピュヨベヒャチンミンマンムラギュヤンファ

0x1ce108a850e54a67f2fc5d9732e8ced0774b0ba7:
  kyuwan'ozohawunnyafenvivyuhyungenchiwobyopamennyan'yanzen
  キュワンオゾハウゥンニャフェンヸヴュヒュンゲンチヲビョパメンニャンヤンゼン

0x6c45790549bf78810c06ccfadd208cd2df3b1c4c:
  fyununmon'unnondyonmoran'yuebyuvyapyunsaryupipyonchankyunyu
  フュヌンモンウンノンヂョンモランユエビュヴャピュンサリュピピョンチャンキュニュ

0x31b6002a767e1001676797587cf6b09a59e0de1f:
  tandeashamemyokaanfenfengenhomyuvedagyahonwapyokyon
  タンデアシャメミョカアンフェンフェンゲンホミュヹダギャホンワピョキョン

0x85ec0c993ce97e59f1fdc8f452e30f95b09159b8:
  runwyuyugonchuwonmyohonvanvyunbovuhiwin'yongundaganhondo
  ルンウュユゴンチュヲンミョホンヷンヴュンボヴヒヰンヨングンダガンホンド

0x60daeb00911f3bbcd305f238ab918b15e0830502:
  fapyawyan'agankyonchandyupin'unvitojanganryankunwarin'un'i
  ファピャウャンアガンキョンチャンヂュピンウンヸトジャンガンリャンクンワリンウンイ

0xab987019b810b8e354a2625b63e19b11ee5f8ceb:
  jangomakondokadowinhuzifihyanfinwangyankanwyohyonryuwyan
  ジャンゴマコンドカドヰンホゥジフィヒャンフィンワンギャンカンウョヒョンリュウャン

0x7b7044cca816afe46aba54bc30f3eb5bdb71f81:
  enden'unyubyaranfyavyonejanzunnyanbin'yonchodundyundenkyonran
  エンデンウニュビャランフャヴョネジャンズンニャンビンヨンチョヅンヂュンデンキョンラン

0xba53fd4e4ded26a8a0bb754bd224f413ca62e025:
  dyahinvyunnyonyunwyunsezozadyanmunnyanpisuvukinbyafiwasun
  ヂャヒンヴュンニョニュンウュンセゾザヂャンムンニャンピスヴキンビャフィワスン

0x133c10f83b82ba39bb4c3d6717f0bb6327432251:
  kinchukavochanridyatondyannyuchunfenkenvadyanfinsenninshihan
  キンチュカヺチャンリヂャトンヂャンニュチュンフェンケンヷヂャンフィンセンニンシハン

0xc61b2a1193f5ab7e993e6e3e8467a1e311f6474:
  yufandizankonchonhyadenwonginwewinwonemyakyotankyonfumu
  ユファンヂザンコンチョンヒャデンヲンギンヱヰンヲネミャキョタンキョンフム

0xaf9092dbf43eea5d0b59976c019ec4cd9b683331:
  jongagipyanvuchowyahyun'yanhongenfyuangyobubyungyanfochintan
  ジョンガギピャンヴチョウャヒュンヤンホンゲンフュアンギョブビュンギャンフォチンタン

0xf8a0b1a00b3e9e2ad255f9b216ee3e4ee2549d00:
  vozadanzayanchogyoshapihunvondikewyochonyowihugyun'a
  ヺザダンザヤンチョギョシャピホゥンヺンヂケウョチョニョヰホゥギュンア

0xd0a98ff68f2ca6b178fb35427263136021c194f2:
  pazonryonveryonshuzedanmovyantsunnimifinkinfasanbanguvi
  パゾンリョンヹリョンシュゼダンモヴャンツンニミフィンキンファサンバングヸ

0xed1271fcc2dd24e7dd07318b2b1c274ab2509e0c:
  wyunkimanvyubipyunsuwenpyun'entanryanshankyusennyadihagyoyu
  ウュンキマンヴュビピュンスヱンピュンエンタンリャンシャンキュセンニャヂハギョユ

0x7b3b026e9c5c08df78db132c060ff75b9aa9bdb8:
  myanchan'ifyogyuhyuopyonmopyankinshueyonvenhyangyazondyundo
  ミャンチャンイフョギュヒュオピョンモピャンキンシュエヨンヹンヒャンギャゾンヂュンド

0xea5548b75981e03e88ddf3cb4f79b5b1f86e6a66:
  wyahunnodenhonranwachoropyunvinbyannyonmondundanvofyofyafe
  ウャホゥンノデンホンランワチョロピュンヸンビャンニョンモンヅンダンヺフョフャフェ

0x539bf144128ea9c6289bb6944c9a4c55e530e328:
  hingyanvannukiryozonbesogyandegunyugyanyuhunwuntawinso
  ヒンギャンヷンヌキリョゾンベソギャンデグニュギャニュホゥンウゥンタヰンソ

0xc76a3a89b175caa59bac794cfe3fbf35e63a32a7:
  benfyacharondanmunbyazungyanzyumonnyuvyochondyontsunwechachizen
  ベンフャチャロンダンムンビャズンギャンジュモンニュヴョチョンヂョンツンヱチャチゼン
```
