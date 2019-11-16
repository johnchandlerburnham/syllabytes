# Syllabytes
A Syllabic Byte Representation inspired by Japanese Kana (仮名)

## Bytes

Even parity:

 | 0x | 0  | 2  | 4  | 6  | 8  | A   | C   | E   |
 |----|----|----|----|----|----|-----|-----|-----|
 | 0  | a  | i  | u  | e  | o  | ya  | yu  | yo  |
 | 1  | ka | ki | ku | ke | ko | kya | kyu | kyo |
 | 2  | sa | si | su | se | so | sha | shu | sho |
 | 3  | ta | ti | tu | te | to | cha | chu | cho |
 | 4  | na | ni | nu | ne | no | nya | nyu | nyo |
 | 5  | ha | hi | hu | he | ho | hya | hyu | hyo |
 | 6  | fa | fi | fu | fe | fo | fya | fyu | fyo |
 | 7  | ma | mi | mu | me | mo | mya | myu | myo |
 | 8  | ra | ri | ru | re | ro | rya | ryu | ryo |
 | 9  | ga | gi | gu | ge | go | gya | gyu | gyo |
 | a  | za | zi | zu | ze | zo | zya | zyu | zyo |
 | b  | da | di | du | de | do | dya | dyu | dyo |
 | c  | ja | ji | ju | je | jo | jya | jyu | jyo |
 | d  | ba | bi | bu | be | bo | bya | byu | byo |
 | e  | pa | pi | pu | pe | po | pya | pyu | pyo |
 | f  | wa | wi | wu | we | wo | wya | wyu | wyo |

Odd parity:

 | 0x | 1   | 3   | 5   | 7   | 9   | B    | D    | F    |
 |----|-----|-----|-----|-----|-----|------|------|------|
 | 0  | an  | in  | un  | en  | on  | yan  | yun  | yon  |
 | 1  | kan | kin | kun | ken | kon | kyan | kyun | kyon |
 | 2  | san | sin | sun | sen | son | shan | shun | shon |
 | 3  | tan | tin | tun | ten | ton | chan | chun | chon |
 | 4  | nan | nin | nun | nen | non | nyan | nyun | nyon |
 | 5  | han | hin | hun | hen | hon | hyan | hyun | hyon |
 | 6  | fan | fin | fun | fen | fon | fyan | fyun | fyon |
 | 7  | man | min | mun | men | mon | myan | myun | myon |
 | 8  | ran | rin | run | ren | ron | ryan | ryun | ryon |
 | 9  | gan | gin | gun | gen | gon | gyan | gyun | gyon |
 | a  | zan | zin | zun | zen | zon | zyan | zyun | zyon |
 | b  | dan | din | dun | den | don | dyan | dyun | dyon |
 | c  | jan | jin | jun | jen | jon | jyan | jyun | jyon |
 | d  | ban | bin | bun | ben | bon | byan | byun | byon |
 | e  | pan | pin | pun | pen | pon | pyan | pyun | pyon |
 | f  | wan | win | wun | wen | won | wyan | wyun | wyon |

## Examples:

```haskell
*Syllabytes> syllabytes 0x10800816
"karaoke"
*Syllabytes> syllabytes 0x476ffe
"nenfyonwyo"
```
