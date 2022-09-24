# BardPlay 1.3 (BardPlay Delphi)

BardPlay (C) 2022 TakeHide Soft.  
TakeHider@outlook.com

## 概要

【FF14対応】 MIDIデバイスからの情報を、PCキーボードのイベントに変換して送信します。  
仕様上、単音しか出せません。複数の音を同時に出すことはできません。  


## 改定内容
### 1.3.0
- MIDIイベントの受信処理をバッファリングに変更。
  レスポンスが大幅に改善しました。
- 複数の音を同時に出そうとしたときに、低い音から順に弾く機能を実装。(*非推奨*)

### 1.2.0
- アイコンを変更。
- オクターブ単位でトランスポーズをさせる機能を追加。
- 安定性向上に向けたマルチスレッド処理の見直し。
- iniの設定で `SendMessage`の代わりに`PostMessage`が使えるようにした。(*非推奨*)
- `exit_outrange`を指定した際に、稀に終了しない不具合の対応




## 実行方法

あらかじめMIDIデバイスをPCに接続してから、アプリケーションを実行してください。

アプリケーションを実行すると、画面にMIDIデバイス名が表示されます。  
もしエラーが出ている場合は、MIDI機器が正しく接続されているか＆電源が入っているかを確認して、リロードボタンを押してみてください。  
複数のMIDI機器を接続していて、意図しないデバイスが表示されていたときは、リストから対象の機器を選択してください。  

MIDI機器の接続エラーが出ている状態では、`Start`ボタンは押せません。  


### 画面ボタン操作

* Start  
  MIDIイベント⇒キーボード入力情報の変換処理を実行します。  
  処理が実行されると、Startボタンは、Stopボタンに切り替わります。  
* Stop  
  MIDIイベント⇒キーボード入力情報の変換処理を停止します。  
  処理が停止されると、Stopボタンは、Startボタンに切り替わります。    
* Quit  
  実行中の処理を止めて、アプリケーションを終了します。  
* Refresh  
  MIDI機器を再検索します。  


## ライセンス

このソフトウェアは GNU 劣等一般公衆利用許諾書(GNU LGPL)に基づいて配布されています。

* あなたはこのソフトウェアを、GNU LGPLに基づき、複製・転載・配布することができます。  
* あなたはこのソフトウェアを改変することができ、それをGNU LGPLに基づき配布することができます。  
* あなたはこのソフトウェアを利用して、あなた独自のライセンスのアプリケーションを製作・配布することができます。  
* いずれの場合も作者に許可を得る必要はありません。  
* このソフトウェアは全くの無保証です。  
  このライブラリを使用した結果生じた損害につきまして作者は一切責任を負いません。  
  あらかじめご了承ください。

詳しくはGNU LGPL[ライセンス - GNUプロジェクト - フリーソフトウェアファウンデーション](http://www.gnu.org/licenses/) もしくはその[日本語訳](https://licenses.opensource.jp/LGPL-3.0/LGPL-3.0.html) をご参照ください。

#### 本ソフトウェアはMIDIIOライブラリ(MIDIIO.dll)を使用します。

`MIDIIOライブラリ`の著作権は"`(C)2002-2012 くず / おーぷんMIDIぷろじぇくと`"様が保有しています。  
 https://openmidiproject.osdn.jp/MIDIIOLibrary.html  
LGPLライセンスで配布されています  


#### ボタンのイメージには Icons 8を使用しています。
イメージボタンの画像は [Icons 8](https://icons8.com)(https://icons8.com) を利用しています。  


## iniファイル

### [CONFIG]

* exit_outrange (default=1)  
  キーマッピングされた範囲外の音が出されたら、処理を停止します。  
  0を指定するとこのオプションは機能しません。  
  1以上の数字を指定すると、マッピングされた範囲から指定された数だけ外れた音が出されたときに、処理を停止します。  
  例えば`1`を指定すると、範囲のすぐ外側の音から有効になります。(半外の音を出した途端に、処理が止まります。)    
  鍵盤楽器など、誤って押してしまいそうなときは、少し範囲を広げてください。  

* start_on_run (default=0)  
  アプリケーション実行時に処理を開始します。  
  ただし、MIDI機器が正しく接続されていないと、実行されません。  

* device_name  
  最後に接続したMIDIデバイス名を保持します。  
  初めて使用されるときは、画面のリストから選択してください。   
  iniファイルの値を直接編集すると、うまく動かない可能性があります。  

* transepose (default=0)
  特定のMIDI機器では、中心のC音が `C4(noteNo.60 - 261.6hz)`ではなく、1オクターブ低かったり、高かったりします。  
  `-3`～`3`の範囲でオクターブを指定することで、音程を調節することができます。

* virtual_chords (default=0)
  和音などで複数の音を同時に出そうとしたときに、低い音から順に出すようにします。  
  とは言え「同時に出す」が難しく、ほぼ気持ちの問題。  
  ノートオフの情報と混ざった時のテストが十分に出来なかったので、非推奨機能に格下げしました。  
 

### [MAPPING]

MIDIのノートをキーに対応させたもの。  
対応するMIDIノートに、割り当てたいキーを指定します。   
(MIDIノートは、READMEの最後に記載しています。)    
`SHIFT`キーや`CTRL`キーを使って、複数のキーを同時に押したいときは、押す順番にスペースで区切ってください。  
例) `SHIFT` + `S`キーのとき -> `shift s`  
特殊キーは Pythonの`pyautogui`に準拠していますが、全ての特殊キーには対応できていません。  
きっと`SHIFT`キーと`CRTL`キーがあれば十分でしょう。


### iniファイル名について

* 実行ファイル名を変更した時は、iniファイルの名前も変えてください。  
  例）  
  　`BardPlay.exe` の時は `bardplay.ini`  
  　`BardPlay2.exe` の時は `bardplay2.ini`

* iniファイルは、Python版 BardPlay.py ([BardPlay 0.9x](https://github.com/TakeHider/BardPlayPy))、GO言語版 BardPlay.go ([BardPlay 1.0x](https://github.com/TakeHider/BardPlayGo))と互換性があります。  
  ただし、[CONFIG]のセクションは、本バージョンから使用されなくなったオプションがあります。  



## その他

以下の機器で動作確認をしています。  
- CACIO LK-511 61鍵 電子キーボード 光ナビゲーション  
- YAMAHA SHS-300 Sonogenic 37鍵 ショルダーキーボード

MIDIの仕様を確認して各社に対応したつもりですが、全てのメーカーの機器を試せていないので、もしかすると正しく動作しない機器があるかもしれません。  
その場合は、お手数ですがご連絡をいただければ、極力対応するようにいたします。   
(ノートOFFは `0x80` - `0xXX` と、`0x90` - `0x00` の2種類に対応しています)  

FF14等で使用していて、もし音が止まらなくなったときは、焦らずに対応するPC-Keyを押してみてください。  


## 変更履歴

|バージョン|リリース日|内容|
|:--|:-:|:--|
|1.3.0|2022/9/24|レスポンスの改善|
|1.2.0|2022/9/19|1オクターブ単位でトランスポーズ機能を追加<br/> パフォーマンスの向上、および軽微な不具合対応|
|1.1.0|2022/9/15|Delphi版作成|


以下、英語訳とマッピングの際の参考資料を記載。  

----


# BardPlay 1.3 (BardPlay Delphi)

BardPlay (C) 2022 TakeHide Soft.  
TakeHider@outlook.com

## Overview

[FF14 compatible] Converts information from MIDI devices into PC keyboard events and transmits them.  
Due to the specifications, only single notes can be produced. It is not possible to play multiple sounds at the same time. 

## Revised contents
### 1.3.0
- Changed MIDI event reception processing to buffering.  
   Greatly improved response.  
- Implemented a function that plays in order from the lowest note when trying to play multiple notes at the same time. (*not recommended*)

### 1.2.0
- Change icon.
- Added a function to transpose in octave units.
- Review of multithreading for stability improvement.
- Made it possible to use `PostMessage` instead of `SendMessage` in the ini settings. (*not recommended*)
- Fixed a bug that rarely exits when `exit_outrange` is specified


## Execution

Connect the MIDI device to your PC in advance, and then run the application.  

When you run the application, you will see the MIDI device name on the screen.  
If you get an error, make sure your MIDI device is connected correctly and powered on, and try pressing the reload button.  
If multiple MIDI devices are connected and an unintended device is displayed, select the target device from the list.  

The `Start` button cannot be pressed while there is a MIDI device connection error.  


### Screen button operation

* Start  
   Executes conversion processing of MIDI event ⇒ keyboard input information.  
   Once the process is done, the Start button will switch to a Stop button.  
* Stop  
   Stop converting MIDI event ⇒ keyboard input information.  
* Quit  
   Quit the application.  
* Refresh  
   Search for MIDI devices again.  


## license

This software is distributed under the GNU Lesser General Public License (GNU LGPL).  

* You may copy, reprint, and distribute this software under the GNU LGPL.  
* You may modify this software and distribute it under the GNU LGPL.  
* You can use this software to create and distribute your own licensed applications.  
* In either case, you don't need to get permission from the author.  
* This software comes with absolutely no warranty.  
   The author is not responsible for any damages resulting from using this library.  
   Please note.  

For details, see GNU LGPL [License - GNU Project - Free Software Foundation](http://www.gnu.org/licenses/).  

#### This software uses MIDIIO Library (MIDIIO.dll).

The copyright of the MIDIIO library is held by "(C)2002-2012 Kuzu / Open MIDI Project".  
 https://openmidiproject.osdn.jp/MIDIIOLibrary.html   
Distributed under LGPL license  

#### Using Icons 8 for the button image.

The image of the image button uses [Icons 8](https://icons8.com)(https://icons8.com).


## ini File

### [CONFIG]

* device_name  
  Retains the name of the last connected MIDI device.  
  When using for the first time, select from the list on the screen.  

* exit_outrange (default=1)  
  Stop processing when a note outside the keymapped range is played.  
  This option has no effect if you specify 0.  
  If you specify a number greater than or equal to 1, processing will stop when the specified number of notes outside the mapped range are played.  
  For example, if you specify `1`, the sound immediately outside the range will be effective. (The process stops as soon as the half-out sound is produced.)  
  Widen the range slightly when you are likely to accidentally press a keyboard instrument.  

* start_on_run (default=0)  
  Start processing when the application is run.  
  However, if the MIDI device is not properly connected, it will not run.

* transpose (default=0)
   On certain MIDI devices, the middle C note is not `C4(noteNo.60 - 261.6hz)`, but one octave lower or higher.  
   You can adjust the pitch by specifying an octave in the range of `-3` to `3`.

* virtual_chords (default=0)
   When trying to produce multiple sounds at the same time, such as in a chord, the sounds will be produced in order from the lowest note.  
   However, it is difficult to "put out at the same time", and it is almost a matter of feeling.  
   It was deprecated because it could not be sufficiently tested when mixed with note-off information.  


### [MAPPING]

A MIDI note that corresponds to a key.  
Specify the key you want to assign to the corresponding MIDI note.  
If you want to press the SHIFT key or CTRL key at the same time, separate the keys in the order you press them with a space.  
Example) SHIFT + S key -> `shift s`
Special keys conform to Python's pyautogui, but not all special keys are supported.  
Surely the SHIFT and CRTL keys will suffice.  

### About ini file name

* When you change the name of the executable file, please change the name of the ini file as well.  
   example)    
   　When using BardPlay.exe, bardplay.ini  
   　When using BardPlay2.exe, bardplay2.ini  

* The ini file is Python version BardPlay.py ([BardPlay 0.9x](https://github.com/TakeHider/BardPlayPy)), GO language version BardPlay.go ([BardPlay 1.0x](https://github. com/TakeHider/BardPlayGo)).  
   However, the [CONFIG] section has options that are no longer used in this version.  




## Other

Operation has been confirmed with the following devices.  
* CACIO LK-511 61-key electronic keyboard optical navigation  
* YAMAHA SHS-300 Sonogenic 37-key shoulder keyboard  

I have checked the MIDI specifications and tried to support each company, but I have not been able to test all manufacturers' devices, so there may be devices that do not work properly.  
In that case, please contact us and we will do our best to accommodate you.  
(`Note OFF` supports two types: `0x80 - 0xXX` and `0x90 - 0x00`)  

If you are using it in FF14 etc. and the sound does not stop, please try to press the corresponding PC-Key without rushing.  

## change history

|Version|Release Date|Contents|
|:--|:-:|:--|
|1.3.0|2022/9/24|Improved response|
|1.2.0|2022/9/19|Added transpose function by 1 octave unit<br/> Improved performance and fixed minor bugs|
|1.1.0|2022/9/15|Created Delphi version|


---
## reference
.ini file [MAPPING] section setting guide. 

### Keyboard-Mapping Configuration 

|configuration<br />Strings|KeyCode<br />(reference)|
|:-:|:-:|
|backspace | 0x08|
|tab       | 0x09|
|enter     | 0x0D|
|shift     | 0x10|
|ctrl      | 0x11|
|alt       | 0x12|
|pause     | 0x13|
|capslock  | 0x14|
|esc       | 0x1B|
|space     | 0x20|
|pageup    | 0x21|
|pagedown  | 0x22|
|end       | 0x23|
|home      | 0x24|
|left      | 0x25|
|up        | 0x26|
|right     | 0x27|
|down      | 0x28|
|printscrn | 0x2C|
|insert    | 0x2D|
|delete    | 0x2E|
|0         | 0x30|
|1         | 0x31|
|2         | 0x32|
|3         | 0x33|
|4         | 0x34|
|5         | 0x35|
|6         | 0x36|
|7         | 0x37|
|8         | 0x38|
|9         | 0x39|
|a         | 0x41|
|b         | 0x42|
|c         | 0x43|
|d         | 0x44|
|e         | 0x45|
|f         | 0x46|
|g         | 0x47|
|h         | 0x48|
|i         | 0x49|
|j         | 0x4A|
|k         | 0x4B|
|l         | 0x4C|
|m         | 0x4D|
|n         | 0x4E|
|o         | 0x4F|
|p         | 0x50|
|q         | 0x51|
|r         | 0x52|
|s         | 0x53|
|t         | 0x54|
|u         | 0x55|
|v         | 0x56|
|w         | 0x57|
|x         | 0x58|
|y         | 0x59|
|z         | 0x5A|
|f1        | 0x70|
|f2        | 0x71|
|f3        | 0x72|
|f4        | 0x73|
|f5        | 0x74|
|f6        | 0x75|
|f7        | 0x76|
|f8        | 0x77|
|f9        | 0x78|
|f10       | 0x79|
|f11       | 0x7A|
|f12       | 0x7B|


### MIDI Note Infomation  

| Tone    | Note Number | .ini File |
|:-------:| -----------:| --------- |
| C-1     | 0           | ×:None    |
| C#      | 1           | ×:None    |
| D       | 2           | ×:None    |
| D#      | 3           | ×:None    |
| E       | 4           | ×:None    |
| F       | 5           | ×:None    |
| F#      | 6           | ×:None    |
| G       | 7           | ×:None    |
| G#      | 8           | ×:None    |
| A       | 9           | ×:None    |
| A#      | 10          | ×:None    |
| B       | 11          | ×:None    |
| C0      | 12          | ×:None    |
| C#      | 13          | ×:None    |
| D       | 14          | ×:None    |
| D#      | 15          | ×:None    |
| E       | 16          | ×:None    |
| F       | 17          | ×:None    |
| F#      | 18          | ×:None    |
| G       | 19          | ×:None    |
| G#      | 20          | ×:None    |
| A       | 21          | ×:None    |
| A#      | 22          | ×:None    |
| B       | 23          | ×:None    |
| C1      | 24          | ×:None    |
| C#      | 25          | ×:None    |
| D       | 26          | ×:None    |
| D#      | 27          | ×:None    |
| E       | 28          | ×:None    |
| F       | 29          | ×:None    |
| F#      | 30          | ×:None    |
| G       | 31          | ×:None    |
| G#      | 32          | ×:None    |
| A       | 33          | ×:None    |
| A#      | 34          | ×:None    |
| B       | 35          | ×:None    |
| C2      | 36          | ×:None    |
| C#      | 37          | ×:None    |
| D       | 38          | ×:None    |
| D#      | 39          | ×:None    |
| E       | 40          | ×:None    |
| F       | 41          | ×:None    |
| F#      | 42          | ×:None    |
| G       | 43          | ×:None    |
| G#      | 44          | ×:None    |
| A       | 45          | ×:None    |
| A#      | 46          | ×:None    |
| B       | 47          | ×:None    |
| **C3**  | **48**      | ○:Set     |
| C#      | 49          | ○:Set     |
| D       | 50          | ○:Set     |
| D#      | 51          | ○:Set     |
| E       | 52          | ○:Set     |
| F       | 53          | ○:Set     |
| F#      | 54          | ○:Set     |
| G       | 55          | ○:Set     |
| G#      | 56          | ○:Set     |
| A       | 57          | ○:Set     |
| A#      | 58          | ○:Set     |
| B       | 59          | ○:Set     |
| **C4**  | **60**      | ○:Set     |
| C#      | 61          | ○:Set     |
| D       | 62          | ○:Set     |
| D#      | 63          | ○:Set     |
| E       | 64          | ○:Set     |
| F       | 65          | ○:Set     |
| F#      | 66          | ○:Set     |
| G       | 67          | ○:Set     |
| G#      | 68          | ○:Set     |
| A       | 69          | ○:Set     |
| A#      | 70          | ○:Set     |
| B       | 71          | ○:Set     |
| **C5**  | **72**      | ○:Set     |
| C#      | 73          | ○:Set     |
| D       | 74          | ○:Set     |
| D#      | 75          | ○:Set     |
| E       | 76          | ○:Set     |
| F       | 77          | ○:Set     |
| F#      | 78          | ○:Set     |
| G       | 79          | ○:Set     |
| G#      | 80          | ○:Set     |
| A       | 81          | ○:Set     |
| A#      | 82          | ○:Set     |
| B       | 83          | ○:Set     |
| **C6**  | **84**      | ○:Set     |
| C#      | 85          | ×:None    |
| D       | 86          | ×:None    |
| D#      | 87          | ×:None    |
| E       | 88          | ×:None    |
| F       | 89          | ×:None    |
| F#      | 90          | ×:None    |
| G       | 91          | ×:None    |
| G#      | 92          | ×:None    |
| A       | 93          | ×:None    |
| A#      | 94          | ×:None    |
| B       | 95          | ×:None    |
| C7      | 96          | ×:None    |
| C#      | 97          | ×:None    |
| D       | 98          | ×:None    |
| D#      | 99          | ×:None    |
| E       | 100         | ×:None    |
| F       | 101         | ×:None    |
| F#      | 102         | ×:None    |
| G       | 103         | ×:None    |
| G#      | 104         | ×:None    |
| A       | 105         | ×:None    |
| A#      | 106         | ×:None    |
| B       | 107         | ×:None    |
| C8      | 108         | ×:None    |
| C#      | 109         | ×:None    |
| D       | 110         | ×:None    |
| D#      | 111         | ×:None    |
| E       | 112         | ×:None    |
| F       | 113         | ×:None    |
| F#      | 114         | ×:None    |
| G       | 115         | ×:None    |
| G#      | 116         | ×:None    |
| A       | 117         | ×:None    |
| A#      | 118         | ×:None    |
| B       | 119         | ×:None    |
| C9      | 120         | ×:None    |
| C#      | 121         | ×:None    |
| D       | 122         | ×:None    |
| D#      | 123         | ×:None    |
| E       | 124         | ×:None    |
| F       | 125         | ×:None    |
| F#      | 126         | ×:None    |
| G       | 127         | ×:None    |
