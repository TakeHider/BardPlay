# BardPlay 1.3 (BardPlay Delphi)

BardPlay (C) 2022 TakeHide Soft.  
TakeHider@outlook.com

## 概要

【FF14対応】 MIDIデバイスからの情報を、PCキーボードのイベントに変換して送信します。  

似たようなアプリケーションは他にもいくつかありますが、かゆいところに手が届かなかったので自前で作りました。   
Ver.0.9(Python版) -> Ver.1.0(GO言語版) -> Ver.1.1以降(Delphi版) と開発を進めてます。  
リポジトリの中には各開発言語のソースが入っています。  

## 改定内容
### 1.3.0
- MIDIイベントの受信処理をバッファリングに変更。
  レスポンスが大幅に改善しました。
- 複数の音を同時に出そうとしたときに、低い音から順に弾く機能を実装。  
  ただし「同時に」が難しく、機能させるのは至難の業。殆どの人は、(機械的に見て)押した順番でしか出せないと思います。  
  実装はしたものの、作成者本人が「ノートオフ情報を絡めたテストが十分に出来なかった」ため、非推奨機能に格下げしました。  


### 1.2.0
- アイコンを変更。
- オクターブ単位でトランスポーズをさせる機能を追加。
- 安定性向上に向けたマルチスレッド処理の大幅見直し。  
- Wait処理の変更  
  `Application.ProcessMessages`を止めて`sleep(1)`を使っています。  
- `exit_outrange`を指定した際に、終了しない場合がある不具合への対応
- iniの設定で `SendMessage`の代わりに`PostMessage`が使えるようにした。(*非推奨*)



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

### 本ソフトウェアはMIDIIOライブラリ(MIDIIO.dll)を使用します。

MIDIIOライブラリの著作権は"(C)2002-2012 くず / おーぷんMIDIぷろじぇくと"様が保有しています。  
 https://openmidiproject.osdn.jp/MIDIIOLibrary.html  
LGPLライセンスで配布されています  


### ボタンのイメージには Icons 8を使用しています。
イメージボタンの画像は [Icons 8](https://icons8.com)(https://icons8.com) を利用しています。  


## Delphiでのコンパイル

本アプリケーションは Embarcadero Delphi 10.4 Community Edition で開発されています。  
デフォルトの状態で、そのままコンパイルすることができます。  
ただし、実行には後述の`MIDIIO.DLL`が必要です。


## その他必要なライブラリ

* MIDIIO.dll - MIDIIOライブラリ  (C)2002-2022 くず
  [おーぷんMIDIぷろじぇくと](https://openmidiproject.osdn.jp/MIDIIOLibrary.html) (https://openmidiproject.osdn.jp/MIDIIOLibrary.html)

MIDIIO.dllを、プロジェクトと同じフォルダか、パスの通っているフォルダに格納してください。

## 実行方法

あらかじめMIDIデバイスをPCに接続してから、アプリケーションを実行してください。

画面にMIDIデバイス名が表示されます。  
もしエラーが出た場合は、MIDI機器が正しく接続されているか＆電源が入っているかを確認して、リロードボタンを押してみてください。  
複数のMIDI機器を接続していて、意図しないデバイスが表示されていたときは、リストから対象の機器を選択してください。  

詳細は、readme.mdをご参照ください。


## iniファイル

### [CONFIG]

* device_name  
  最後に接続したMIDIデバイス名を保持します。  
  初めて使用されるときは、画面のリストから選択してください。   

* exit_outrange (default=1)  
  キーマッピングされた範囲外の音が出されたら、処理を停止します。  
  0を指定するとこのオプションは機能しません。  
  1以上の数字を指定すると、指定された数だけ範囲から外れた音が出されたときに、処理を停止します。  
  1だと、範囲のすぐ外側の音から有効になります。2だと、2音外側の音から有効になります。    
  鍵盤楽器などで、誤って押してしまいそうなときは、少し範囲を広げてください。  
  アプリケーションの画面に戻らなくても、MIDI楽器から処理を停止させることが出来る機能ですが、再び処理を開始させるにはアプリの画面から`Start`ボタンを押すしかなく、便利な様でいまいちな機能です。      

* start_on_run (default=0)  
  アプリケーション実行時に処理を開始します。  
  MIDI機器が正しく接続されていないと実行されません。  

* transepose (default=0)  
  オクターブを、`-3`～`3`の範囲で調節することができます。  
  SHS-300側でギターの音色を指定したら1オクターブ低かったので、慌てて実装しました。  
  1音単位で調整できるようにすることもできますが、個人的には要らないので実装していません。(
  要望があれば付けます)

* use_postmessage (default=0)  
  `SendMessage`の代わりに`PostMessage`を使います。  
  パフォーマンスは若干上がったのですが、CTRLやSHIFTを同時に押すようなときに上手く動作しない場合があったので、公式な設定から外しました。  
  そのうち`PostMessage`は機能からも落とします。  

* virtual_chords (default=0)  
  和音などで複数の音を同時に出そうとしたときに、低い音から順に出力してくれます。  
  内部的には、バッファリングされた内容を低音→高音順にソートして処理しています。  
  実際はバッファリングされるよりも先に処理をこなしてしまうので、よほどのことがない限り「低い音から順に出る」の機能を体感するとはないと思います。  
  開発者も、ノートオンはテストできたのですが、ノートオフと混在した時のテストが十分に出来ず、品質の担保ができないため、実装したものの非推奨機能にしています。
    
* color (default=#F0FFF0)  
  アプリケーションの背景色を指定します。  
  自分的には気に入った色を背景色にしたつもりですが、「いまいちだなぁ」と思う人もいるかもしれないので実装しました。    

### [MAPPING]

MIDIのノートをキーに対応させたもの。  
対応するMIDIノートに、割り当てたいキーを指定します。    
SHIFTキーや、CTRLキーなど、同時に押したいときは、押す順番にスペースで区切ってください。  
例) SHIFT + Sキーのとき -> `shift s`  
特殊キーは Pythonのpyautoguiに準拠していますが、全ての特殊キーには対応できていません。  
きっとSHIFTキーとCRTLキーがあれば十分でしょう。   


### iniのファイル名について

* 実行ファイル名を変更した時は、iniファイルの名前も変えてください。  
  例）  
  　BardPlay.exe の時は bardplay.ini  
  　BardPlay2.exe の時は bardplay2.ini

* iniファイルは、Python版 BardPlay.py ([BardPlay 0.9x](https://github.com/TakeHider/BardPlayPy))、GO言語版 BardPlay.go ([BardPlay 1.0x](https://github.com/TakeHider/BardPlayGo))と互換性があります。  
  ただし、[CONFIG]のセクションは、本バージョンから使用されなくなったオプションがあります。  


## その他

最初にPythonで作ったものはコンソールからの実行でした。  
そこで、GUIにするためにGO言語で作りなおしたのですが、GO言語はあまりに実行ファイルのサイズが大きくなりすぎたので、Delphiで作り直すことにしました。  
Delphi版は、当初はPython版よりレスポンスが悪かったのですが、`Ver.1.3.0`でようやくPython版に並ぶことが出来ました。   

同時発音数が1音しかありませんが、ターゲットがFF14なので、複数同時発音に対応させるつもりはありません。 

個人的に必要な機能が充足できたので、これ以上バージョンアップをさせる予定はありません。    
もし要望があれば、機能として取り込んでいく予定です。  



## 変更履歴

|バージョン|言語|リリース日|内容|
|:--|:-:|:-:|:--|
|1.3.0|Delphi|2022/9/24|レスポンスの改善<br/> 同時に音を出したときに低い音から順に出す機能を実装|
|1.2.0|Delphi|2022/09/19|1オクターブ単位でトランスポーズ機能を追加<br/> パフォーマンスの向上、および軽微な不具合対応<br />PostMessageにも対応|
|1.1.0|Delphi|2022/09/15|Delphi版作成|
|1.0.0|GOlang|2022/09/06|GOlang版作成|
|0.9.1|Python|2022/09/06|WindowsではSendMessageで送信に変更|
|0.9.0|Python|2022/09/04||初回リリース(Python)|

----


# BardPlay 1.3 (BardPlay Delphi)

BardPlay (C) 2022 TakeHide Soft.  
TakeHider@outlook.com

## Overview

[FF14 compatible] Converts information from MIDI devices into PC keyboard events and transmits them.  

There are some other similar applications, but I couldn't reach the itch, so I made my own.  
Ver.0.9 (Python version) -> Ver.1.0 (GO language version) -> Ver.1.1 or later (Delphi version).  
The repository contains sources for each development language.  



## Revised contents
### 1.3.0  
- Changed MIDI event reception processing to buffering.
   Greatly improved response.  
- Implemented a function to play in order from the lowest note when trying to play multiple notes at the same time.  
   However, "at the same time" is difficult, and it is extremely difficult to make it work. Most people think that they can only come out in the order they were pressed (from a mechanical point of view).  
   Although it was implemented, it was deprecated because the creator himself said, "I couldn't sufficiently test the note-off information."  

### 1.2.0
- Change icon.
- Added a function to transpose in octave units.
- Major overhaul of multithreading for stability improvement.
- Changed Wait processing
   Stopping `Application.ProcessMessages` and using `sleep(1)`.
- Dealing with a bug that may not end when `exit_outrange` is specified
- Made it possible to use `PostMessage` instead of `SendMessage` in the ini settings. (*not recommended*)



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

### This software uses MIDIIO Library (MIDIIO.dll).

The copyright of the MIDIIO library is held by "(C)2002-2012 Kuzu / Open MIDI Project".  
 https://openmidiproject.osdn.jp/MIDIIOLibrary.html   
Distributed under LGPL license  

### UUsing Icons 8 for the button image.

The image of the image button uses [Icons 8](https://icons8.com)(https://icons8.com).

## Compilation in Delphi

This application is developed on his Embarcadero Delphi 10.4 Community Edition.  
You can compile as-is in the default state.  
However, `MIDIIO.DLL` described later is required for execution.  


## Other required libraries

* MIDIIO.dll - MIDIIO library (C)2002-2022 scrap [Open MIDI project](https://openmidiproject.osdn.jp/MIDIIOLibrary.html) (https://openmidiproject.osdn.jp/MIDIIOLibrary.html )

Store MIDIIO.dll in the same folder as the project or in a folder with a path.


## Execution method

Connect the MIDI device to your PC in advance, and then run the application.  

The screen will show the MIDI device name.  
If you get an error, make sure your MIDI device is connected correctly and powered on, and try pressing the reload button.  
If multiple MIDI devices are connected and an unintended device is displayed, select the target device from the list.  

See readme.md for details.  


### Screen button operation

* Start  
   Executes conversion processing of MIDI event ⇒ keyboard input information.  
   If the MIDI device is not properly connected, the button cannot be pressed.  
* Stop  
   Stop converting MIDI event ⇒ keyboard input information.  
* Quit  
   Quit the application.  
* Refresh  
   Search for MIDI devices again.  


## ini File

### [CONFIG]

* port_in (default=1)  
   MIDI port number to use.  
   Not used in Delphi version.  

* device_name  
   Retains the name of the last connected MIDI device.  
   When using for the first time, select from the list on the screen.  

* exit_outrange (default=1)  
   Stop processing when a note outside the keymapped range is played.  
   This option has no effect if you specify 0.  
   If you specify a number greater than or equal to 1, processing will stop when the specified number of out-of-range notes are played.  
   A value of 1 activates notes immediately outside the range.  
   Widen the range slightly when you are likely to accidentally press a keyboard instrument.  

* start_on_run (default=0)  
   Start processing when the application is run.  
   However, if the MIDI device is not properly connected, it will not run.  

* transpose (default=0)  
   You can adjust the pitch by specifying an octave in the range of `-3` to `3`.  
   When I specified the guitar tone on the SHS-300, it was one octave lower, so I implemented it in a hurry.  
   It is also possible to make it possible to adjust by 1 note unit, but I personally don't need it, so I didn't implement it.  
   I will add it if requested.  

* use_postmessage (default=0)  
  Use `PostMessage` instead of `SendMessage`.  
  The performance has improved a little, but there were cases where it didn't work well when pressing CTRL and SHIFT at the same time, so it was removed from the official setting.  
  Among them, `PostMessage` is also dropped from the function.  

* virtual_chords (default=0)  
  When you try to output multiple sounds at the same time, such as a chord, it will output in order from the lowest sound.  
  Internally, the buffered contents are sorted in order from low to high and processed.  
  In fact, the processing is done before it is buffered, so I don't think you will experience the function of "playing in order from the lowest sound" unless there is a very good reason.  
  The developer was able to test note-on, but it was not possible to fully test when it was mixed with note-off.  
    
* color (default=#F0FFF0)  
  Specifies the background color of the application.  
  Personally, I intended to use a color I like as the background color, but some people may think that it's not good enough, so I implemented it.  


### [MAPPING]

A MIDI note that corresponds to a key.  
Specify the key you want to assign to the corresponding MIDI note.  
If you want to press the SHIFT key or CTRL key at the same time, separate the keys in the order you press them with a space.  
Example) SHIFT + S key -> `shift s`
Special keys conform to Python's pyautogui, but not all special keys are supported.  
Surely the SHIFT and CRTL keys will suffice.  

### memo

* When you change the name of the executable file, please change the name of the ini file as well.  
   example)    
   　When using BardPlay.exe, bardplay.ini  
   　When using BardPlay2.exe, bardplay2.ini  

* The ini file is Python version BardPlay.py ([BardPlay 0.9x](https://github.com/TakeHider/BardPlayPy)), GO language version BardPlay.go ([BardPlay 1.0x](https://github. com/TakeHider/BardPlayGo)).  
   However, the [CONFIG] section has options that are no longer used in this version.  




## others

The first thing I made with Python was execution from the console.  
Therefore, I remade it in GO language to make it a GUI, but GO language made the size of the executable file too large, so I decided to remake it in Delphi.  
The Delphi version had a worse response than the Python version at first, but with `Ver.1.3.0` it was finally able to line up with the Python version.  


There is only one sound that can be played simultaneously, but since the target is FF14, we do not intend to support multiple simultaneous sounds.


Since I was able to satisfy the functions that I personally need, there are no plans to upgrade any further.  
If there is a demand, we plan to include it as a feature.




## change history

|Version|Language|Release Date|Contents|
|:--|:-:|:-:|:--|
|1.3.0|Delphi|2022/9/24|Improved response<br/> Implemented a function to output sounds in order from the lowest one when playing sounds at the same time|
|1.2.0|Delphi|2022/09/19|Added transpose function by 1 octave unit<br/> Improved performance and fixed minor bugs<br />Also supports PostMessage|
|1.1.0|Delphi|2022/09/15|Created Delphi version|
|1.0.0|GOlang|2022/09/06|GOlang version created|
|0.9.1|Python|2022/09/06|Changed to sending with SendMessage on Windows|
|0.9.0|Python|2022/09/04||Initial release (Python)|


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
