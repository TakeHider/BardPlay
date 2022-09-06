# BardPlay 1.0 (BardPlay GO)

BardPlay (C) 2022 TakeHide Soft.  
TakeHider@outlook.com

## 概要

【FF14対応】 MIDIデバイスからの情報を、PCキーボードのイベントに変換して送信します。

Python版を作成したものの、必要なパフォーマンスが出なかったので、新たにGO言語で焼き直しました。  
GO言語で作たため、実行ファイルのファイルサイズは大きくなります。

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

## GO言語で必要なライブラリ

標準のGO言語開発環境に加え、下記のライブラリが別途必要です。  

* fyne.io  
* path/filepath  
* gopkg.in/ini.v1  

インストール方法  

```cmd
> CD ≪プロジェクトフォルダ≫
> go get fyne.io/fyne
> go get gopkg.in/ini.v1
```

## その他必要なライブラリ

* MIDIIO.dll - MIDIIOライブラリ  (C)2002-2022 くず
  [おーぷんMIDIぷろじぇくと](https://openmidiproject.osdn.jp/MIDIIOLibrary.html) (https://openmidiproject.osdn.jp/MIDIIOLibrary.html)

MIDIIO.dllを、プロジェクトと同じフォルダか、パスの通っているフォルダに格納してください。

## 実行方法

あらかじめMIDIデバイスをPCに接続してから、以下のコマンドを実行します。

```cmd
> cd ≪プロジェクトフォルダ≫
> go run BardPlay.go
```

画面にMIDIデバイス名が表示されます。
もしエラーが出た場合、あるいは、複数のMIDIデバイスを接続していて、意図しないデバイスが表示されていたときは、iniファイルでポート番号(port_in)を変更してみてください。

### 画面ボタン操作

* START
  MIDIイベント⇒キーボード入力情報の変換処理を実行します。

* STOP
  MIDIイベント⇒キーボード入力情報の変換処理を停止します。

* QUIT
  アプリケーションを終了します。

## iniファイル

### [CONFIG]

* port_in (default=1)  
  使用するMIDIのポート番号。  
  もし複数のMIDIデバイスを持っていたら、番号を変更してみてください。  

* exit_outrange (default=1)  
  キーマッピングされた範囲外の音が出されたら、処理を停止します。  
  0を指定するとこのオプションは機能しません。  
  1以上の数字を指定すると、指定された数だけ範囲から外れた音が出されたときに、処理を停止します。  
  1だと、範囲のすぐ外側の音から有効になります。  
  鍵盤楽器など、誤って押してしまいそうなときは、少し範囲を広げてください。  

* start_on_run (default=1)
  アプリケーション実行時に処理を開始します。  

### [MAPPING]

MIDIのノートをキーに対応させたもの。  
SHIFTキーや、CTRLキーなど、同時に押したいときは、押す順番にスペースで挟んでください。

### その他

* 実行ファイル名を変更した時は、iniファイルの名前も変えてください。  
  例）  
  　BardPlay.exe の時は bardplay.ini  
  　BardPlay2.exe の時は bardplay2.ini

* iniファイルは、Python版 BardPlay.py ([BardPlay 0.9x](https://github.com/TakeHider/BardPlayPy))と互換性があります。

## EXEファイルの作り方

ファイルサイズを小さくするため、下記のコマンドで作成しています。

```cmd
> go build -ldflags "-H=windowsgui -s -w" BardPlay.go 
```

fyneのツールを入れることで、アイコン付実行ファイルを作ることもできます。  
ただし、ファイルサイズはさらに大きくなります。

```cmd
> go install fyne.io/fyne/cmd/fyne
> fyne package -os windows -icon icon.png
```

今回のアイコンは`StabilityAI`で作りました。    
アイコンの呪文は `A bard girl with a violin in Japanese girl anime style white background` です。

## その他

Pythonで作成した前バージョンが余りにパフォーマンスが悪かったので、無料で使えるコンパイラ言語で焼き直すことにしました。    
DelphiかGO言語か迷ったのですが、好奇心でGO言語で作ることにしました。  
GO言語は初めてだったので、プログラムソースはGO言語らしくない記述が多いかもしれません。  

画面周りのGUIはfyneを使っています。もともと凝った画面を作るつもりでなかったので、これで十分です。  

ネットの情報も豊富ですし、無料でここまで作れるならGO言語で十分ですね。  

しかし！

GO言語で作ったものは、実行ファイル(exe)のサイズがどうしても大きくなってしまいます。  
分かっていたのですが、ここまで大きくなるとは思いませんでした。メモリの使用量もとてつもなく多い。  
これだったら、最初からDelphiで作ればよかった。  

次のバージョンは、きっとDelphiで書かれていると思います。  

----

# BardPlay 1.0 (BardPlay GO)

BardPlay (C) 2022 TakeHide Soft.  
TakeHider@outlook.com

## Overview

[FF14 compatible] Converts information from MIDI devices into PC keyboard events and transmits them.  

I made a Python version, but I didn't get the performance I needed, so I remade it in GO language.   
Since it was created in GO language, the file size of the executable file will be large.  

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

## Libraries required by GO language

 In addition to the standard GO language development environment, the following libraries are required separately.  

* fyne.io
* path/filepath
* gopkg.in/ini.v1

Installation 

```cmd
> CD <Project Folder>
> go get fyne.io/fyne
> go get gopkg.in/ini.v1
```

## Other required libraries

* MIDIIO.dll - MIDIIO library (C)2002-2022 scrap [Open MIDI project](https://openmidiproject.osdn.jp/MIDIIOLibrary.html) (https://openmidiproject.osdn.jp/MIDIIOLibrary.html )

Store MIDIIO.dll in the same folder as the project or in a folder with a path.

## Execution

Connect the MIDI device to the PC in advance, then execute the following command.

```cmd
> cd <Project Folder>
> go run BardPlay.go
```

The screen will show the MIDI device name.   
If you get an error, or if you have multiple MIDI devices connected and an unintended device is displayed, try changing the port number (port_in) in the ini file.  

### Screen button operation

* START
  Executes conversion processing of MIDI event -> keyboard input information.
* STOP
  Stop converting MIDI event ⇒ keyboard input information.
* QUIT
  Quit the application.

## ini File

### [CONFIG]

* port_in (default=1)  
  MIDI port number to use.   
  If you have multiple MIDI devices, try changing the numbers.  

* exit_outrange (default=1)  
  Stop processing when a note outside the keymapped range is played.   
  This option has no effect if you specify 0.   
  If you specify a number greater than or equal to 1, processing will stop when the specified number of out-of-range notes are played.   
  A value of 1 activates notes immediately outside the range. Widen the range slightly when you are likely to accidentally press a keyboard instrument.  

* start_on_run (default=1)  
  Start processing when the application is run.

### [MAPPING]

A MIDI note that corresponds to a key.   
If you want to press the SHIFT key, CTRL key, etc. at the same time, put a space between them in the order you press them.  

### memo

* When you change the name of the executable file, change the name of the ini file as well.  
  ex.)    
    BardPlay.exe  --> bardplay.ini    
    BardPlay2.exe --> bardplay2.ini  

* The ini file is compatible with the Python version of BardPlay.py ([BardPlay 0.9x](https://github.com/TakeHider/BardPlayPy)).

## How to make an EXE file

In order to reduce the file size, it is created with the following command.

```cmd
> go build -ldflags "-H=windowsgui -s -w" BardPlay.go 
```

fBy installing the fyne tool, you can create an executable file with an icon. However, the file size will be even larger.  

```cmd
> go install fyne.io/fyne/cmd/fyne
> fyne package -os windows -icon icon.png
```

This icon was made with `StabilityAI`.   
The spell on the icon is `A bard girl with a violin in Japanese girl anime style white background`.  

## Other

The previous version, written in Python, performed so poorly that I decided to rewrite it in a freely available compiler language.  
I was torn between Delphi and GO, but out of curiosity, I decided to use GO.  
Since it was my first time using the GO language, the program source may have many descriptions that are not typical of the GO language.  
The GUI around the screen uses fyne. Originally I didn't intend to make an elaborate screen, so this is enough.  
There is plenty of information on the internet, and if you can make this far for free, GO language is enough.  

However!  

The size of the executable file (exe) will inevitably become large for those created in the GO language.  
I knew it, but I never thought it would get this big. Memory usage is too high.  
If this were the case, I should have made it in Delphi from the beginning.  
The next version will probably be written in Delphi.  

---

## MIDI Note Infomation (.ini File [MAPPING] Section )  

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
