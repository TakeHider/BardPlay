# BardPlay 0.9

BardPlay.py (C) 2022 TakeHide Soft.
TakeHideSoft@outlook.com

## 概要

【FF14対応】 MIDIデバイスからの情報を、PCキーボードのイベントに変換して送信します。

Windowsで動作確認をしていますが、おそらくMacでも動くと思います。

キーボードイベントのメッセージ送信にPyAutoGUIを使っているのですが、PyAutoGUIはパフォーマンスより確実性重視で作られているためか、動作がもっさりしています。
ゆったりとした曲なら対応できますが、通常の演奏には耐えられません。

## 

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

## Pythonバージョン

このソフトウェアはPython 3.10 で書かれています。
Ptyhon 3.10独自のコードは使われていないので、Python 3.xであれば動かすことができます。

## Pythonライブラリ

このソフトウェアを実行するには、下記のライブラリが必要です。

* PyAutoGUI
* Pygame

インストール方法

```cmd
$ pip install pyautogui
$ pip install pygame
```

Windowsでのインストール方法

```cmd
> python -m pip install pyautogui
> python -m pip install pygame
```

## 実行方法

あらかじめMIDIデバイスをPCに接続しておきます。

```cmd
> cd ≪プロジェクトフォルダ≫
>python BardPlay.py
```

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

### [MAPPING]

MIDIのノートをキーに対応させたもの。  
SHIFTキーや、CTRLキーなど、同時に押したいときは、押す順番にスペースで挟んでください。

### 

## その他

MIDI信号の受信用に`pygame`を使っています。  
信号の受信だけなのでToo Muchなのですが、`mido`だと、他に`rtmidi` や `Microsoft Visual C++ 14.0` が必要になるため断念。

実際に使ってみると、`PyAutoGUI`を使ったメッセージ送信のパフォーマンスが悪く、動作がとてももっさりしてしまいました。
Python＋ライブラリを使ったプログラムでは限界があるので、Windows限定で見切って、メッセージ送信処理を user32.dllのSendMessageに替えてみようかと思います。



----

# BardPlay 0.9

BardPlay.py (C) 2022 TakeHide Soft.
TakeHideSoft@outlook.com

## Overview

[FF14 compatible] Converts information from MIDI devices into PC keyboard events and transmits them. 

I've tested it on Windows, but I'm sure it will work on Mac as well. 

I'm using PyAutoGUI to send keyboard event messages, but PyAutoGUI's behavior is sluggish, probably because it's made with more emphasis on certainty than performance. 

If it's a slow song, it can handle it, but it won't hold up to normal playing. 

## Licence

This software is distributed under the GNU Lesser General Public License (GNU LGPL).

* You may copy, reprint, and distribute this software under the GNU LGPL.

* You may modify this software and distribute it under the GNU LGPL.

* You can use this software to create and distribute your own licensed applications.

* In either case, you don't need to get permission from the author.

* This software comes with absolutely no warranty.
  The author is not responsible for any damages resulting from using this library.
  Please note.

For details, see GNU LGPL [License - GNU Project - Free Software Foundation](http://www.gnu.org/licenses/) .

## Python version

This software is written in Python 3.10. 

Ptyhon 3.10 specific code is not used, so you can run it with Python 3.x. 

## Python library

The following libraries are required to run this software. 

* PyAuto GUI 

* Pygame 

Installation 

```cmd
$ pip install pyautogui
$ pip install pygame
```

Installation on Windows

```cmd
> python -m pip install pyautogui
> python -m pip install pygame
```

## Execution

Connect your MIDI device to your PC in advance.

```cmd
> cd <Project Folder>
>python BardPlay.py
```

## ini File

### [CONFIG]

* port_in (default=1)
  MIDI-Port number to use.
  If you have multiple MIDI devices, try different numbers.

* exit_outrange (default=1)
  If you make a sound outside the range, the process will stop.
  Enter a number greater than or equal to 1 to enable.
  The numbers indicate how far away from the boundary values.

### [MAPPING]

A MIDI note that corresponds to a key.

If you want to press the SHIFT key, CTRL key, etc. at the same time, put a space between them in the order you press them.

Notes not specified here will be the range of exit_outrange.

## Others

 I am using `pygame` for receiving MIDI signals. 

It's too much because it only receives signals, but with `mido`, you need `rtmidi` and `Microsoft Visual C++ 14.0`, so I gave up. 

When I actually used it, the performance of sending messages using `PyAutoGUI` was bad and the operation was very slow. 

Programs using Python + libraries have limitations, so I'm thinking of changing the message sending process to SendMessage in user32.dll instead of using Windows only.



---

## MIDI Info

BardPlay.ini 
[MAPPING]Section Key Number

| Tone   | Note Number | .ini File |
|:------:| -----------:| --------- |
| C-1    | 0           | ×:None    |
| C#     | 1           | ×:None    |
| D      | 2           | ×:None    |
| D#     | 3           | ×:None    |
| E      | 4           | ×:None    |
| F      | 5           | ×:None    |
| F#     | 6           | ×:None    |
| G      | 7           | ×:None    |
| G#     | 8           | ×:None    |
| A      | 9           | ×:None    |
| A#     | 10          | ×:None    |
| B      | 11          | ×:None    |
| C0     | 12          | ×:None    |
| C#     | 13          | ×:None    |
| D      | 14          | ×:None    |
| D#     | 15          | ×:None    |
| E      | 16          | ×:None    |
| F      | 17          | ×:None    |
| F#     | 18          | ×:None    |
| G      | 19          | ×:None    |
| G#     | 20          | ×:None    |
| A      | 21          | ×:None    |
| A#     | 22          | ×:None    |
| B      | 23          | ×:None    |
| C1     | 24          | ×:None    |
| C#     | 25          | ×:None    |
| D      | 26          | ×:None    |
| D#     | 27          | ×:None    |
| E      | 28          | ×:None    |
| F      | 29          | ×:None    |
| F#     | 30          | ×:None    |
| G      | 31          | ×:None    |
| G#     | 32          | ×:None    |
| A      | 33          | ×:None    |
| A#     | 34          | ×:None    |
| B      | 35          | ×:None    |
| C2     | 36          | ×:None    |
| C#     | 37          | ×:None    |
| D      | 38          | ×:None    |
| D#     | 39          | ×:None    |
| E      | 40          | ×:None    |
| F      | 41          | ×:None    |
| F#     | 42          | ×:None    |
| G      | 43          | ×:None    |
| G#     | 44          | ×:None    |
| A      | 45          | ×:None    |
| A#     | 46          | ×:None    |
| B      | 47          | ×:None    |
| **C3** | **48**      | ○:Set     |
| C#     | 49          | ○:Set     |
| D      | 50          | ○:Set     |
| D#     | 51          | ○:Set     |
| E      | 52          | ○:Set     |
| F      | 53          | ○:Set     |
| F#     | 54          | ○:Set     |
| G      | 55          | ○:Set     |
| G#     | 56          | ○:Set     |
| A      | 57          | ○:Set     |
| A#     | 58          | ○:Set     |
| B      | 59          | ○:Set     |
| **C4** | **60**      | ○:Set     |
| C#     | 61          | ○:Set     |
| D      | 62          | ○:Set     |
| D#     | 63          | ○:Set     |
| E      | 64          | ○:Set     |
| F      | 65          | ○:Set     |
| F#     | 66          | ○:Set     |
| G      | 67          | ○:Set     |
| G#     | 68          | ○:Set     |
| A      | 69          | ○:Set     |
| A#     | 70          | ○:Set     |
| B      | 71          | ○:Set     |
| **C5** | **72**      | ○:Set     |
| C#     | 73          | ○:Set     |
| D      | 74          | ○:Set     |
| D#     | 75          | ○:Set     |
| E      | 76          | ○:Set     |
| F      | 77          | ○:Set     |
| F#     | 78          | ○:Set     |
| G      | 79          | ○:Set     |
| G#     | 80          | ○:Set     |
| A      | 81          | ○:Set     |
| A#     | 82          | ○:Set     |
| B      | 83          | ○:Set     |
| **C6** | **84**      | ○:Set     |
| C#     | 85          | ×:None    |
| D      | 86          | ×:None    |
| D#     | 87          | ×:None    |
| E      | 88          | ×:None    |
| F      | 89          | ×:None    |
| F#     | 90          | ×:None    |
| G      | 91          | ×:None    |
| G#     | 92          | ×:None    |
| A      | 93          | ×:None    |
| A#     | 94          | ×:None    |
| B      | 95          | ×:None    |
| C7     | 96          | ×:None    |
| C#     | 97          | ×:None    |
| D      | 98          | ×:None    |
| D#     | 99          | ×:None    |
| E      | 100         | ×:None    |
| F      | 101         | ×:None    |
| F#     | 102         | ×:None    |
| G      | 103         | ×:None    |
| G#     | 104         | ×:None    |
| A      | 105         | ×:None    |
| A#     | 106         | ×:None    |
| B      | 107         | ×:None    |
| C8     | 108         | ×:None    |
| C#     | 109         | ×:None    |
| D      | 110         | ×:None    |
| D#     | 111         | ×:None    |
| E      | 112         | ×:None    |
| F      | 113         | ×:None    |
| F#     | 114         | ×:None    |
| G      | 115         | ×:None    |
| G#     | 116         | ×:None    |
| A      | 117         | ×:None    |
| A#     | 118         | ×:None    |
| B      | 119         | ×:None    |
| C9     | 120         | ×:None    |
| C#     | 121         | ×:None    |
| D      | 122         | ×:None    |
| D#     | 123         | ×:None    |
| E      | 124         | ×:None    |
| F      | 125         | ×:None    |
| F#     | 126         | ×:None    |
| G      | 127         | ×:None    |
