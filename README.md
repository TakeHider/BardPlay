# BardPlay 0.9

BardPlay.py (C) 2022 TakeHide Soft.

## 概要

【FF14対応】 MIDIデバイスからの情報を、PCキーボードのイベントに変換して送信します。

Windowsで動作確認をしていますが、おそらくMacでも動くと思います。

キーボードイベントのメッセージ送信にPyAutoGUIを使っているのですが、PyAutoGUIはパフォーマンスより確実性重視で作られているためか、動作がもっさりしています。

ゆったりとした曲なら対応できますが、通常の演奏には耐えられません。
Windowsユーザであれば、GO言語で焼き直した [BardPlay 1.0](https://github.com/TakeHider/BardPlayGo) をお勧めします。

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

## その他

MIDI信号の受信用に`pygame`を使っています。  
信号の受信だけなのでToo Muchなのですが、`mido`だと、他に`rtmidi` や `Microsoft Visual C++ 14.0` が必要になるため断念。

実際に使ってみると、`PyAutoGUI`を使ったメッセージ送信のパフォーマンスが悪く、動作がとてももっさりしてしまいました。
Pythonでは限界があるので、Windows限定で見切って、次バージョンは別の言語で作ります。

----

# BardPlay 0.9

BardPlay.py (C) 2022 TakeHide Soft.

## Overview

[FF14 compatible] Converts information from MIDI devices into PC keyboard events and transmits them. 

I've tested it on Windows, but I'm sure it will work on Mac as well. 

I'm using PyAutoGUI to send keyboard event messages, but PyAutoGUI's behavior is sluggish, probably because it's made with more emphasis on certainty than performance. 

If it's a slow song, it can handle it, but it won't hold up to normal playing. If you are a Windows user, we recommend [BardPlay 1.0](https://github.com/TakeHider/BardPlayGo) rewritten in GO language.

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

## Others

 I am using `pygame` for receiving MIDI signals. 

It's too much because it only receives signals, but with `mido`, you need `rtmidi` and `Microsoft Visual C++ 14.0`, so I gave up. 

When I actually used it, the performance of sending messages using `PyAutoGUI` was bad and the operation was very slow. 

Python has its limits, so I will give up on Windows only and make the next version in another language.
