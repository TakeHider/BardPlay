# BardPlay python

Midi Event to PC-Keyboard Event  
FF14 compatible  

**Python is slow and has poor performance.**  
**You can play it slowly, but**  
**If you are a Windows user, please use the GOlang version of BardPlay ([BardPlayGO](https://github.com/TakeHider/BardPlayGo)).**

## Python Version

python 3.x  

## Python Library

* PyAutoGUI  
* Pygame  

```cmd
$ pip install pyautogui
$ pip install pygame
```

Windows  

```cmd
> python -m pip install pyautogui
> python -m pip install pygame
```

## Run

```cmd
>python BardPlay.py
```

## その他

MIDI信号の受信用に`pygame`を使っています。  
信号の受信だけなのでToo Muchなのですが、`mido`だと、他に`rtmidi` や `Microsoft Visual C++ 14.0` が必要になるため断念。  

思っていた以上に動作がもっさりしています。  
Windows用に割り切って、キーイベント送信処理を、GyAutoGUIからPostMessageに変更してみようかと思います。  
→思い切ってGO言語で作ってみました。Python版はニーズがあれば手を入れていこうと思います。    
