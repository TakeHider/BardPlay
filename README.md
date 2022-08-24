# BardPlay

Midi Event to PC-Keyboard Event for FF14

## Python Version

python 3.x

## Python Library

* PyAutoGUI
* Pygame

```cmd
$ pip install pyautogui
$ pip install pygame
```

```cmd
> python -m pip install pyautogui
> python -m pip install pygame
```

## その他
MIDI信号の受信用に`pygame`を使っています。
信号の受信だけなのでToo Muchなのですが、
`mido`だと、他に`rtmidi` や `Microsoft Visual C++ 14.0` が必要になるため断念。

