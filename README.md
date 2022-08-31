# BardPlay

Midi Event to PC-Keyboard Event  
FF14 compatible  

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
<<<<<<< HEAD

=======
>>>>>>> 44c7dc8a6336be146a51a7f53810fa5070b2dccc
```cmd
> python -m pip install pyautogui
> python -m pip install pygame
```

<<<<<<< HEAD
## Run

=======
## Run  
>>>>>>> 44c7dc8a6336be146a51a7f53810fa5070b2dccc
```cmd
>python BardPlay.py
```

<<<<<<< HEAD
## その他

MIDI信号の受信用に`pygame`を使っています。  
信号の受信だけなのでToo Muchなのですが、`mido`だと、他に`rtmidi` や `Microsoft Visual C++ 14.0` が必要になるため断念。  

=======

## その他
MIDI信号の受信用に`pygame`を使っています。  
信号の受信だけなのでToo Muchなのですが、`mido`だと、他に`rtmidi` や `Microsoft Visual C++ 14.0` が必要になるため断念。  

>>>>>>> 44c7dc8a6336be146a51a7f53810fa5070b2dccc
思っていた以上に動作がもっさりしています。  
Windows用に割り切って、キーイベント送信処理を、GyAutoGUIからPostMessageに変更したバージョンを作成予定です。  
