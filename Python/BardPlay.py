#!/usr/bin/env python3

# -------------
# BardPlay 0.9.1
# -------------

# FileDescription   Midi Event to PC-Keyboard-Event (BardPlay Python)
# InternalName      BardPlay.py
# FileVersion       0.9.1

# LegalCopyright    (C) 2022 TakeHide Soft.
# ContactUs         TakeHider@outlook.com

import platform
import os 
import sys
import configparser
from ctypes import *
import pyautogui   as pauto
import pygame.midi as pygame


NOTE_ON     = 0x90
NOTE_OFF    = 0x80
INI_FILE    = 'BardPlay.ini'

OS_WINDOWS  = 'WIN'
OS_MAC      = 'MAC'
OS_LINUX    = 'LINUX'
OS_UNKNOWN  = 'UNKNOWN'


# OSの判定
def getOS():
    pf = platform.system()
    if pf == 'Windows':
        return OS_WINDOWS
    elif pf == 'Darwin':
        return OS_MAC
    elif pf == 'Linux':
        return OS_LINUX
    else:
        return OS_UNKNOWN
        
# Windowsの場合はWin32APIを使う
if getOS() == OS_WINDOWS:
    SendMessage         = windll.user32.SendMessageW
    GetForegroundWindow = windll.user32.GetForegroundWindow

    WM_KEYUP   = 0x101  # メッセージイベント KeyUp
    WM_KEYDOWN = 0x100  # メッセージイベント KeyDown


# デバイス情報を表示する
def getDeviceInfo():
    # MIDIの初期化
    pygame.init()
    # デバイスの数を取得
    i_num = pygame.get_count()
    # デバイスの表示
    print('Device Infomation')
    for i in range(i_num):
        info = pygame.get_device_info(i)
        print("  #{:d} : {:s} {:s}".format(i, str(info[2:5]),str(info[1])[2:-1]))
    # MIDIを閉じる
    pygame.quit()
    

# 音を出す(キーを押す)
def send_on(sendkeys):
    OS = getOS()
    if OS == OS_WINDOWS:
        hwnd = GetForegroundWindow()
        
    # 指定された順番に押す
    for key in sendkeys.split(' '):
        if OS == OS_WINDOWS:
            SendMessage( hwnd, WM_KEYDOWN, KEY_CODE[key],0)
        else:
            pauto.keyDown(key)

def send_off(sendkeys):
    OS = getOS()
    if OS == OS_WINDOWS:
        hwnd = GetForegroundWindow()

    # 指定された順番の逆から離す
    for key in reversed(sendkeys.split(' ')):
        if OS == OS_WINDOWS:
            SendMessage( hwnd, WM_KEYUP, KEY_CODE[key],0)
        else:
            pauto.keyUp(key)

# SendMessageで送信するキーコード
KEY_CODE = {
    "backspace" : 0x08,
    "tab"       : 0x09,
    "enter"     : 0x0D,
    "shift"     : 0x10,
    "ctrl"      : 0x11,
    "alt"       : 0x12,
    "pause"     : 0x13,
    "capslock"  : 0x14,
    "esc"       : 0x1B,
    "space"     : 0x20,
    "pageup"    : 0x21,
    "pagedown"  : 0x22,
    "end"       : 0x23,
    "home"      : 0x24,
    "left"      : 0x25,
    "up"        : 0x26,
    "right"     : 0x27,
    "down"      : 0x28,
    "printscrn" : 0x2C,
    "insert"    : 0x2D,
    "delete"    : 0x2E,
    "0"         : 0x30,
    "1"         : 0x31,
    "2"         : 0x32,
    "3"         : 0x33,
    "4"         : 0x34,
    "5"         : 0x35,
    "6"         : 0x36,
    "7"         : 0x37,
    "8"         : 0x38,
    "9"         : 0x39,
    "a"         : 0x41,
    "b"         : 0x42,
    "c"         : 0x43,
    "d"         : 0x44,
    "e"         : 0x45,
    "f"         : 0x46,
    "g"         : 0x47,
    "h"         : 0x48,
    "i"         : 0x49,
    "j"         : 0x4A,
    "k"         : 0x4B,
    "l"         : 0x4C,
    "m"         : 0x4D,
    "n"         : 0x4E,
    "o"         : 0x4F,
    "p"         : 0x50,
    "q"         : 0x51,
    "r"         : 0x52,
    "s"         : 0x53,
    "t"         : 0x54,
    "u"         : 0x55,
    "v"         : 0x56,
    "w"         : 0x57,
    "x"         : 0x58,
    "y"         : 0x59,
    "z"         : 0x5A,
    "f1"        : 0x70,
    "f2"        : 0x71,
    "f3"        : 0x72,
    "f4"        : 0x73,
    "f5"        : 0x74,
    "f6"        : 0x75,
    "f7"        : 0x76,
    "f8"        : 0x77,
    "f9"        : 0x78,
    "f10"       : 0x79,
    "f11"       : 0x7A,
    "f12"       : 0x7B
}

# メイン関数
if __name__=='__main__':
    # -----------------------------------------------------------------------------
    # 引数のチェック
    args = sys.argv
    if len(args)>=2:
        if args[1]=='-d' or args[1]=='--device_info':
            getDeviceInfo()
            exit()
        elif args[1]=='/?' or args[1]=='-?':          
            print('BardPlay.py Copyright (C) 2022 TakeHide Soft.')
            print('This program comes with ABSOLUTELY NO WARRANTY. ')
            print('This is free software, and you are welcome to redistribute it under certain conditions.')
            print('-----------------')            
            print('USASE')
            print('\t>python '+ args[0] + ' -d     ... Show Device Information.')   
        else:
            print(args)

    # -----------------------------------------------------------------------------
    # Config.iniから取得
    if not os.path.exists(INI_FILE):
        raise FileNotFoundError(errno.ENOENT, os.strerror(errno.ENOENT), INI_FILE)
    config_ini = configparser.ConfigParser()
    config_ini.read(INI_FILE, encoding='utf-8')
    
    # 設定ファイルの読み込み
    # port_in = config_ini.get('CONFIG','port_in') if config_ini.has_option('CONFIG','port_in') else -1
    if config_ini.has_option('CONFIG','port_in'):
        port_in = int(config_ini.get('CONFIG','port_in'))
    else:
        port_in =-1
    if config_ini.has_option('CONFIG','exit_outrange'):
        exit_outrange = int(config_ini.get('CONFIG','exit_outrange'))
    else:
        exit_outrange = -1
    # STOPボタン
    if config_ini.has_option('CONFIG','exit_note'):
        exit_note = config_ini.get('CONFIG','exit_note').strip()
    else:
        exit_note = ''
    if exit_note != '':
        exit_status = int(exit_note.split('-')[0].strip())
        exit_data   = int(exit_note.split('-')[1].strip()) if '-' in exit_note else 0
    else:
        exit_status = 0
        exit_data   = 0
        

    # キーのマッピング
    key_map = list(' '*127)
    key_min = 0
    key_max = 0
    for n in range(127):
        if config_ini.has_option('MAPPING',str(n)):
            # マッピングを取得
            key_map[n] = str(config_ini.get('MAPPING',str(n))).strip()    
            # マッピングの上限と下限を取得
            if key_min == 0:
                key_min = n
            key_max = n
        else:
            key_map[n]=''
    # -----------------------------------------------------------------------------

    # MIDIの初期化
    pygame.init()

    # MIDIって繋がってるんだっけ？
    if not pygame.get_init():
        print("MIDI Device not connected.")
        print("Please connect MIDI device to your PC.")
        exit(1)

    # デフォルトのポートを取得する
    if port_in == -1:
        port_i = pygame.get_default_input_id()
    else:
        port_i = port_in

    # MIDI入力
    midi_in = pygame.Input(port_i)

    loop = True
    pre_event = ''
    while loop:
        if midi_in.poll():
            midi_events = midi_in.read(5)   # 一応5イベントまで取得
            for event in midi_events:
                status  = event[0][0]  # イベント
                data1   = event[0][1]  # 音
                data2   = event[0][2]  # 強さ(64はOFF)
                
                if status == NOTE_ON or status == NOTE_OFF:

                    if key_min <= data1 <= key_max:
                        # マッピング範囲内の時
                        if status == NOTE_ON and data2 != 0x00:      # ノート ON
                            # 前に音が鳴っていたら消す
                            if pre_event != '' and pre_event != key_map[data1]:
                                send_off(pre_event)
                              
                            # 今と異なる状態だったら、指定されたキーを押す
                            if pre_event != key_map[data1]:
                                send_on(key_map[data1])
                                pre_event = key_map[data1]
                        elif status == NOTE_OFF or (status == NOTE_ON and data2 == 0x00):        # ノート OFF
                            # 今押されているノートと同じだった時に放す
                            if pre_event == key_map[data1]:
                                send_off(key_map[data1])
                                pre_event = ''
                    else:
                        # 範囲外のとき
                        if exit_outrange > 0: 
                            # 範囲外の音が出たときに止める指定がされていたら、ループを抜ける
                            if  (data1 - exit_outrange ) < key_min or (data1 + exit_outrange ) > key_max:
                                loop = False
                elif exit_status > 0:
                    if exit_status == status and (data1 if exit_data >0 else -1) == exit_data:
                        loop = False
                else:
                    print('STATUS:{:3d}-note:{:3d}'.format(status,data1)) 
    # もし何か鳴っていたら止める
    if pre_event != '':
        send_off(pre_event)
        pre_event = ''
    # 諸々閉じる    
    pygame.quit()
    sys.exit()
