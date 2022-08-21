#!/usr/bin/env python3

# Midi Event to PC-Keyboard-Event for FF14

import os 
import sys
import configparser
import pyautogui   as pauto
import pygame.midi as pygame

NOTE_ON     = 0x90
NOTE_OFF    = 0x80
INI_FILE    = 'MidiTokey.ini'

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
    # 指定された順番に押す
    for key in sendkeys.split(' '):
        pauto.keyDown(key)

def send_off(sendkeys):
    # 指定された順番の逆から離す
    for key in reversed(sendkeys.split(' ')):
        pauto.keyUp(key)

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
            midi_events = midi_in.read(1)   # 一応5イベントまで取得
            for event in midi_events:
                status  = event[0][0]  # イベント
                data1   = event[0][1]  # 音
                data2   = event[0][2]  # 強さ(64はOFF)
                
                if status == NOTE_ON or status == NOTE_OFF:

                    if key_min <= data1 <= key_max:
                        # マッピング範囲内の時
                        if status == NOTE_ON:           # ノート ON
                            # 前に音が鳴っていたら消す
                            if pre_event != '':
                                send_off(pre_event)
                            # 指定されたキーを押す
                            send_on(key_map[data1])
                            pre_event = key_map[data1]
                        elif status == NOTE_OFF:        # ノート OFF
                            # 指定されたキーを離す
                            send_off(key_map[data1])
                            pre_event = ''
                    else:
                        # 範囲外のとき
                        if exit_outrange > 0: 
                            # 範囲外の音が出たときに止める指定がされていたら、ループを抜ける
                            if  (data1 - exit_outrange ) < key_min or (data1 + exit_outrange ) > key_max:
                                loop = False
    pygame.quit()
    sys.exit()