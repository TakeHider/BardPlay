#!/usr/bin/env python3


import pyautogui   as pauto
import pygame.midi as pmidi

NOTE_ON     = 0x90
NOTE_OFF    = 0x80
KEY_MAP     = list('q2w3er5t6y7u'*10)   # キーボードのマッピング
KEY_OCT_UP  = 'shift'                   # 1オクターブUP
KEY_OCT_DOWN= 'ctrl'                    # 1オクターブDOWN

# デバイス情報を表示する
def getDeviceInfo():
    pmidi.init()
    i_num = pmidi.get_count()
    for i in range(i_num):
        print(i)
        print(pmidi.get_device_info(i))
    pmidi.quit()

# MIDI楽器からの情報をPCのキーイベントに変換する
# すると「143（ミディノートオフ）」「48」が選択された状態になるので
# 赤枠をクリックし「159（ヌーンノート）」を選び直します。

# メイン関数
if __name__=='__main__':

    # MIDIの初期化
    pmidi.init()
    # デフォルトのポートを取得する
    port_i = pmidi.get_default_input_id()
    print(port_i)

    # MIDI入力
    midi_in = pmidi.Input(port_i)

    count = 0
    loop = True
    while loop:
        if midi_in.poll():
            midi_events = midi_in.read(4)
            print ("full midi_events:" + str(midi_events))
            status  = midi_events[0][0][0]  # イベント
            data1   = midi_events[0][0][1]  # 音
            data2   = midi_events[0][0][2]  # 強さ(64はOFF)
            if status == NOTE_ON or status == NOTE_OFF:
                if data1 >= 48 and data1 <= 84:     # 音の範囲は C3～C6
                    if status == NOTE_ON:
                        if data1 < 60:  # C4未満
                            pauto.keyDown(KEY_OCT_DOWN)
                        elif data1 >=72: # C5を超える
                            pauto.keyDown(KEY_OCT_UP)
                        pauto.keyDown(KEY_MAP[data1])              
                    elif status == NOTE_OFF:
                        pauto.keyUp(KEY_MAP[data1])              
                        if data1 < 60:  # C4未満
                            pauto.keyUp(KEY_OCT_DOWN)
                        elif data1 >=72: # C5を超える
                            pauto.keyUp(KEY_OCT_UP)
                elif data1 <=36 or data1 >= 96:
                    loop = False

    

