# BardPlay 1.3 (BardPlay Delphi)

BardPlay (C) 2022 TakeHide Soft.  
TakeHider@outlook.com

## Overview

[FF14 compatible] Converts information from MIDI devices into PC keyboard events and transmits them.  
For example, if you put the bard in a performance mode, you can enjoy playing from a MIDI instrument connected to your PC.  
Due to the specifications, multiple sounds cannot be played at the same time.

## Revised contents
### 1.3.0
- Changed MIDI event reception processing to buffering.  
   Greatly improved response.  
- Implemented a function that plays in order from the lowest note when trying to play multiple notes at the same time. (*not recommended*)

### 1.2.0
- Change icon.
- Added a function to transpose in octave units.
- Review of multithreading for stability improvement.
- Made it possible to use `PostMessage` instead of `SendMessage` in the ini settings. (*not recommended*)
- Fixed a bug that rarely exits when `exit_outrange` is specified


## Execution

Connect the MIDI device to your PC in advance, and then run the application.

When you run the application, you will see the MIDI device name on the screen.  
If you get an error, make sure your MIDI device is connected correctly and powered on, and try pressing the reload button.  
If multiple MIDI devices are connected and an unintended device is displayed, select the target device from the list.

The `Start` button cannot be pressed if there is a connection error with the MIDI device.


### Screen operation

![Image](.\README.png)

1. MIDI Devices  
  Select the MIDI device to use.  
  Connect your MIDI device to your PC in advance.
2. Start  
  Executes conversion processing of MIDI event -> keyboard input information.  
  Once the process is done, the Start button will switch to a Stop button.  
  In this state, if you play the selected MIDI device, the information of the corresponding PC key will be transmitted.
3. Stop  
  Stop converting MIDI event -> keyboard input information.  
  When processing is stopped, the Stop button will switch to a Start button.
4. Quit  
  Stops running processes and terminates the application.
5. Refresh  
  Search for MIDI devices again.
6. Transpose  
  Select here if you want to transpose in octave steps.  
  You can specify from -36 to +36 in steps of 12 (1 octave).
7. Start on Run  
  If you want to start the process when the application starts, check it.


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

#### This software uses MIDIIO Library (MIDIIO.dll).

The copyright of the MIDIIO library is held by "(C)2002-2012 Kuzu / Open MIDI Project".  
 https://openmidiproject.osdn.jp/MIDIIOLibrary.html   
Distributed under LGPL license  

#### Using Icons 8 for the button image.

The image of the image button uses [Icons 8](https://icons8.com)(https://icons8.com).


## ini File

### [CONFIG]

* device_name  
  Retains the name of the last connected MIDI device.  
  When using for the first time, select from the list on the screen.  

* exit_outrange (default=1)  
  Stop processing when a note outside the keymapped range is played.  
  This option has no effect if you specify 0.  
  If you specify a number greater than or equal to 1, processing will stop when the specified number of notes outside the mapped range are played.  
  For example, if you specify `1`, the sound immediately outside the range will be effective. (The process stops as soon as the half-out sound is produced.)  
  Widen the range slightly when you are likely to accidentally press a keyboard instrument.  

* start_on_run (default=0)  
  Start processing when the application is run.  
  However, if the MIDI device is not properly connected, it will not run.

* transpose (default=0)
   On certain MIDI devices, the middle C note is not `C4(noteNo.60 - 261.6hz)`, but one octave lower or higher.  
   You can adjust the pitch by specifying an octave in the range of `-3` to `3`.

* virtual_chords (default=0)
   When trying to produce multiple sounds at the same time, such as in a chord, the sounds will be produced in order from the lowest note.  
   However, it is difficult to "put out at the same time", and it is almost a matter of feeling.  
   It was deprecated because it could not be sufficiently tested when mixed with note-off information.  


### [MAPPING]

A MIDI note that corresponds to a key.  
Specify the key you want to assign to the corresponding MIDI note.  
(MIDI notes are listed at the end of the README.)  
If you want to press multiple keys at the same time using the `SHIFT` or `CTRL` key, separate them with spaces in the order you press them.
Example) `SHIFT` + `S` key -> `shift s`  
If you specify the reverse (`s shift`), the `shift` key will be pressed after pressing the `s` key, so there is a possibility that the intended sound will not be produced.
Special keys conform to Python's `pyautogui`, but not all special keys are supported.  
Surely the `shift` and `ctrl` keys will suffice.  
Please refer to [end](#reference) for corresponding keys.

### About ini file name

* When you change the name of the executable file, please change the name of the ini file as well.  
   example)    
   　When using BardPlay.exe, bardplay.ini  
   　When using BardPlay2.exe, bardplay2.ini  

* The ini file is Python version BardPlay.py (`BardPlay 0.9x`), GO language version BardPlay.go (`BardPlay 1.0x`).  
   However, the [CONFIG] section has options that are no longer used in this version.  




## Other

### Support for MIDI devices

Operation has been confirmed with the following devices.  
* CACIO LK-511 61-key electronic keyboard optical navigation  
* YAMAHA SHS-300 Sonogenic 37-key shoulder keyboard  

I have checked the MIDI specifications and tried to support each company, but I have not been able to test all manufacturers' devices, so there may be devices that do not work properly.  
In that case, please contact us and we will do our best to accommodate you.  
(`Note OFF` supports two types: `0x80 - 0xXX` and `0x90 - 0x00`)  

If you are using it in FF14 etc. and the sound does not stop, please try to press the corresponding PC-Key without rushing.  

### latest files and source code
BardPlay publishes the source code on GitHub.  
In addition to the Delphi version (1.1.x or later), Python version (0.9.x) and GO language version (1.0.x), which have been developed, are also uploaded, so if you are interested in program development, please take a look. please give me.  
The latest release is also published on GitHub, so if you have downloaded it from another site, please check GibHub.  
[https://github.com/TakeHider/BardPlay](https://github.com/TakeHider/BardPlay)

## change history

|Version|Release Date|Contents|
|:--|:-:|:--|
|1.3.0|2022/9/24|Improved response|
|1.2.0|2022/9/19|Added transpose function by 1 octave unit<br/> Improved performance and fixed minor bugs|
|1.1.0|2022/9/15|Created Delphi version|


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
|:-------:| -----------:| :---------:|
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
