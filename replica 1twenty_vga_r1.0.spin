''    replica 1 TWENTY I/O controller by Briel Computers
''
''    Added ability to change text color with black background F12
''    Added code so if HOME is pressed, backspace does _ out but screen does backspace!
''    Added simple Block graphics capabilities with inverse, home, x,y setting
''    Add cursor chr$ functions
''    As of March 3, 2023 only NTSC is available

''    Cursors:
''    1  =  Block
''    2  =  Block Blink Slow
''    3  =  Block Blink Fast
''    4  =  Underscore
''    5  =  Underscore Blink Slow
''    6  =  Underscore Blink Fast
''    7  =  No Cursor
''    Idendify port pins below
''    P0-P6 are Video Data Bus pins
''    P7 is Data Available from 6821 letting you know there is data
''    P24 is Ready Data Available letting 6821 know you can receive was P8
''    P8-P15 Video port
''    P16-23 is keyboard data out to 6821 P23 is Strobe bit
''    P24 is RDA for video out mode on 6821
''    P25 is RESET circuit to RESET 65816 when determined, such as keystroke like ctrl-F12
''
''    Character set special commands sent to video screen
''    1 chr$(1)                 Clear Screen & Home Cursor
''    2 chr$(2)                 Home cursor
''    4 chr$(4)                 Turn cursor off
''    5 chr$(5)                 Turn cursor on
''    5 chr$(9)                 Inverse characters on/off
''    26 chr$(26)               set x,y location or ctrl-z, next 2 hex values set x and y location to move cursor to
''
''    Keyboard Controls
''    CTRL-F1                   Cycle through cursor options
''    CTRL-F12                  Cycle through screen colors (background always black)
''    HOME                      Clear Screen, Home cursor at top left of screen
''    CTRL-I (9)                Set/Reset Inverse mode 09
''    Shift-Break/Pause         RESET the CPU (same as RESET button on replica 1 board)
''****************************************************************
CON

  _clkmode = xtal1 + pll16x
  _xinfreq = 5_000_000

' Text Colors
  DIMCYAN            = $29
  DIMGREEN           = $27
  DIMORANGE          = $92
  DIMPINK            = $95
  DIMRED             = $C1
  PURPLE             = $99
  CLASSICAMBER       = $A2
  CLASSICAMBERDARK   = $E2
  LIGHTAMBER         = $A5
  WHITE              = $FF
  HOTPINK            = $C9
  PINK               = $D9
  RED                = $C5
  CYAN               = $7F
  DARKBLUE           = $5F
  CLASSICGREEN       = $75
  BRIGHTGREEN        = $22
' DULLYELLOW         = $F0

  video      = 8
  RX_Pin     = 31
  TX_Pin     = 30
  BaudRate   = 9_600
  NUM        = %100
  CAPS       = %010
  SCROLL     = %001
  RepeatRate = 40

VAR
' Byte Index
  Byte Rx
  Word key
  Byte rxbyte
  Long Stack[300],stack1[300],stack2[300]
  Byte temp
  Byte ascii
  Byte keyinuse
  Byte serdata
  Byte textcolor
  Long CLR[18]
  Byte curset
  Byte inverse
  Byte move
  Byte byte1
  Byte byte2
  Byte xtemp
  Byte ytemp
OBJ                                                     ' Driver Files
  text: "VGA_1024v1201"
  Serport: "Serial_IO"
  kb: "keyboard"
PUB   main                                              ' Startup and Video Handler

  move:=0
  textcolor:= 10
  curset := 2
  CLR[1]:=DIMCYAN
  CLR[2]:=DIMGREEN
  CLR[3]:=DIMORANGE
  CLR[4]:=DIMPINK
  CLR[5]:=DIMRED
  CLR[6]:=PURPLE
  CLR[7]:=CLASSICAMBER
  CLR[8]:=CLASSICAMBERDARK
  CLR[9]:= LIGHTAMBER
  CLR[10]:=WHITE
  CLR[11]:=HOTPINK
  CLR[12]:=PINK
  CLR[13]:=RED
  CLR[14]:=CYAN
  CLR[15]:=DARKBLUE
  CLR[16]:=CLASSICGREEN
  CLR[17]:=BRIGHTGREEN
'================================================
'START UP PROPELLER VIDEO ROUTINES
'================================================
  text.start(video)                                     ' video is the starting pin number on Propeller IC
  text.cls(CLR[textcolor])                              ' Clear Video Screen
  text.color(CLR[textcolor])                            ' Set Starting Text Color
  inverse:= 0
  text.inv(inverse)
  text.cursorset(curset)
  move:=0
  text.str(string("20.0"))                              ' Hide version number in start up garbage screenT
 DIRA[25]~                                              ' Set pin going to RESET on 65816 as ~input ~~ is output
'================================================
'START UP SERIAL PORT AND KEYBOARD DRIVERS
'================================================
  Serport.startx(RX_Pin, TX_Pin, BaudRate)              ' Start Serial Port, set baud, pins, etc
  kb.startx(26, 27, NUM+CAPS, RepeatRate)               ' Start Keyboard Driver, set Caps lock
'================================================
'ESTABLISH PORT FOR ASCII KEYBOARD ON 65C21
'================================================
  DIRA[23..16]~                                         ' Set ASCII keyboard port as input
  DIRA[24]~~                                            ' Set RDA was P8 as output pin to RDA signal was RDA
'================================================
'START COGS (CORES) FOR PS2 AND SERIAL DRIVERS
'================================================
  cognew (ps2 , @stack1)
  cognew (serial, @stack2)
  text.fill
'================================================
'AUTO RESET CIRCUIT!
'================================================
  DIRA[25]~~                                            ' Set Pin 25 to Control RESET Line on 65816
  OUTA[25]~                                             ' Pull RESET Low to RESET 65816
  PAUSEMS(20)                                           ' Pause for 20mS to make sure RESET has been made
  OUTA[25]~~                                            ' Make RESET line High to start 65816 CPU
  DIRA[25]~                                             ' Make pin input so no accidental RESET
'================================================
' VIDEO ROUTINE LOOP
'================================================
  Repeat
      temp := 0
      OUTA[24]~~                                        'Turn on RDA was P8 to recieve video data (to RDA)
      temp := INA[7]                                    'Read DA signal from 6521
      if temp >> 0                                      'Is there data out there?
          temp :=INA[6..0]                              'Yes, get video data
          if temp > 127
            temp := 33
          !OUTA[24]                                     'Turn off RDA signal
          if temp == 95                                 ' Is it _ backspace character?
               temp := 08
          if temp == 13 and move == 0                   'Is data a Carriage Return (CR)?
             Serport.out(10)
          if temp == 1  and move == 0                   'home clear screen command?
             text.cls(textcolor)
             text.home
          if temp == 2 and move == 0                    'home cursor, no screen change
             text.home
          if temp == 9 and move == 0                    'inverse character mode? was 5
             if inverse == 0
               inverse:= 1
             else
               inverse:= 0
             text.inv(inverse)
          if temp == 4 and move == 0                    'chr$(4) will turn cursor off
                 text.cursorset(7)
          if temp == 5 and move == 0                    'chr$(5) turn cursor on
                 text.cursorset(curset)

'================================================
'START OF CURSOR POSITION CONTROL CODE
'================================================
          if temp == 26 and move == 0                   'ctrl-z 'Start of x,y cursor move code?
            move:=1
            byte1:=0
            byte2:=0
            temp:= 0                                    'clear the character so it doesn't output anything
          if move > 0 and temp > 0                      'do we have X,Y Command set and are we on more than byte 1 of code
            case move
             $01:
              move++                                    'Are we on X character? Yes, increase vt00, get x value
              xtemp:= temp
              temp:=0
             $02:
               ytemp:= temp                             'we have x and y now set coordinates
               text.rcursor(xtemp,ytemp)
               'text.out(7)
               move:= 0
               xtemp:=ytemp:= 0
               temp:=0

'==============================================
'  SEND DATA TO VGA AND SERIAL PORT
'==============================================
          if temp > 31 or temp == 13                    'Is it printable?
             text.out(temp)
          Serport.out(temp)                             'Send it out serial port


PUB ps2                                                 'PS/2 Keyboard COG

  DIRA[23..16]~                                         'Set up 8 bit port to output data
'================================================
'PS2 KEYBOARD INPUT LOOP ROUTINE
'================================================
  Repeat
      key := kb.getkey                                  'Go get keystroke, then return here
      'text.dec(key)
'================================================       CONVERT THESE TO CASE STATEMENTS FOR CLEANER CODE
'CHECK FOR SPECIAL KEYSTROKE COMMANDS
'================================================
      if key == 720                                     'ctrl-F1 Cursor set
        key:= 0
        curset++
        if curset > 7
          curset:= 1
        text.cursorset(curset)
      if key == 222                                     'caps lock Don't print value
        key:= 0
      if key == 203                                     'escape key?
        key:= 27
      if key == 196                                     ' PS/2 keyboard CLS HOME Key
        key:= 0                                         ' Reset keystroke
        text.cls(textcolor)
        text.home
      if key == 731                                     ' F12 to change color?
        key:=0
        textcolor++
        if textcolor > 17
          textcolor:=1
        text.color(CLR[textcolor])
      if key == 991                                      ' SHIFT Pause or SHIFT Break then do a reset command
        key:=0
        text.str(string("KEYBOARD RESET"))
        DIRA[25]~~ ' output
        OUTA[25]~ 'PULL LOW
        PAUSEMS(20)
        OUTA[25]~~
        DIRA[25]~
      if key > 576 and key < 603                        ' ctrl A-Z
        key:= key - 576
      if key > 608 and key < 635                        'ctrl a-z
        key:= key - 608
      if key == 200                                     'Is it backspace? Make it code for underscore
        key:= 95
        text.out(key)
      if key >> 94
         key := 0
'================================================
'CODE TO PUT KEYSTROKE ON 65C21 KEYBOARD PORT
'================================================
      DIRA[23..16]~~                                    'Set pins to output ASCII code
      OUTA[23]~                                         'Make sure STROBE signal is off
'      if key == 203                                    'Is it upper code for ESC key?
'          key:= 27                                     'Yes, convert to standard ASCII value
'      if key >> << 96                                      'Is the keystroke Apple 1 compatible?
      if key > 0 and key < 96
          outa[22..16]:= key                            'Yes, send the 7 bit code
          outa[23]~~                                    'Set strobe high
          PauseMS(4)                                    'Pause long enough to register as a keystroke was 20 WAS 10
          outa[23]~                                     'Ok, turn off strobe bit
      DIRA[23..16]~                                     'Set pins back to ASCII keyboard ready
      OUTA[23]~

PUB serial                                              'Serial INPUT COG
  DIRA[23..16]~
  repeat
      serdata:= Serport.in                              'Get data from serial port, wait until received
      DIRA[23..16]~~
      OUTA[23]~
      if serdata < 96
          OUTA[22..16]:= serdata                        'send data to keyboard port on 6821
          OUTA[23]~~                                    'set strobe
          PauseMS(4)                                    'pause for strobe effect WAS 20
          OUTA[23]~
      DIRA[23..16]~
      OUTA[23]~

PRI PauseMs( mS )                                       ' Routine to delay in specified mS

  waitcnt( ( clkfreq / 1000 ) * mS + cnt )