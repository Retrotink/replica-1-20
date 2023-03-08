''
''
'' MODIFIED BY VINCE BRIEL FOR REPLICA 1 TWENTIETH FEATURES 2023
''
''

CON
  cols          = 80 '128                               ' number of screen columns
  rows          = 40 '64                                ' number of screen rows
  chars         = rows*cols                             ' number of screen characters
  esc           = $CB                                   ' keyboard esc char
  cols1         = 81                                    ' adjusted value for 80th character

OBJ
  vga : "vga_Hires_Text"

VAR
  byte  screen[chars]           ' screen character buffer
  word  colors[rows]            ' color specs for each screen row (see ColorPtr description above)
  byte  cursor[6]               ' cursor info array (see CursorPtr description above)
  long  sync, loc, xloc, yloc   ' sync used by VGA routine, others are local screen pointers
  long  Brate
  byte  inverse
  byte  invs
PUB start(BasePin) | i, char

''start vga
  vga.start(BasePin, @screen, @colors, @cursor, @sync)
  waitcnt(clkfreq * 1 + cnt)    'wait 1 second for cogs to start
  xloc := cursor[0] := 0
  yloc := cursor[1] := 0
  loc  := xloc + yloc*cols

PUB inv(c)
  inverse:=c
PUB cursorset(c) | i
  i:=%000
  if c == 1
    i:= %001
  if c == 2
    i:= %010
  if c == 3
    i:= %011
  if c == 4
    i:= %101
  if c == 5
    i:= %110
  if c == 6
    i:= %111
  if c == 7
    i:= %000
  cursor[2] := i
PUB bin(value, digits)

'' Print a binary number, specify number of digits

  repeat while digits > 32
    out("0")
    digits--

  value <<= 32 - digits

  repeat digits
    out((value <-= 1) & 1 + "0")

PUB insertat(amount) | i,j,len
    len := (cols - xloc) - amount
    i := @screen                                     'starting location
    i := i + loc
    j := i + amount                                   'new location which is plus 1?
    bytemove(j,i, len)                           ' move chars over one
    bytefill(i, $20,amount)

PUB delp(amount) | i,j,len

    len := (cols - xloc) - amount
    i := @screen                                     'starting location
    i := i + loc
    j := i + amount                                   'new location which is plus 1?
    bytemove(i,j, len)                           ' move chars over one
    'bytefill(j, $20,amount)

PUB cls(screencolor) | i,x,y        'cls(c,screencolor,pcport,ascii,CR) | i,x,y


  x :=xloc
  y := yloc
  invs := inverse
 ' clrbtm(TURQUOISE)
  longfill(@screen, $20202020, chars/4)
  xloc := 0
  yloc :=0
  loc  := xloc + yloc*cols
  repeat cols
     out(32)
  xloc := cursor[0] := x 'right & left       was 0
  yloc := cursor[1] := y 'from top           was 1
  loc  := xloc + yloc*cols

PUB clearlinefromcursor | x,xx,y, loop

   y := cursor[1] 'yloc
   x := cursor[0] 'xloc
  xx := cursor[0] 'xloc

  repeat until xx == cols
    out(32)
    xx++

  yloc := cursor[1] := y
  xloc := cursor[0] := x
  loc  := xloc + yloc*cols

PUB clearlinetocursor | x,y,loop

  x := xloc
  xloc := loop := 0
  loc  := xloc + yloc*cols
  repeat until loop == x
     out(32)
     loop++
  xloc := x
  loc  := xloc + yloc*cols

PUB clearline | x,y

  x := xloc
  xloc := 0
  loc  := xloc + yloc*cols
  repeat cols
     out(32)
  xloc := x
  loc  := xloc + yloc*cols

PUB clsfromcursordown | x,y,loop,i
  x:=xloc
  y:=yloc

  i := rows - y
  i--
  'xloc :=0
  loop := 0
  'loc  := xloc + yloc*cols
  repeat until xloc == cols
     out(32)
  xloc := 0
  yloc++
  loc  := xloc + yloc*cols
  loop := yloc
  repeat until loop == rows
     str(string("                                                                                "))
     loop++
  xloc := cursor[0] := x
  yloc := cursor[1] := y
  loc  := xloc + yloc*cols

PUB clstocursor | x,y,z,loop 'working correctly now
  y := yloc
  x := xloc
  xloc :=0
  loc  := xloc + yloc*cols
  repeat until xloc == x
     out(32)
  yloc := 0
  z:=0
  'repeat until z == y-1
  repeat until z == y
    yloc := z
    xloc :=0
    loc := xloc + yloc*cols
    str(string("                                                                                "))
    z:= z + 1
'  yloc--
'  repeat until yloc <= 0
'     xloc :=0
'     loc  := xloc + yloc*cols
'     repeat cols
'        out(65)
'     yloc--
'  xloc := 0
'  loc  := xloc + yloc*cols
'  repeat 80
'     out(32)
  xloc := cursor[0] := x
  yloc := cursor[1] := y
  loc  := xloc + yloc*cols

PUB home

  xloc := cursor[0] := 0 'right & left
  yloc := cursor[1] := 0 'from top                   'was 1
  loc  := xloc + yloc*cols

PUB color(ColorVal) | i
''reset screen colors
  repeat i from 0 to rows - 1
    colors[i] := $0000 + ColorVal

'PUB clrbtm(ColorVal) | i
'  repeat i from 36 to rows - 1                         'was 35
'   colors[i] := $0000 + ColorVal
PUB rowcolor(ColorVal, row)
'' reset row color to colorval
  if row > rows-1
    row := rows-1

   colors[row] := $0000 + ColorVal


PUB cursloc(x, y)

'' move cursor to x, y position

'my code fix for y axis 1 is actually 0
'  y--


  xloc := cursor[0] := x
  yloc := cursor[1] := y
  loc  := xloc + yloc*cols


PUB rcursor(x, y)

'' move cursor to x, y position  -1 so hits zero location

'my code fix for y axis 1 is actually 0
  y--
  x--


  xloc := cursor[0] := x
  yloc := cursor[1] := y
  loc  := xloc + yloc*cols
PUB cursrow(y)

'' move cursor to y position

'  xloc := cursor[0] := x
  yloc := cursor[1] := y
  loc  := xloc + yloc*cols
PUB dec(value) | i

'' Print a decimal number

  if value < 0
    -value
    out("-")

  i := 1_000_000_000

  repeat 10
    if value => i
      out(value/i + "0")
      value //= i
      result~~
    elseif result or i == 1
      out("0")
    i /= 10

PUB hex(value, digits)

'' Print a hexadecimal number, specify number of digits

  repeat while digits > 8
    out("0")
    digits--

  value <<= (8 - digits) << 2

  repeat digits
    out(lookupz((value <-= 4) & $f : "0".."9", "A".."F"))

PUB scrollD | i,len,y,dest,source

    y := yloc * cols
    i := @screen
    dest := i + y
    source := dest + cols
    len := (chars-y-cols)/4
    longmove(source, dest, len)
    longfill(dest,$20202020, cols/4)



PUB scrollM  | i,y,dest,source,len
   'longmove(dest,source,length)
    y := yloc * cols
    i := @screen
    dest := i + y
    'len := (chars1-y)/4
    len := (chars-y-cols)/4
    source := dest + cols
    longmove(dest,source,len)


PRI newline | i, j, len
  if ++yloc == rows                           ' if last line on screen, shift all up       was just rows now says rowsnow
    yloc--                                    ' reset yloc it at bottom of screen
    i := @screen
    i += cols
    len := (chars - cols)/4                             'was chars now rowsnow*cols    (rowsnow*cols)
    longmove(@screen, i, len)                 ' shift screen up one line

    i := @screen
    i += ((rows*cols) - cols)              ' set "i" for use below

  else                                        ' if not last line, shift lines down
    i := @screen
    i += (rows - 2)*cols                      ' init ptr to start of next-to-last line  was -2 now -1
    i += cols                                 ' point to start of last line moved

  longfill(i, $20202020, cols/4)              ' clear the last line moved


  j := i - cols + xloc                        ' point to original cursor location
  bytemove(i, j, cols - xloc)                 ' move chars from cursor pos down to start of next line

  bytefill(j, $20, cols - xloc)               ' clear original part of line that was moved

  xloc := cursor[0] := 0                      ' reset xloc, loc and cursor position
  cursor[1] := yloc
  loc  := yloc*cols
PRI linefeed | i, j, len
  if ++yloc == rows                            ' if last line on screen, shift all up       was just rows now says rowsnow
    yloc--                                    ' reset yloc it at bottom of screen
    i := @screen


    i += cols

    len := (chars - cols)/4           'was chars now rowsnow*cols    (rowsnow*cols)
    longmove(@screen, i, len)                 ' shift screen up one line

    i := @screen
    i += ((rows*cols) - cols)              ' set "i" for use below     WAS CHARS NOW ROWSNOW*COLS

  else                                        ' if not last line, shift lines down
    i := @screen
    i += (rows - 2)*cols                      ' init ptr to start of next-to-last line  was -2 now -1
    i += cols                                 ' point to start of last line moved
  longfill(i, $20202020, cols/4)              ' clear the last line moved
  'j := i - cols + xloc                        ' point to original cursor location
  j := i + xloc
  bytemove(i, j, cols - xloc)                 ' move chars from cursor pos down to start of next line

  bytefill(j, $20, cols - xloc)               ' clear original part of line that was moved

  cursor[1] := yloc
  cursor[0] := xloc
  loc := xloc + (yloc*cols)
PUB out(c) | i, j
'' Print a character
''
''  $09 = tab
''  $0A = Linefeed
''  $0D = return -> CR
''  $20..$7E = display character
''  $7F = skip
''  $C0   left arrow
''  $C1 = right arrow
''  $C2 = up arrow
''  $C3 = down arrow
''  $C4 = home key - go to beginning of line
''  $C5 = end key - go past last char on line
''  $C6 = page up key - skip this key
''  $C7 = page down key - skip this key
''  $C8 = backspace key
''  $C9 = delete key
''  $CA = insert key - skip this key
''  $CB = esc - skip this key
''  $CC = left arrow don't scroll up
  case c



    $09:                        ' tab command
      repeat
        out($C1)                ' recursive call to out( )
      while xloc & 7            ' tab to multiples of 8
      'while xloc & 3            ' tab to multiples of 4
    $0A:
      linefeed
    $0D:                        ' CR, return to start of line
      if xloc
         repeat
           out($C0)                ' recursive call to shift left until at leftmost edge
         while xloc
      linefeed

    $5F:                        ' backspace  _
      if loc                    ' skip if at upper left of screen
        if xloc                 ' do 'else' if at start of line
          xloc--                ' xloc left one space
          loc--

          i := @screen          ' calculate
          i += xloc + yloc*cols ' destination for shift left one
          bytemove(i, i+1, cols - xloc - 1)
          screen[cols - 1 + yloc*cols] := $20

        else                    ' here if xloc == 0
          if screen[loc-1] == $20   ' last char on prev line
            yloc--

            i := @screen          ' calculate
            i += loc - 1          ' destination for shift left one

            repeat while screen[--loc] == $20
              bytemove(i, i+1, cols) ' move one row's worth of chars
              i--                 ' dec "i" to correspond to --loc

              screen[loc + cols] := $20  ' clear old char

              if ++xloc == cols   ' use xloc as counter here, 0..., don't move > 1 row
                loc--             ' make as if loc had been bumped above B4 we quit
                quit

            loc++                 ' bump loc to space char
            xloc := loc - yloc*cols ' re-calculate xloc from loc and yloc

        cursor[0] := xloc         ' reset cursor loc
        cursor[1] := yloc




    $20..$7E:                   ' character

      if inverse==1             'check for inverse character mode
        c:=c + $80                'add for inverse
      if ++xloc == cols1
         xloc := xloc - 1
         newline
         xloc := cursor[0] := 1

      screen[loc++] := c        ' output the character
      cursor[0] := xloc
      cursor[1] := yloc
    $C0:                        ' left arrow
      if loc                    ' skip this if at upper left screen
        loc--
        if xloc
          xloc--
        else
          xloc := cols - 1
          yloc--
        cursor[0] := xloc
        cursor[1] := yloc



    $C1:                        ' right arrow
      if loc <> chars - 1       ' skip if at lower right of screen
        loc++
        if xloc <> cols - 1
          xloc++
        else
          xloc := 0
          yloc++
        cursor[0] := xloc
        cursor[1] := yloc

    $C2:                        ' up arrow
      if yloc                   ' skip if yloc at top of screen
        yloc--                  ' move yloc up one row
        loc -= cols             ' move loc var back one row
        cursor[1] := yloc       ' reset 'y' cursor position

    $C3:                        ' down arrow
      if yloc <> rows - 1    ' skip if at bottom of screen
        yloc++                  ' move yloc dowm one row
        loc += cols             ' move loc var down one row
        cursor[1] := yloc

    $C4:                        ' home key - move to 1st char of line
      xloc := cursor[0] := 0
      loc := xloc + yloc*cols

    $C5:                        ' end key - move to last char of line
      if xloc <> cols - 1
        repeat xloc from cols - 1 to 0
         loc := xloc + yloc*cols
         if screen[loc] <> $20  ' continue until first non-space char
           if xloc <> cols - 1
             xloc++               ' move past non-blank char
             loc++
           quit

        cursor[0] := xloc       ' loc is already reset from above

    $C8:                        ' backspace
      if loc                    ' skip if at upper left of screen
        if xloc                 ' do 'else' if at start of line
          xloc--                ' xloc left one space
          loc--

          i := @screen          ' calculate
          i += xloc + yloc*cols ' destination for shift left one
          bytemove(i, i+1, cols - xloc - 1)
          screen[cols - 1 + yloc*cols] := $20

        else                    ' here if xloc == 0
          if screen[loc-1] == $20   ' last char on prev line
            yloc--

            i := @screen          ' calculate
            i += loc - 1          ' destination for shift left one

            repeat while screen[--loc] == $20
              bytemove(i, i+1, cols) ' move one row's worth of chars
              i--                 ' dec "i" to correspond to --loc

              screen[loc + cols] := $20  ' clear old char

              if ++xloc == cols   ' use xloc as counter here, 0..., don't move > 1 row
                loc--             ' make as if loc had been bumped above B4 we quit
                quit

            loc++                 ' bump loc to space char
            xloc := loc - yloc*cols ' re-calculate xloc from loc and yloc

        cursor[0] := xloc         ' reset cursor loc
        cursor[1] := yloc

    $C9:                          ' delete
      if xloc == cols - 1
        screen[loc] := $20        ' if at last char on line, clear it and exit

      else
        repeat i from xloc to cols - 2
          j := i + yloc*cols
          screen[j] := screen[j+1]

       screen[j+1] := $20       ' clear last char on line after shift left
PUB fill | i, temp,ran,dont
  repeat i from 0 to 3953                               ' Do all of video RAM to emulate garbage screen  3973 is all characters
    dont := ||(ran?//127)
      if dont < 97  or cols == 0
        temp := ||(ran?//127)                         ' Generate random byte and store it

        if temp < 20
           temp := temp + 20
    out(temp)
PUB str(string_ptr)

'' Print a zero terminated string

  repeat strsize(string_ptr)
    out(byte[string_ptr++])