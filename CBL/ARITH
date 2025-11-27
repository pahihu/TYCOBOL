000000*AA1ABBB+----2----+----3----+----4----+----5----+----6----+----7--SEQUENCE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARITH.
       AUTHOR. Andras Pahi.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-INT1 PIC 99 VALUE 10.
       01 WS-INT2 PIC 99 VALUE 5.
       01 WS-INT3 PIC 99 VALUE 20.
       01 WS-SINT3 PIC S99 VALUE 20.
       01 WS-ITEM2 PIC 99V99  VALUE 10.00.
       01 WS-ITEM3 PIC 99V999 VALUE 10.126.
       01 WS-STR2  PIC X(2) VALUE SPACES.
       01 WS-STR4  PIC X(4) VALUE SPACES.
       01 WS-STR8  PIC X(8) VALUE SPACES.
       01 WS-STR12 PIC X(12) VALUE SPACES.
       01 WS-EDIT8 PIC XX/XX/XX VALUE SPACES.
       01 WS-STR-GROUP1.
          03 WS-STR-ITEM1 PIC X(7) VALUE 'ABCDEFG'.
       01 WS-DISP2 PIC ZZ.99.
       01 WS-DISP3 PIC ZZ.999.
       01 WS-ZERO  PIC 9 VALUE ZERO.
       01 WS-TMP1  PIC 99 VALUE ZERO.
       01 WS-TMP2  PIC 99 VALUE ZERO.
       01 WS-NUM92 PIC 9(9)V99 VALUE ZEROS.
       01 WS-EDIT62 PIC 999,999.99.
       01 WS-EDIT22 PIC 99.99.
       01 WS-NUM22 PIC 99V99 VALUE ZEROS.
       01 WS-NUM32 PIC 999V99 VALUE ZEROS.
       01 WS-NUM11 PIC 9V9 VALUE ZEROS.
       01 WS-GROUP1.
          03 FG-FIRST   PIC 9(2) VALUE 1.
          03 FG-SECOND  PIC 9(2) VALUE 2.
          03 FG-THIRD   PIC 9(2) VALUE 3.
       01 WS-GROUP2.
          03 FG-FIRST   PIC 9(2) VALUE 10.
          03 FG-SECOND  PIC 9(2) VALUE 20.
          03 FG-THIRD   PIC 9(2) VALUE 30.
      *-----------------------------------------------------------------
      * !!!  GNUCOBOL DOES NOT DETECT ZERO DIVIDE !!!
       01 FORCED-EXCEPTION-X  PIC X VALUE SPACE.
       01 FORCED-EXCEPTION REDEFINES FORCED-EXCEPTION-X PIC S9 COMP-3.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
           PERFORM SHOW-ITEMS
           DISPLAY ' '
           DISPLAY 'ADD'
           ADD 5 WS-INT1 TO WS-INT2 WS-INT3
           PERFORM SHOW-ITEMS
      *-----------------------------------------------------------------
           DISPLAY ' '
           DISPLAY 'ADD DECIMAL'
           ADD 10.126 TO WS-ITEM2
           PERFORM SHOW-ITEMS
      *-----------------------------------------------------------------
           DISPLAY ' '
           DISPLAY 'ADD DECIMAL ROUNDED'
           MOVE 10.00 TO WS-ITEM2
           ADD 10.126 TO WS-ITEM2 ROUNDED
           PERFORM SHOW-ITEMS
      *-----------------------------------------------------------------
           DISPLAY ' '
           DISPLAY 'ADD OVERFLOW'
           PERFORM SHOW-ITEMS
           ADD 70 TO WS-INT3
           PERFORM SHOW-ITEMS
      *-----------------------------------------------------------------
           DISPLAY ' '
           DISPLAY 'ADD OVERFLOW ON SIZE ERROR'
           PERFORM SHOW-ITEMS
           MOVE 35 TO WS-INT3
           ADD 70 TO WS-INT3
               ON SIZE ERROR
                  DISPLAY 'Field WS-INT3 overflowed on ADD'
               NOT ON SIZE ERROR
                  DISPLAY 'Field WS-INT3 did not overflow'
           END-ADD
           PERFORM SHOW-ITEMS
      *-----------------------------------------------------------------
           DISPLAY ' '
           DISPLAY 'ADD GIVING'
           MOVE  5 TO WS-INT2
           MOVE 10 TO WS-INT3
           PERFORM SHOW-ITEMS
           ADD 1, 2, 3 TO WS-INT1 GIVING WS-INT2, WS-INT3
           PERFORM SHOW-ITEMS
      *-----------------------------------------------------------------
           DISPLAY ' '
           DISPLAY 'ADD CORRESPONDING'
           PERFORM SHOW-GROUP
           ADD CORRESPONDING WS-GROUP1 TO WS-GROUP2
           PERFORM SHOW-GROUP
      *-----------------------------------------------------------------
           DISPLAY ' '
           DISPLAY 'SUBTRACT'
           MOVE 20 TO WS-INT1 WS-INT3
           PERFORM SHOW-ITEMS
           SUBTRACT 1, WS-INT1 FROM WS-INT3 GIVING WS-SINT3
           PERFORM SHOW-ITEMS
      *-----------------------------------------------------------------
           DISPLAY ' '
           DISPLAY 'MULTIPLY'
           PERFORM SHOW-ITEMS
           MULTIPLY 2.5 BY WS-INT2 GIVING WS-ITEM2 ROUNDED
               ON SIZE ERROR DISPLAY 'Multiplication error'
               NOT ON SIZE ERROR DISPLAY 'No multiplication error'
           END-MULTIPLY
           PERFORM SHOW-ITEMS
      *-----------------------------------------------------------------
           DISPLAY ' '
           DISPLAY 'DIVIDE'
           PERFORM SETUP-ITEMS
           PERFORM SHOW-ITEMS
           DIVIDE WS-INT2 INTO WS-INT3
           PERFORM SHOW-ITEMS
      * Form 2... Y INTO X
           PERFORM SETUP-ITEMS
           DIVIDE WS-INT2 INTO WS-INT3 GIVING WS-INT1
           PERFORM SHOW-ITEMS
      * Form 3...  X BY Y
           PERFORM SETUP-ITEMS
           DIVIDE WS-INT3 BY WS-INT2 GIVING WS-INT1
           PERFORM SHOW-ITEMS
      * Form 4...  Y INTO X REMAINDER Z
           PERFORM SETUP-ITEMS
           DIVIDE 3 INTO WS-INT3 GIVING WS-INT1 REMAINDER WS-INT2
           PERFORM SHOW-ITEMS
      * Form 5...  X BY Y REMAINDER Z
           PERFORM SETUP-ITEMS
           DIVIDE WS-INT3 BY 3 GIVING WS-INT1 REMAINDER WS-INT2
           PERFORM SHOW-ITEMS
      * divide by zero
           DISPLAY 'DIVIDE BY ZERO'
           PERFORM SETUP-ITEMS
           DIVIDE 0 INTO WS-INT2 GIVING WS-INT3
      D        ON SIZE ERROR ADD +1 TO FORCED-EXCEPTION
           END-DIVIDE
           PERFORM SHOW-ITEMS
      *-----------------------------------------------------------------
           DISPLAY ' '
           DISPLAY 'COMPUTE'
           PERFORM SETUP-ITEMS
           MOVE 5 TO WS-TMP1
           MOVE 7 TO WS-TMP2
           COMPUTE WS-ITEM2 = (WS-INT2 * WS-TMP1) + (WS-INT1 * WS-TMP2)
                ON SIZE ERROR DISPLAY 'Size error on COMPUTE'
           END-COMPUTE
           PERFORM SHOW-ITEMS
           DISPLAY 'COMPUTE ROUNDED'
           PERFORM SETUP-ITEMS
           COMPUTE WS-ITEM2 ROUNDED = (WS-INT2 * 5) + (WS-INT1 * 7)
                ON SIZE ERROR DISPLAY 'Size error on COMPUTE ROUNDED'
           END-COMPUTE
           PERFORM SHOW-ITEMS
      *-----------------------------------------------------------------
           DISPLAY ' '
           DISPLAY 'MOVE ALNUM'
           MOVE 'ABCD' TO WS-STR2, WS-STR4, WS-STR8
           PERFORM SHOW-STR
           DISPLAY ' '
           DISPLAY 'MOVE NUMERIC'
           MOVE 123.45 TO WS-NUM22
           MOVE 123.456 TO WS-NUM32, WS-NUM11
           PERFORM SHOW-STR
           DISPLAY ' '
           DISPLAY 'MOVE EDITED'
           MOVE 123999.99 TO WS-EDIT62
           MOVE 12.99 TO WS-EDIT22
           MOVE WS-EDIT62 TO WS-NUM92, WS-STR12
           PERFORM SHOW-STR
           MOVE WS-EDIT22 TO WS-NUM92, WS-STR12
           PERFORM SHOW-STR
           DISPLAY ' '
           DISPLAY 'MOVE NUMERIC TO ALNUM'
      * !!! SHOULD BE ERROR !!!
           MOVE -12345.67 TO WS-STR12
           DISPLAY ' '
           DISPLAY 'MOVE GROUP TO EDITED'
           MOVE WS-STR-GROUP1 TO WS-EDIT8
           PERFORM SHOW-STR
           MOVE WS-STR-ITEM1 TO WS-EDIT8
           PERFORM SHOW-STR
      *-----------------------------------------------------------------
           STOP RUN
           .

      *=================================================================
       SHOW-STR.
           DISPLAY '--- STR ITEMS ---'
           DISPLAY 'WS-STR2:=/' WS-STR2 '/'
           DISPLAY 'WS-STR4:=/' WS-STR4 '/'
           DISPLAY 'WS-STR8:=/' WS-STR8 '/'
           DISPLAY 'WS-STR12:=/' WS-STR12 '/'
           DISPLAY 'WS-EDIT8:=/' WS-EDIT8 '/'
           DISPLAY 'WS-NUM22:=' WS-NUM22
           DISPLAY 'WS-NUM32:=' WS-NUM32
           DISPLAY 'WS-NUM11:=' WS-NUM11
           DISPLAY 'WS-NUM92:=' WS-NUM92
           .
      *-----------------------------------------------------------------
       SETUP-ITEMS.
           MOVE 10 TO WS-INT1
           MOVE  5 TO WS-INT2
           MOVE 20 TO WS-INT3
           MOVE 20 TO WS-SINT3
           MOVE 10.00 TO WS-ITEM2
           MOVE 10.126 TO WS-ITEM3
           .
      *-----------------------------------------------------------------
       SHOW-ITEMS.
           DISPLAY '--- ITEMS ---'
           DISPLAY 'WS-INT1.... ' WS-INT1
           DISPLAY 'WS-INT2.... ' WS-INT2
           DISPLAY 'WS-INT3.... ' WS-INT3
           DISPLAY 'WS-SINT3... ' WS-SINT3
           DISPLAY 'WS-ITEM2... ' WS-ITEM2
           MOVE WS-ITEM2 TO WS-DISP2
           DISPLAY 'WS-DISP2... ' WS-DISP2
           DISPLAY 'WS-ITEM3... ' WS-ITEM3
           MOVE WS-ITEM3 TO WS-DISP3
           DISPLAY 'WS-DISP3... ' WS-DISP3
           .
      *-----------------------------------------------------------------
       SHOW-GROUP.
           DISPLAY '--- GROUP ITEMS ---'
           DISPLAY 'FG-FIRST.... ' FG-FIRST  OF WS-GROUP2
           DISPLAY 'FG-SECOND... ' FG-SECOND IN WS-GROUP2
           DISPLAY 'FG-THIRD.... ' FG-THIRD  OF WS-GROUP2
           .

