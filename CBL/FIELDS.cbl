      *A-1-B--+----2----+----3----+----4----+----5----+----6----+----7--
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIELDS.
       AUTHOR. Andras Pahi.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CURRENCY IS '$'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COMPOSITE  PIC 9(4) VALUE 1234.
       01 FILLER REDEFINES WS-COMPOSITE.
          05 WS-X       PIC 99.
          05 WS-Y       PIC 99.
       01 WS-DISPLAY    PIC S9(5)V9(2) VALUE -12345.67 USAGE IS DISPLAY.
       01 WS-BINARY     PIC S9(5)V9(2) VALUE -12345.67 USAGE IS COMP.
       01 WS-FLOAT                     VALUE -12345.67 USAGE IS COMP-1.
       01 WS-DOUBLE                    VALUE -12345.67 USAGE IS COMP-2.
       01 WS-PACKED     PIC S9(5)V9(2) VALUE -12345.67 USAGE IS COMP-3.
       01 WS-BINARY2    PIC S9(5)V9(2) VALUE -12345.67 USAGE IS COMP-4.
       01 WS-NATIVE     PIC S9(5)V9(2) VALUE -12345.67 USAGE IS COMP-5.
      *-----------------------------------------------------------------
       01 WS-ENTER      PIC X.
       01 CUSTOMER-NAME PIC X(30)      VALUE 'John Jones'.
       01 ALL-STARS     PIC X(80)      VALUE ALL '*'.
       01 WS-STR        PIC X(8)       VALUE ALL SPACES.
      *-----------------------------------------------------------------
       01 WS-Z          PIC Z(5).ZZ.
       01 WS-STAR       PIC *(5).**.
       01 WS-CONST      PIC S9(4)V99   VALUE -1234.56.
       01 WS-PLUS-CONST PIC S9(4)V99   VALUE +1234.56.
       01 WS-NEGATIVE           PIC -9(5).99.
       01 WS-NEGATIVE-SUPPRESS  PIC -(5).99.
       01 WS-NEGATIVE-TRAIL     PIC 9(5).99-.
       01 WS-POSITIVE-TRAIL     PIC 9(5).99+.
       01 WS-COMMA-TRAIL        PIC 99,999.99-.
       01 WS-SUPPRESS-COMMA-TRAIL   PIC ZZ,ZZZ.ZZ-.
       01 WS-DEBIT      PIC ZZ,ZZZ.ZZDB.
       01 WS-CURRENCY   PIC $$,$$$.99-.
      *-----------------------------------------------------------------
       01 WS-PHONE      PIC +99B99/999B9999.
       01 WS-DATE       PIC XX/XX/XXXX.
      *-----------------------------------------------------------------
       01 HELLO-WORLD.
           05 HELLO     PIC X(5) VALUE 'HELLO'.
           05 FILLER    PIC X    VALUE SPACE.
           05 WORLD     PIC X(5) VALUE 'WORLD'.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
           PERFORM NUMERICS
           PERFORM ALPHANUMERICS
           PERFORM FILLER-REDEFINE
           PERFORM GET-KEY
           STOP RUN
           .
       FILLER-REDEFINE.
           DISPLAY 'WS-COMPOSITE. ' WS-COMPOSITE
           MOVE 5678 TO WS-COMPOSITE
           DISPLAY 'WS-COMPOSITE. ' WS-COMPOSITE
           MOVE 11 TO WS-X
           DISPLAY 'WS-COMPOSITE. ' WS-COMPOSITE
           MOVE 22 TO WS-Y
           DISPLAY 'WS-COMPOSITE. ' WS-COMPOSITE
           .
       NUMERICS.
           DISPLAY 'WS-DISPLAY... ' FUNCTION HEX-OF(WS-DISPLAY)
           DISPLAY 'WS-BINARY.... ' FUNCTION HEX-OF(WS-BINARY)
           DISPLAY 'WS-FLOAT..... ' FUNCTION HEX-OF(WS-FLOAT)
           DISPLAY 'WS-DOUBLE.... ' FUNCTION HEX-OF(WS-DOUBLE)
           DISPLAY 'WS-PACKED.... ' FUNCTION HEX-OF(WS-PACKED)
           DISPLAY 'WS-BINARY2... ' FUNCTION HEX-OF(WS-BINARY2)
           DISPLAY 'WS-NATIVE.... ' FUNCTION HEX-OF(WS-NATIVE)
           .
       ALPHANUMERICS.
           DISPLAY 'CUSTOMER NAME ' CUSTOMER-NAME
           MOVE ALL ZEROS TO WS-STR
           DISPLAY 'STR.......... ' FUNCTION HEX-OF(WS-STR)
           MOVE ALL SPACES TO WS-STR
           DISPLAY 'STR.......... ' FUNCTION HEX-OF(WS-STR)
           MOVE LOW-VALUES TO WS-STR
           DISPLAY 'STR.......... ' FUNCTION HEX-OF(WS-STR)
           MOVE HIGH-VALUES TO WS-STR
           DISPLAY 'STR.......... ' FUNCTION HEX-OF(WS-STR)
           MOVE WS-CONST TO WS-Z
           DISPLAY 'Z............ ' WS-Z
           MOVE WS-CONST TO WS-STAR
           DISPLAY 'STAR......... ' WS-STAR
           MOVE WS-CONST TO WS-NEGATIVE
           DISPLAY 'NEGATIVE..... ' WS-NEGATIVE
           MOVE WS-CONST TO WS-NEGATIVE-SUPPRESS
           DISPLAY 'NEGATIVE-SUPRESS.... ' WS-NEGATIVE-SUPPRESS
           MOVE WS-CONST TO WS-NEGATIVE-TRAIL
           DISPLAY 'NEGATIVE-TRAIL...... ' WS-NEGATIVE-TRAIL
           MOVE WS-PLUS-CONST TO WS-POSITIVE-TRAIL
           DISPLAY 'POSITIVE-TRAIL...... ' WS-POSITIVE-TRAIL
           MOVE WS-CONST TO WS-COMMA-TRAIL
           DISPLAY 'COMMA-TRAIL......... ' WS-COMMA-TRAIL
           MOVE WS-CONST TO WS-SUPPRESS-COMMA-TRAIL
           DISPLAY 'COMMA-TRAIL......... ' WS-SUPPRESS-COMMA-TRAIL
           MOVE WS-CONST TO WS-DEBIT
           DISPLAY 'DEBIT............... ' WS-DEBIT
           MOVE WS-PLUS-CONST TO WS-CURRENCY
           DISPLAY 'CURRENCY............ ' WS-CURRENCY
           MOVE WS-CONST TO WS-CURRENCY
           DISPLAY 'CURRENCY............ ' WS-CURRENCY
           MOVE 36302011362 TO WS-PHONE
           DISPLAY 'PHONE............... ' WS-PHONE
           MOVE 11192025 TO WS-DATE
           DISPLAY 'DATE................ ' WS-DATE
           DISPLAY HELLO
           DISPLAY WORLD
           DISPLAY HELLO-WORLD
           .
       GET-KEY.
           DISPLAY ' '
           DISPLAY 'Press Enter'
           ACCEPT WS-ENTER.
           
