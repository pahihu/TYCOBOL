       IDENTIFICATION DIVISION.
       PROGRAM-ID. ZONEDHEX.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Unsigned / signed zoned decimals (DISPLAY format)
       01  WS-ZU-5          PIC 9(5)      VALUE 12345.
       01  WS-ZS-5P         PIC S9(5)     VALUE +12345.
       01  WS-ZS-5N         PIC S9(5)     VALUE -12345.
       01  WS-ZU-3V2        PIC 9(3)V99   VALUE 123.45.
       01  WS-ZS-3V2P       PIC S9(3)V99  VALUE +123.45.
       01  WS-ZS-3V2N       PIC S9(3)V99  VALUE -123.45.

      * HEX-OF() results (2 hex chars per byte; 5 bytes → 10 chars; oversize OK)
       01  HX-ZU-5          PIC X(20).
       01  HX-ZS-5P         PIC X(20).
       01  HX-ZS-5N         PIC X(20).
       01  HX-ZU-3V2        PIC X(20).
       01  HX-ZS-3V2P       PIC X(20).
       01  HX-ZS-3V2N       PIC X(20).

       01  SEP              PIC X(60) VALUE ALL '-'.

       PROCEDURE DIVISION.
           DISPLAY SEP
           DISPLAY 'ZONED DECIMAL VALUES AND HEX (EBCDIC BYTES)'
           DISPLAY SEP

           *> Compute hex strings once
           MOVE FUNCTION HEX-OF(WS-ZU-5)     TO HX-ZU-5
           MOVE FUNCTION HEX-OF(WS-ZS-5P)    TO HX-ZS-5P
           MOVE FUNCTION HEX-OF(WS-ZS-5N)    TO HX-ZS-5N
           MOVE FUNCTION HEX-OF(WS-ZU-3V2)   TO HX-ZU-3V2
           MOVE FUNCTION HEX-OF(WS-ZS-3V2P)  TO HX-ZS-3V2P
           MOVE FUNCTION HEX-OF(WS-ZS-3V2N)  TO HX-ZS-3V2N

           *> Show each line via paragraphs
           PERFORM SHOW-ZU-5
           PERFORM SHOW-ZS-5P
           PERFORM SHOW-ZS-5N
           PERFORM SHOW-ZU-3V2
           PERFORM SHOW-ZS-3V2P
           PERFORM SHOW-ZS-3V2N

           DISPLAY SEP
           DISPLAY 'Notes:'
           DISPLAY ' - Last byte carries the sign (zone): F = positive/
      -    'unsigned, D = negative.'
           DISPLAY ' - Example: -12345 (S9(5)) ends with D5; +12345 end
      -    's with F5.'
           DISPLAY SEP

           GOBACK.

       SHOW-ZU-5.
           DISPLAY 'WS-ZU-5    PIC 9(5)      (unsigned) = ' WS-ZU-5
                   '    HEX: ' HX-ZU-5
           EXIT.

       SHOW-ZS-5P.
           DISPLAY 'WS-ZS-5P   PIC S9(5)     (positive) = ' WS-ZS-5P
                   '   HEX: ' HX-ZS-5P
           EXIT.

        SHOW-ZS-5N.
           DISPLAY 'WS-ZS-5N   PIC S9(5)     (negative) = ' WS-ZS-5N
                   '   HEX: ' HX-ZS-5N
           EXIT.

        SHOW-ZU-3V2.
           DISPLAY 'WS-ZU-3V2  PIC 9(3)V99   (unsigned) = ' WS-ZU-3V2
                   '   HEX: ' HX-ZU-3V2
           EXIT.

        SHOW-ZS-3V2P.
           DISPLAY 'WS-ZS-3V2P PIC S9(3)V99  (positive) = ' WS-ZS-3V2P
                   '  HEX: ' HX-ZS-3V2P
           EXIT.

        SHOW-ZS-3V2N.
           DISPLAY 'WS-ZS-3V2N PIC S9(3)V99  (negative) = ' WS-ZS-3V2N
                   '  HEX: ' HX-ZS-3V2N
           EXIT.

