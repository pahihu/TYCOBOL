      *A-1-B--+----2----+----3----+----4----+----5----+----6----+----7--SEQUENCE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NUMSTOR.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *  Same numeric value across formats (1,234,567.89)
       01  WS-ZONED         PIC S9(7)V99           VALUE 1234567.89.
       01  WS-PACKED        PIC S9(7)V99  COMP-3   VALUE 1234567.89.
       01  WS-BINARY        PIC S9(7)V99  COMP     VALUE 1234567.89.
       01  WS-NATIVE        PIC S9(7)V99  COMP-5   VALUE 1234567.89.
       01  WS-FLOAT4        USAGE COMP-1           VALUE 1.2345679E+06.
       01  WS-FLOAT8        USAGE COMP-2           VALUE 1.23456789E+06.

      *  HEX buffers (oversized is fine) and length counters
       01  HX-ZONED         PIC X(40).
       01  HX-PACKED        PIC X(40).
       01  HX-BINARY        PIC X(40).
       01  HX-NATIVE        PIC X(40).
       01  HX-FLOAT4        PIC X(40).
       01  HX-FLOAT8        PIC X(40).

       01  L-ZONED          PIC 9(4) COMP.
       01  L-PACKED         PIC 9(4) COMP.
       01  L-BINARY         PIC 9(4) COMP.
       01  L-NATIVE         PIC 9(4) COMP.
       01  L-FLOAT4         PIC 9(4) COMP.
       01  L-FLOAT8         PIC 9(4) COMP.

       01  SEP              PIC X(70) VALUE ALL '-'.

       PROCEDURE DIVISION.
           DISPLAY SEP
           DISPLAY 'NUMERIC STORAGE DEMO (Enterprise COBOL)'
           DISPLAY 'Value used: 1,234,567.89'
           DISPLAY SEP

      *      Compute hex strings and byte lengths
           MOVE FUNCTION HEX-OF(WS-ZONED)  TO HX-ZONED
           MOVE FUNCTION HEX-OF(WS-PACKED) TO HX-PACKED
           MOVE FUNCTION HEX-OF(WS-BINARY) TO HX-BINARY
           MOVE FUNCTION HEX-OF(WS-NATIVE) TO HX-NATIVE
           MOVE FUNCTION HEX-OF(WS-FLOAT4) TO HX-FLOAT4
           MOVE FUNCTION HEX-OF(WS-FLOAT8) TO HX-FLOAT8

           MOVE LENGTH OF WS-ZONED   TO L-ZONED
           MOVE LENGTH OF WS-PACKED  TO L-PACKED
           MOVE LENGTH OF WS-BINARY  TO L-BINARY
           MOVE LENGTH OF WS-NATIVE  TO L-NATIVE
           MOVE LENGTH OF WS-FLOAT4  TO L-FLOAT4
           MOVE LENGTH OF WS-FLOAT8  TO L-FLOAT8

      *      Show each format: DISPLAY value, length, and raw bytes (EBCDIC/host)
           PERFORM SHOW-ZONED
           PERFORM SHOW-PACKED
           PERFORM SHOW-BINARY
           PERFORM SHOW-NATIVE
           PERFORM SHOW-FLOAT4
           PERFORM SHOW-FLOAT8

           DISPLAY SEP
           DISPLAY 'Notes:'
           DISPLAY ' - DISPLAY (zoned): 1 byte per digit; sign in last 
      -    'byte''s zone (F/D).'
           DISPLAY ' - COMP-3 (packed): ~2 digits/byte; sign nibble at 
      -    'end.'
           DISPLAY ' - COMP (binary): size by total digits: 1-4->2B, 5-9
      -    '->4B, 10-18->8B.'
           DISPLAY ' - COMP-5: native binary (no PICTURE limit/rounding 
      -    'by compiler).'
           DISPLAY ' - COMP-1/2: IEEE/hex floats; value is approximate.'
           DISPLAY ' - On z/Architecture, binary is big-endian.'
           DISPLAY SEP

           GOBACK.

       SHOW-ZONED.
           DISPLAY 'ZONED   (PIC S9(7)V99       ) = ' WS-ZONED
                   '  | LEN: ' L-ZONED ' | HEX: ' HX-ZONED
           EXIT.

       SHOW-PACKED.
           DISPLAY 'PACKED  (PIC S9(7)V99 COMP-3) = ' WS-PACKED
                   '  | LEN: ' L-PACKED ' | HEX: ' HX-PACKED
           EXIT.

       SHOW-BINARY.
           DISPLAY 'BINARY  (PIC S9(7)V99 COMP  ) = ' WS-BINARY
                   ' | LEN: ' L-BINARY ' | HEX: ' HX-BINARY
           EXIT.

       SHOW-NATIVE.
           DISPLAY 'COMP-5  (PIC S9(7)V99 COMP-5) = ' WS-NATIVE
                   ' | LEN: ' L-NATIVE ' | HEX: ' HX-NATIVE
           EXIT.

       SHOW-FLOAT4.
           DISPLAY 'FLOAT4  (USAGE COMP-1       ) = ' WS-FLOAT4
                   '   | LEN: ' L-FLOAT4 ' | HEX: ' HX-FLOAT4
           EXIT.

       SHOW-FLOAT8.
           DISPLAY 'FLOAT8  (USAGE COMP-2       ) = ' WS-FLOAT8
                   '  | LEN: ' L-FLOAT8 ' | HEX: ' HX-FLOAT8
           EXIT.

