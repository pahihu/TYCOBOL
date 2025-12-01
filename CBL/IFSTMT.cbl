000000*AA1ABBB+----2----+----3----+----4----+----5----+----6----+----7--SEQUENCE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IFSTMT.
       AUTHOR. Andras Pahi.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CLASS ABC IS 'A' THRU 'C' SPACE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-TEXT   PIC X(20) VALUE SPACES.
       01 WS-DATA   PIC 9(02) VALUE 3.
       PROCEDURE DIVISION.
           IF WS-TEXT IS EQUAL SPACES THEN
               DISPLAY 'ALL SPACES'
           END-IF
           MOVE 'ABC' TO WS-TEXT
           IF WS-TEXT ABC THEN
               DISPLAY WS-TEXT ' MATCHES ABC'
           END-IF
           MOVE 'ABCD' TO WS-TEXT
           IF WS-TEXT IS NOT ABC THEN
               DISPLAY WS-TEXT ' DOES NOT MATCHES ABC'
           END-IF
           IF WS-TEXT IS NUMERIC THEN
               DISPLAY WS-TEXT ' IS NUMERIC'
           END-IF
           IF WS-TEXT IS ALPHABETIC THEN
               DISPLAY WS-TEXT ' IS ALPHABETIC'
           END-IF
           IF WS-TEXT IS ALPHABETIC-LOWER THEN
               DISPLAY WS-TEXT ' IS ALPHABETIC-LOWER'
           END-IF
           IF WS-TEXT IS ALPHABETIC-UPPER THEN
               DISPLAY WS-TEXT ' IS ALPHABETIC-UPPER'
           END-IF
           INSPECT WS-TEXT
               CONVERTING 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
               TO         'abcdefghijklmnopqrstuvwxyz'
           IF WS-TEXT IS ALPHABETIC-LOWER THEN
               DISPLAY WS-TEXT ' IS ALPHABETIC-LOWER'
           END-IF
           IF 1 IS EQUAL TO 2 THEN
               CONTINUE
           ELSE
               DISPLAY '1 DOES NOT EQUAL 2'
           END-IF
      *-----------------------------------------------------------------
           IF WS-DATA = 2 OR WS-DATA = 3 OR WS-DATA = 4 THEN
               DISPLAY 'ONE OF 2 OR 3 OR 4'
           END-IF
           IF WS-DATA = 2 OR 3 OR 4 THEN
               DISPLAY 'ONE OF 2 OR 3 OR 4'
           END-IF
           GOBACK
           .
