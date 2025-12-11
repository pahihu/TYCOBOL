000000*AA1ABBB+----2----+----3----+----4----+----5----+----6----+----7--SEQUENCE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PERF.
       AUTHOR. Andras Pahi.
       DATE-WRITTEN. 2025-12-11.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-COUNTER    PIC 9(2) VALUE ZEROS.
       01 WS-TEST       PIC X    VALUE SPACE.
       PROCEDURE DIVISION.
       PERF-SECTION SECTION.
       START-OF-PROGRAM.
           PERFORM FIRST-SECTION
           DISPLAY 'RETURN FROM FIRST-SECTION'
           PERFORM PARA-2
           DISPLAY 'RETURN FROM PARA 2'
           DISPLAY 'PARA 1 THRU PARA 2'
           PERFORM PARA-1 THRU PARA-2
           DISPLAY 'RETURN FROM THRU'
           PERFORM PARA-1 THRU PARA-1-EXIT
           INITIALIZE WS-COUNTER
           MOVE ZEROS TO WS-COUNTER
           PERFORM COUNT-BY-1 10 TIMES
           MOVE 'A' TO WS-TEST
           PERFORM NEVER-EXECUTED UNTIL WS-TEST = 'A'
           MOVE 17 TO WS-COUNTER
           PERFORM NEVER-DO-IT
           .
       END-OF-PROGRAM.
           GOBACK
           .
       COUNT-BY-1.
           ADD 1 TO WS-COUNTER
           DISPLAY WS-COUNTER
           .
       NEVER-DO-IT.
           IF WS-COUNTER < 20
               PERFORM COUNT-BY-1
               GO TO NEVER-DO-IT
           END-IF
           .
       NEVER-EXECUTED.
           DISPLAY 'SHOULD NOT SEE THIS'
           .
       FIRST-SECTION SECTION.
           DISPLAY 'FIRST-SECTION'
           .
       PARA-1.
           DISPLAY 'PARA 1'
           DISPLAY 'END OF PARA 1'
           .
       PARA-1-EXIT.
      * does nothing, visual PARA-1 termination
           EXIT
           .
       PARA-2.
           DISPLAY 'PARA 2'
           DISPLAY 'END OF PARA 2'
           .
       PARA-3.
           DISPLAY 'PARA 3'
           DISPLAY 'END OF PARA 3'
           .
