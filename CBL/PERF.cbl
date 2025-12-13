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
      *01 BIG-COUNTER   PIC 9(9) COMP-5 VALUE ZERO.  0.00s
      *01 BIG-COUNTER   PIC 9(9) COMP-3 VALUE ZERO.  0.29s
      *01 BIG-COUNTER   PIC 9(9) COMP   VALUE ZERO.  1.23s
       01 PRT-BIG-COUNTER.
          03 FILLER     PIC X(13) VALUE 'BIG-COUNTER:='.
          03 DISP-BIG-COUNTER PIC 9(9) VALUE ZEROS.
       01 I             PIC 9    VALUE ZERO.
       01 DISP-ELAPSED  PIC Z(8)9.99 VALUE ZEROS.
       01 TIMERS.
          03 FILLER        OCCURS 2 TIMES.
             05 DT-AS-SECONDS     PIC 9(9)V99 VALUE ZEROS.
             05 DT-DOY            PIC 9(5) VALUE ZEROS.
             05 DT-STAMP.
                07 DT-YEAR        PIC 9(4).
                07 DT-MONTH       PIC 9(2).
                07 DT-DAY         PIC 9(2).
                07 DT-HOURS       PIC 9(2).
                07 DT-MINUTES     PIC 9(2).
                07 DT-SECONDS     PIC 9(2)V99.
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

           DISPLAY 'PERFORM VARYING'
           PERFORM COUNT-ROUTINE VARYING WS-COUNTER
                   FROM 1 BY 1 UNTIL WS-COUNTER > 10
           DISPLAY 'WS-COUNTER:=' WS-COUNTER

           PERFORM COUNT-ROUTINE WITH TEST AFTER VARYING WS-COUNTER
                   FROM 10 BY -1 UNTIL WS-COUNTER = 1
           DISPLAY 'WS-COUNTER:=' WS-COUNTER

           PERFORM COUNT-ROUTINE WITH TEST AFTER VARYING WS-COUNTER
                   FROM 1 BY 1 UNTIL WS-COUNTER = 10
           DISPLAY 'WS-COUNTER:=' WS-COUNTER
           PERFORM MENTINK
           .
       END-OF-PROGRAM.
           GOBACK
           .
       MENTINK.
           DISPLAY 'MENTINK START...'
           INITIALIZE BIG-COUNTER
           PERFORM COUNTER
           PERFORM 20000 TIMES
               PERFORM 2000 TIMES
                   ADD 1 TO BIG-COUNTER
               END-PERFORM
           END-PERFORM
           PERFORM TIMER
           MOVE BIG-COUNTER TO DISP-BIG-COUNTER
           DISPLAY PRT-BIG-COUNTER
           .
       COUNTER.
           MOVE FUNCTION CURRENT-DATE TO DT-STAMP(1)
           ACCEPT DT-DOY(1) FROM DAY
           .
       TIMER.
           MOVE FUNCTION CURRENT-DATE TO DT-STAMP(2)
           ACCEPT DT-DOY(2) FROM DAY
           MOVE 1 TO I
           PERFORM TIME-TO-SECONDS
           MOVE 2 TO I
           PERFORM TIME-TO-SECONDS
           SUBTRACT DT-AS-SECONDS(1) FROM DT-AS-SECONDS(2)
           MOVE DT-AS-SECONDS(2) TO DISP-ELAPSED
           DISPLAY 'ELAPSED TIME ' DISP-ELAPSED 'SECONDS'
           .
       TIME-TO-SECONDS.
           COMPUTE DT-AS-SECONDS(I) =
                    ((DT-DOY(I) * 365 + DT-HOURS(I)) * 24
                    + DT-MINUTES(I)) * 60
                    + DT-SECONDS(I)
           .
       COUNT-ROUTINE.
           DISPLAY WS-COUNTER
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
