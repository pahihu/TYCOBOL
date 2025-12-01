000000*AA1ABBB+----2----+----3----+----4----+----5----+----6----+----7--SEQUENCE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRMAN.
       AUTHOR. Andras Pahi.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 FIRST-NAME    PIC X(25) VALUE SPACES.
       01 LAST-NAME     PIC X(25) VALUE SPACES.
       01 NAME          PIC X(30) VALUE SPACES.
       01 LAST-LENGTH   PIC 9(02) VALUE ZEROS.
       01 NUM-SPACES    PIC 9(02) VALUE ZEROS.
       01 WS-STR        PIC X(25) VALUE SPACES.
       01 SWITCHES.
          03 LAST-EMPTY PIC X VALUE SPACE.
          88 DONE             VALUE 'Y'.
       PROCEDURE DIVISION.
           PERFORM READ-LAST
           PERFORM UNTIL NOT DONE
               DISPLAY 'First name: ' WITH NO ADVANCING
               ACCEPT FIRST-NAME
      *-----------------------------------------------------------------
               INITIALIZE NAME
               STRING FIRST-NAME DELIMITED BY SPACE
                      ' '        DELIMITED BY SIZE
                      LAST-NAME  DELIMITED BY SPACE
                   INTO
                      NAME
                   ON OVERFLOW
                      MOVE LAST-NAME TO NAME
               END-STRING
               DISPLAY 'NAME...: ' NAME
               PERFORM READ-LAST
           END-PERFORM
           DISPLAY 'Done'
           GOBACK
           .
      *-----------------------------------------------------------------
       READ-LAST.
           DISPLAY 'Enter last name: ' WITH NO ADVANCING
           INITIALIZE WS-STR
           ACCEPT WS-STR
           INITIALIZE NUM-SPACES
           INSPECT WS-STR
               TALLYING NUM-SPACES
               FOR LEADING SPACES
           IF NUM-SPACES = LENGTH OF WS-STR THEN
               SET DONE TO TRUE
           ELSE
               INITIALIZE LAST-LENGTH
               ADD 1 TO NUM-SPACES
               INSPECT WS-STR(NUM-SPACES:)
                   TALLYING LAST-LENGTH
                   FOR ALL CHARACTERS
                   BEFORE INITIAL ' '
               MOVE WS-STR(NUM-SPACES:LAST-LENGTH) TO LAST-NAME
           END-IF
           .
