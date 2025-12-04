000000*AA1ABBB+----2----+----3----+----4----+----5----+----6----+----7--SEQUENCE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RULE30.
       AUTHOR. Andras Pahi.
       DATE-WRITTEN. 2025-12-04.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 WORLD-LENGTH   PIC 9(3) VALUE 120.
       01 WORLD.
          03 CELL        OCCURS 122 TIMES
                         PIC X VALUE SPACE.
             88 ALIVE             VALUE '*'.
       01 FILLER REDEFINES WORLD.
          03 FILLER        PIC X.
          03 VISIBLE-WORLD PIC X(120).
          03 FILLER        PIC X.
       01 NEXT-WORLD.
          03 NEXT-CELL   OCCURS 122 TIMES
                         PIC X VALUE SPACE.
             88 NEXT-ALIVE        VALUE '*'.
       01 POS            PIC 9(3) VALUE ZERO.
       01 HP             PIC 9(2) VALUE ZERO.
          88 HEALTHY              VALUE 1 THRU 4.
       01 GENERATION     PIC 9(2) VALUE ZERO.
       01 MAX-GENERATION PIC 9(3) VALUE 60.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
           SET ALIVE(61) TO TRUE
           DISPLAY VISIBLE-WORLD
           PERFORM VARYING GENERATION FROM 1 BY 1
                   UNTIL GENERATION > MAX-GENERATION
               PERFORM ALIVE-OR-DEAD
               DISPLAY VISIBLE-WORLD
           END-PERFORM
           GOBACK
           .
      *-----------------------------------------------------------------
       ALIVE-OR-DEAD.
           INITIALIZE NEXT-WORLD
           PERFORM VARYING POS FROM 2 BY 1
                   UNTIL POS > LENGTH OF VISIBLE-WORLD + 1
               INITIALIZE HP
               IF ALIVE(POS - 1)
                   ADD 4 TO HP
               END-IF
               IF ALIVE(POS)
                   ADD 2 TO HP
               END-IF
               IF ALIVE(POS + 1)
                   ADD 1 TO HP
               END-IF
               IF HEALTHY
                   SET NEXT-ALIVE(POS) TO TRUE
               END-IF
           END-PERFORM
           MOVE NEXT-WORLD TO WORLD
           .
