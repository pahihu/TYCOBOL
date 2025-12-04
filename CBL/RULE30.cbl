      ******************************************************************
      *                                                                *
      *             Wolfram's Rule 30 cellular automata                *
      *                                                                *
      ******************************************************************
000000*AA1ABBB+----2----+----3----+----4----+----5----+----6----+----7--SEQUENCE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RULE30.
       AUTHOR. Andras Pahi.
       DATE-WRITTEN. 2025-12-04.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WORLD.
          03 CELL        OCCURS 120 TIMES
                         PIC X VALUE SPACE.
             88 ALIVE             VALUE '*'.
       01 NEXT-WORLD.
          03 NEXT-CELL   OCCURS 120 TIMES
                         PIC X VALUE SPACE.
             88 NEXT-ALIVE        VALUE '*'.
       01 POS            PIC 9(3) BINARY VALUE ZERO.
       01 HP             PIC 9(1) BINARY VALUE ZERO.
          88 HEALTHY              VALUE 1 THRU 4.
       01 GENERATION     PIC 9(2) BINARY VALUE ZERO.
       01 MAX-GENERATION PIC 9(2) BINARY VALUE 60.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
           SET ALIVE(60) TO TRUE
           DISPLAY WORLD
           PERFORM VARYING GENERATION FROM 1 BY 1
                   UNTIL GENERATION > MAX-GENERATION
               PERFORM ALIVE-OR-DEAD
               DISPLAY WORLD
           END-PERFORM
           GOBACK
           .
      *-----------------------------------------------------------------
       ALIVE-OR-DEAD.
           INITIALIZE NEXT-WORLD, HP
      *-----------------------------------------------------------------
      * Precompute cell(1) liveness, then computing the next requires
      * only keeping the low 2 bits of HP and a shift left.
      *-----------------------------------------------------------------
           IF ALIVE(1)
               ADD 2 TO HP
           END-IF
           IF ALIVE(2)
               ADD 1 TO HP
           END-IF
           IF HEALTHY
               SET NEXT-ALIVE(1) TO TRUE
           END-IF
           COMPUTE HP = 2 * HP
      *-----------------------------------------------------------------
           PERFORM VARYING POS FROM 2 BY 1
                   UNTIL POS > LENGTH OF WORLD - 1
               IF ALIVE(POS + 1)
                   ADD 1 TO HP
               END-IF
               IF HEALTHY
                   SET NEXT-ALIVE(POS) TO TRUE
               END-IF
               COMPUTE HP = 2 * FUNCTION REM(HP, 4)
           END-PERFORM
      *-----------------------------------------------------------------
           IF HEALTHY
               SET NEXT-ALIVE(LENGTH OF WORLD) TO TRUE
           END-IF
           MOVE NEXT-WORLD TO WORLD
           .
