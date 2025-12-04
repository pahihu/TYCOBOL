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
       01 WORLD.
          03 CELL        OCCURS 120 TIMES
                         PIC X VALUE SPACE.
             88 ALIVE             VALUE '*'.
       01 NEXT-WORLD.
          03 NEXT-CELL   OCCURS 120 TIMES
                         PIC X VALUE SPACE.
             88 NEXT-ALIVE        VALUE '*'.
       01 POS            PIC 9(3) VALUE ZERO.
       01 CENTER         PIC 9(3) VALUE ZERO.
       01 HP             PIC 9(2) VALUE ZERO.
          88 HEALTHY              VALUE 1 THRU 4.
       01 GENERATION     PIC 9(2) VALUE ZERO.
       01 MAX-GENERATION PIC 9(3) VALUE 60.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
           SET ALIVE(60) TO TRUE
           PERFORM SHOW-WORLD
           PERFORM VARYING GENERATION FROM 1 BY 1
                   UNTIL GENERATION > MAX-GENERATION
               PERFORM ALIVE-OR-DEAD
               PERFORM SHOW-WORLD
           END-PERFORM
           GOBACK
           .
      *-----------------------------------------------------------------
       ALIVE-OR-DEAD.
           INITIALIZE NEXT-WORLD
           PERFORM VARYING POS FROM 0 BY 1
                   UNTIL POS > LENGTH OF WORLD
               INITIALIZE HP
               IF (0 < POS AND POS < LENGTH OF WORLD)
                  AND ALIVE(POS)
                   ADD 4 TO HP
               END-IF
               IF (POS + 1 < LENGTH OF WORLD)
                  AND ALIVE(POS + 1)
                   ADD 2 TO HP
               END-IF
               IF (POS + 2 < LENGTH OF WORLD)
                  AND ALIVE(POS + 2)
                   ADD 1 TO HP
               END-IF
      D        IF HP > 0
      D            DISPLAY 'POS=' POS ' HP=' HP ' HEALTHY='
      D            WITH NO ADVANCING
      D            IF HEALTHY
      D               DISPLAY 'Y'
      D            ELSE
      D               DISPLAY 'N'
      D            END-IF
      D        END-IF
               IF HEALTHY AND (POS + 1 < LENGTH OF WORLD)
      D            DISPLAY 'HEALTHY HP=' HP
                   COMPUTE CENTER = POS + 1
                   SET NEXT-ALIVE(CENTER) TO TRUE
               END-IF
           END-PERFORM
           MOVE NEXT-WORLD TO WORLD
           .
      *-----------------------------------------------------------------
       SHOW-WORLD.
           DISPLAY ' '
           PERFORM VARYING POS FROM 1 BY 1
                   UNTIL POS > LENGTH OF WORLD
               DISPLAY CELL(POS) WITH NO ADVANCING
           END-PERFORM
           .
