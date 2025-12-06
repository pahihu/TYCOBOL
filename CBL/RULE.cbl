      ******************************************************************
      *                                                                *
      *             Wolfram's Rule N cellular automata                 *
      *                                                                *
      ******************************************************************
000000*AA1ABBB+----2----+----3----+----4----+----5----+----6----+----7--SEQUENCE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RULE.
       AUTHOR. Andras Pahi.
       DATE-WRITTEN. 2025-12-04.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WORLD-VIEW.
          03 DISP-GENERATION    PIC 9(3)   VALUE ZEROS.
          03 WORLD.
             05 CELL            OCCURS 120 TIMES
                                PIC X      VALUE SPACE.
                88 ALIVE                   VALUE '*'.
       01 FILLER.
          03 PAST-WORLD         OCCURS 999 TIMES.
             05 PAST-STATE      OCCURS   4 TIMES
                                PIC 9(10)  BINARY VALUE ZERO.
       01 CURRENT-WORLD.
          03 CURRENT-STATE      OCCURS   4 TIMES
                                PIC 9(10)  BINARY VALUE ZERO.
       01 NEXT-WORLD.
          03 NEXT-CELL          OCCURS 120 TIMES
                                PIC X      VALUE SPACE.
             88 NEXT-ALIVE                 VALUE '*'.
       01 HP                    PIC 9(1)   BINARY VALUE ZERO.
       01 GENERATION            PIC 9(3)   BINARY VALUE ZERO.
       01 PREV-GENERATION       PIC 9(3)   BINARY VALUE ZERO.
       01 MAX-GENERATION        PIC 9(3)   BINARY VALUE 60.
       01 POS                   PIC 9(3)   BINARY VALUE ZERO.
       01 I                     PIC 9(3)   BINARY VALUE ZERO.
       01 J                     PIC 9(3)   BINARY VALUE ZERO.
       01 RULE                  PIC 9(3)   BINARY VALUE ZERO.
       01 RULE-PATTERNS.
          03 PATTERN            OCCURS   8 TIMES
                                PIC 9      VALUE ZERO.
             88 BORN                       VALUE 1.
      *-----------------------------------------------------------------
       01 SWITCHES.
          03 SW-SAME-STATE      PIC X      VALUE SPACE.
             88 SAME-STATE                 VALUE 'Y'
                                           WHEN SET TO FALSE ' '.
          03 SW-DONE            PIC X      VALUE SPACE.
             88 DONE                       VALUE 'Y'
                                           WHEN SET TO FALSE ' '.
      *-----------------------------------------------------------------
       01 PRT-HEADER0.
          03 FILLER             PIC X(13)  VALUE 'LAST SEEN AT '.
          03 DISP-GENERATION    PIC 9(03)  VALUE ZEROS.
       01 PRT-HEADER1.
          03 FILLER             PIC X(05)  VALUE 'RULE '.
          03 DISP-RULE          PIC 9(03)  VALUE ZEROS.
          03 FILLER             PIC X(16)  VALUE ' MAX-GENERATION '.
          03 DISP-MAX-GENERATION PIC 9(03) VALUE ZEROS.
       01 PRT-HEADER2           PIC X(48)
          VALUE ' +---+ +---+ +---+ +---+ +---+ +---+ +---+ +---+'.
       01 PRT-HEADER3
          PIC X(48)
          VALUE ' |***| |** | |* *| |*  | | **| | * | |  *| |   |'.
       01 PRT-HEADER4.
          03 FILLER             OCCURS 8 TIMES.
             05 FILLER          PIC XXX    VALUE '  |'.
             05 STATE           PIC X      VALUE SPACE.
             05 FILLER          PIC XX     VALUE '| '.
       01 PRT-HEADER5
          PIC X(48)
          VALUE '  +-+   +-+   +-+   +-+   +-+   +-+   +-+   +-+ '.
      *-----------------------------------------------------------------
       COPY CALLIO.
       LINKAGE SECTION.
       COPY ARGS.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION USING ARGUMENTS.
           PERFORM GET-ARGS
           PERFORM UNTIL EXIT
               SET DONE TO FALSE
               PERFORM INIT-WORLD
               PERFORM SHOW-WORLD
               MOVE CURRENT-WORLD TO PAST-WORLD(GENERATION)
               PERFORM VARYING GENERATION FROM 2 BY 1
                       UNTIL (GENERATION > MAX-GENERATION) OR DONE
                   PERFORM ALIVE-OR-DEAD
                   PERFORM SHOW-WORLD
                   IF SAME-STATE
                       MOVE PREV-GENERATION TO
                            DISP-GENERATION OF PRT-HEADER0
                       DISPLAY PRT-HEADER0
                       SET DONE TO TRUE
                   ELSE
                       MOVE CURRENT-WORLD TO PAST-WORLD(GENERATION)
                   END-IF
               END-PERFORM
      *.................................................................
               MOVE 'Press Enter to continue...*' TO IO-P-STRING
               SET IOP-ACCEPT TO TRUE
               CALL 'GNUIO' USING IO-PARAMS
      *.................................................................
               IF IO-P-CHAR NOT = ' '
                   GOBACK
               END-IF
               COMPUTE RULE = RULE + 1
           END-PERFORM
           .
      *-----------------------------------------------------------------
       SHOW-WORLD.
           MOVE GENERATION TO DISP-GENERATION OF WORLD-VIEW
           DISPLAY WORLD-VIEW
           PERFORM CALC-CURRENT-WORLD-STATE
           PERFORM SEARCH-PAST-WORLDS
           .
      *-----------------------------------------------------------------
       INIT-WORLD.
           INITIALIZE WORLD
           SET ALIVE(60) TO TRUE
           MOVE 1 TO GENERATION

           PERFORM INIT-RULE
           .
      *-----------------------------------------------------------------
       INIT-RULE.
           MOVE RULE TO DISP-RULE
           MOVE MAX-GENERATION TO DISP-MAX-GENERATION
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 8
               MOVE ' ' TO STATE(I)
           END-PERFORM
           INITIALIZE RULE-PATTERNS
           MOVE RULE TO POS
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 8
               COMPUTE J = FUNCTION REM(POS, 2)
               IF 1 = J
                   SET BORN(I) TO TRUE
                   MOVE '*' TO STATE(9 - I)
               END-IF
               COMPUTE POS = POS / 2
           END-PERFORM
           DISPLAY PRT-HEADER1
           PERFORM CHECK-VIABLE
           DISPLAY PRT-HEADER2
           DISPLAY PRT-HEADER3
           DISPLAY PRT-HEADER2
           DISPLAY PRT-HEADER4
           DISPLAY PRT-HEADER5
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
           IF BORN(HP + 1)
               SET NEXT-ALIVE(1) TO TRUE
           END-IF
           COMPUTE HP = 2 * HP
      *-----------------------------------------------------------------
           PERFORM VARYING POS FROM 2 BY 1
                   UNTIL POS > LENGTH OF WORLD - 1
               IF ALIVE(POS + 1)
                   ADD 1 TO HP
               END-IF
               IF BORN(HP + 1)
                   SET NEXT-ALIVE(POS) TO TRUE
               END-IF
               COMPUTE HP = 2 * FUNCTION REM(HP, 4)
           END-PERFORM
      *-----------------------------------------------------------------
           IF BORN(HP + 1)
               SET NEXT-ALIVE(LENGTH OF WORLD) TO TRUE
           END-IF
           MOVE NEXT-WORLD TO WORLD
           .
      *-----------------------------------------------------------------
       CALC-CURRENT-WORLD-STATE.
           MOVE 1 TO POS
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 4
               INITIALIZE CURRENT-STATE(I)
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 30
                   COMPUTE CURRENT-STATE(I) = 2 * CURRENT-STATE(I)
                   IF CELL(POS) = '*'
                       COMPUTE CURRENT-STATE(I) = 1 + CURRENT-STATE(I)
                   END-IF
                   COMPUTE POS = POS + 1
               END-PERFORM
           END-PERFORM
           .
      *-----------------------------------------------------------------
       SEARCH-PAST-WORLDS.
           SET SAME-STATE TO FALSE
           PERFORM VARYING POS FROM 1 BY 1
                   UNTIL (POS > (GENERATION - 1)) OR SAME-STATE
               SET SAME-STATE TO TRUE
               PERFORM VARYING I FROM 1 BY 1
                       UNTIL (I > 4) OR (NOT SAME-STATE)
                   IF CURRENT-STATE(I) NOT = PAST-STATE(POS, I)
                      SET SAME-STATE TO FALSE
                   END-IF
               END-PERFORM
               IF SAME-STATE
                   MOVE POS TO PREV-GENERATION
               END-IF
           END-PERFORM
           .
      *-----------------------------------------------------------------
       GET-ARGS.
           EVALUATE ARGC
               WHEN 1
                   PERFORM GET-RULE
               WHEN 2
                   PERFORM GET-RULE
                   PERFORM GET-MAX-GENERATION
               WHEN OTHER
                   PERFORM SHOW-USAGE
           END-EVALUATE
           .
       GET-RULE.
           MOVE FUNCTION NUMVAL(ARGV(1)) TO RULE
           .
       GET-MAX-GENERATION.
           MOVE FUNCTION NUMVAL(ARGV(2)) TO MAX-GENERATION
           .
      *-----------------------------------------------------------------
       CHECK-VIABLE.
           IF BORN(1) OR BORN(2) OR BORN(3) OR BORN(5)
               CONTINUE
           ELSE
               DISPLAY 'THIS RULE IS NOT VIABLE'
               SET DONE TO TRUE
           END-IF
           .
      *-----------------------------------------------------------------
       SHOW-USAGE.
           DISPLAY 'USAGE: RULE <RULE-NUMBER>'
           MOVE 1000 TO RETURN-CODE
           GOBACK
           .
