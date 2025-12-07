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
          03 SW-VERBOSE         PIC X      VALUE SPACE.
             88 VERBOSE                    VALUE 'Y'.
             88 QUIET                      VALUE 'N'.
          03 SW-CYCLIC-WORLD    PIC X      VALUE SPACE.
             88 FLAT-WORLD                 VALUE 'N'.
             88 CYCLIC-WORLD               VALUE 'Y'.
          03 SW-VIABLE          PIC X      VALUE SPACE.
             88 DODOID                     VALUE 'N'.
             88 VIABLE                     VALUE 'Y'.
      *-----------------------------------------------------------------
       01 PRT-HEADER0.
          03 FILLER             PIC X(05)      VALUE 'RULE '.
          03 DISP-RULE          PIC 9(03)      VALUE ZEROS.
          03 FILLER             PIC X(12)      VALUE ' GENERATION '.
          03 DISP-CURRENT-GENERATION PIC 9(03) VALUE ZEROS.
          03 FILLER             PIC X(14)      VALUE ' LAST SEEN AT '.
          03 DISP-PREV-GENERATION    PIC 9(03) VALUE ZEROS.
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
           PERFORM VARYING RULE FROM RULE BY 1 UNTIL RULE > 255
               PERFORM INIT-WORLD
               IF NOT VIABLE
                   DISPLAY 'THIS RULE IS NOT VIABLE'
                   EXIT PERFORM CYCLE
               END-IF
               PERFORM SHOW-WORLD
               MOVE CURRENT-WORLD TO PAST-WORLD(GENERATION)
               PERFORM VARYING GENERATION FROM 2 BY 1
                       UNTIL (GENERATION > MAX-GENERATION)
                   PERFORM ALIVE-OR-DEAD
                   PERFORM SHOW-WORLD
                   IF SAME-STATE
                       PERFORM SHOW-CYCLE
                       EXIT PERFORM
                   ELSE
                       MOVE CURRENT-WORLD TO PAST-WORLD(GENERATION)
                   END-IF
               END-PERFORM
               PERFORM ASK-CONTINUE
           END-PERFORM
           GOBACK
           .
      *-----------------------------------------------------------------
       ASK-CONTINUE.
               MOVE 'Press Enter to continue...*' TO IO-P-STRING
               SET IOP-ACCEPT TO TRUE
               CALL 'GNUIO' USING IO-PARAMS
               IF IO-P-CHAR NOT = ' '
                   GOBACK
               END-IF
               .
       SHOW-CYCLE.
               MOVE RULE TO DISP-RULE OF PRT-HEADER0
               MOVE GENERATION TO
                    DISP-CURRENT-GENERATION
               MOVE PREV-GENERATION TO
                    DISP-PREV-GENERATION
               DISPLAY PRT-HEADER0
      *-----------------------------------------------------------------
       SHOW-WORLD.
           MOVE GENERATION TO DISP-GENERATION OF WORLD-VIEW
           IF VERBOSE
               DISPLAY WORLD-VIEW
           END-IF
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
           MOVE RULE TO DISP-RULE OF PRT-HEADER1
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
           IF VERBOSE
               DISPLAY PRT-HEADER2
               DISPLAY PRT-HEADER3
               DISPLAY PRT-HEADER2
               DISPLAY PRT-HEADER4
               DISPLAY PRT-HEADER5
           END-IF
           .
      *-----------------------------------------------------------------
       ALIVE-OR-DEAD.
           INITIALIZE NEXT-WORLD, HP
      *-----------------------------------------------------------------
      * Precompute cell(1) liveness, then computing the next requires
      * only keeping the low 2 bits of HP and a shift left.
      *-----------------------------------------------------------------
           IF CYCLIC-WORLD AND ALIVE(LENGTH OF WORLD)
               ADD 4 TO HP
           END-IF
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
           IF CYCLIC-WORLD AND ALIVE(1)
               ADD 1 TO HP
           END-IF
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
           SET VERBOSE TO TRUE
           SET FLAT-WORLD TO TRUE
           MOVE 1 TO I
           PERFORM VARYING POS FROM 1 BY 1 UNTIL POS > ARGC
               EVALUATE ARGV(POS)
                   WHEN 'Q'
                       SET QUIET TO TRUE
                   WHEN 'V'
                       SET VERBOSE TO TRUE
                   WHEN 'F'
                       SET FLAT-WORLD TO TRUE
                   WHEN 'C'
                       SET CYCLIC-WORLD TO TRUE
                   WHEN OTHER
                       EVALUATE I
                           WHEN 1
                               PERFORM GET-RULE
                               MOVE 2 TO I
                           WHEN 2
                               PERFORM GET-MAX-GENERATION
                               MOVE 3 TO I
                           WHEN OTHER
                               PERFORM SHOW-USAGE
                       END-EVALUATE
               END-EVALUATE
           END-PERFORM
           IF I = 1
               PERFORM SHOW-USAGE
           END-IF
           .
       GET-RULE.
           MOVE FUNCTION NUMVAL(ARGV(POS)) TO RULE
           .
       GET-MAX-GENERATION.
           MOVE FUNCTION NUMVAL(ARGV(POS)) TO MAX-GENERATION
           .
      *-----------------------------------------------------------------
       CHECK-VIABLE.
           IF BORN(1) OR BORN(2) OR BORN(3) OR BORN(5)
               SET VIABLE TO TRUE
           ELSE
               SET DODOID TO TRUE
           END-IF
           .
      *-----------------------------------------------------------------
       SHOW-USAGE.
           DISPLAY 'USAGE: RULE [VQFC] <RULE-NUMBER> [MAX-GENERATION]'
           MOVE 1000 TO RETURN-CODE
           GOBACK
           .
