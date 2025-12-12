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
       01 WORLD-WIDTH           PIC 9(3)   BINARY VALUE 120.
       01 CENTER-POS            PIC 9(3)   BINARY VALUE 60.
       01 WORLD-VIEW.
          03 DISP-GENERATION    PIC Z(4)9  VALUE ZEROS.
          03 FILLER             PIC X      VALUE '.'.
          03 WORLD.
             05 CELL            OCCURS 120 TIMES
                                PIC X      VALUE SPACE.
                88 DEAD                    VALUE ' '.
       01 TRANS-VIEW.
          03 DISP-GENERATION    PIC Z(4)9  VALUE ZEROS.
          03 FILLER             PIC X      VALUE '.'.
          03 TRANS-WORLD.
             05 TRANS-CELL      OCCURS 120 TIMES
                                PIC X      VALUE SPACE.
       01 FILLER.
          03 PAST-WORLD-STATE   OCCURS 16384 TIMES.
             05 PAST-STATE      OCCURS    4 TIMES
                                PIC 9(10)  BINARY VALUE ZERO.
       01 CURRENT-WORLD-STATE.
          03 CURRENT-STATE      OCCURS   4 TIMES
                                PIC 9(10)  BINARY VALUE ZERO.
       01 NEXT-WORLD.
          03 NEXT-CELL          OCCURS 120 TIMES
                                PIC X      VALUE SPACE.
             88 NEXT-DEAD                  VALUE ' '.
       01 HP                    PIC 9(1)   BINARY VALUE ZERO.
       01 GENERATION-IDX        PIC 9(5)   BINARY VALUE ZERO.
       01 GENERATION            PIC 9(5)   BINARY VALUE ZERO.
       01 PREV-GENERATION       PIC 9(5)   BINARY VALUE ZERO.
       01 MAX-GENERATION        PIC 9(5)   BINARY VALUE 60.
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
          03 SW-TRANSITION      PIC X      VALUE SPACE.
             88 TRANSITION                 VALUE 'Y'.
      *-----------------------------------------------------------------
       01 PRT-HEADER0.
          03 FILLER             PIC X(05)    VALUE 'RULE '.
          03 DISP-RULE          PIC ZZ9      VALUE ZEROS.
          03 FILLER             PIC X(12)    VALUE ' GENERATION '.
          03 DISP-CURRENT-GENERATION PIC Z(4)9 VALUE ZEROS.
          03 FILLER             PIC X(14)    VALUE ' LAST SEEN AT '.
          03 DISP-PREV-GENERATION  PIC Z(4)9 VALUE ZEROS.
          03 FILLER             PIC X(08)    VALUE ' PERIOD '.
          03 DISP-PERIOD        PIC Z(4)9    VALUE ZEROS.
       01 PRT-HEADER1.
          03 FILLER             PIC X(05)    VALUE 'RULE '.
          03 DISP-RULE          PIC ZZ9      VALUE ZEROS.
          03 FILLER             PIC X(16)    VALUE ' MAX-GENERATION '.
          03 DISP-MAX-GENERATION PIC Z(4)9   VALUE ZEROS.
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
       01 PRT-HEADER6.
          03 FILLER             PIC X(05)      VALUE 'RULE '.
          03 DISP-RULE          PIC ZZ9        VALUE ZEROS.
          03 FILLER             PIC X(12)      VALUE ' ETERNAL'.
       01 PRT-HEADER7.
          03 FILLER             PIC X(05)      VALUE 'RULE '.
          03 DISP-RULE          PIC ZZ9        VALUE ZEROS.
          03 FILLER             PIC X(12)      VALUE ' DEAD END'.
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
                   MOVE RULE TO DISP-RULE OF PRT-HEADER7
                   DISPLAY PRT-HEADER7
                   EXIT PERFORM CYCLE
               END-IF
               PERFORM SHOW-WORLD
               MOVE CURRENT-WORLD-STATE TO PAST-WORLD-STATE(GENERATION)
               PERFORM VARYING GENERATION FROM 2 BY 1
                       UNTIL (GENERATION > MAX-GENERATION)
                   PERFORM ALIVE-OR-DEAD
                   PERFORM SHOW-WORLD
                   IF SAME-STATE
                       PERFORM SHOW-CYCLE
                       EXIT PERFORM
                   ELSE
                       MOVE CURRENT-WORLD-STATE TO
                            PAST-WORLD-STATE(GENERATION)
                   END-IF
               END-PERFORM
               IF GENERATION > MAX-GENERATION
                   MOVE RULE TO DISP-RULE OF PRT-HEADER6
                   DISPLAY PRT-HEADER6
               END-IF
               IF VERBOSE
                   PERFORM ASK-CONTINUE
               END-IF
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
               SUBTRACT PREV-GENERATION FROM GENERATION
                        GIVING DISP-PERIOD
               DISPLAY PRT-HEADER0
               .
      *-----------------------------------------------------------------
       SHOW-WORLD.
           IF VERBOSE
               IF TRANSITION
                   COMPUTE POS =
                       FUNCTION NUMVAL-C(TRANS-CELL(CENTER-POS))
                       + FUNCTION ORD('A')
                   MOVE FUNCTION CHAR(POS) TO TRANS-CELL(CENTER-POS)
                   MOVE GENERATION TO DISP-GENERATION OF TRANS-VIEW
                   DISPLAY TRANS-VIEW
               ELSE
                   MOVE GENERATION TO DISP-GENERATION OF WORLD-VIEW
                   DISPLAY WORLD-VIEW
               END-IF
           END-IF
           PERFORM CALC-CURRENT-WORLD-STATE
           PERFORM SEARCH-PAST-WORLDS
           .
      *-----------------------------------------------------------------
       INIT-WORLD.
           INITIALIZE WORLD, TRANS-WORLD
           MOVE '*' TO CELL(CENTER-POS)
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
           PERFORM CHECK-VIABLE
           IF VERBOSE
               DISPLAY PRT-HEADER1
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
           IF CYCLIC-WORLD AND NOT DEAD(WORLD-WIDTH)
               ADD 4 TO HP
           END-IF
           IF NOT DEAD(1)
               ADD 2 TO HP
           END-IF
           IF NOT DEAD(2)
               ADD 1 TO HP
           END-IF
           MOVE 1 TO POS
           PERFORM MARK-CELL
           COMPUTE HP = 2 * HP
      *-----------------------------------------------------------------
           PERFORM VARYING POS FROM 2 BY 1
                   UNTIL POS > WORLD-WIDTH - 1
               IF NOT DEAD(POS + 1)
                   ADD 1 TO HP
               END-IF
               PERFORM MARK-CELL
               COMPUTE HP = 2 * FUNCTION REM(HP, 4)
           END-PERFORM
      *-----------------------------------------------------------------
           IF CYCLIC-WORLD AND NOT DEAD(1)
               ADD 1 TO HP
           END-IF
           PERFORM MARK-CELL
           MOVE NEXT-WORLD TO WORLD
           .
      *-----------------------------------------------------------------
       MARK-CELL.
           IF TRANSITION AND HP NOT = 0
               MOVE HP TO TRANS-CELL(POS)
           END-IF
           IF BORN(HP + 1)
               MOVE '*' TO NEXT-CELL(POS)
           END-IF
           .
      *-----------------------------------------------------------------
       CALC-CURRENT-WORLD-STATE.
           MOVE 1 TO POS
           MOVE 1 TO I
           MOVE 1 TO J
           INITIALIZE CURRENT-WORLD-STATE
           PERFORM VARYING POS FROM 1 BY 1 UNTIL POS > WORLD-WIDTH
               COMPUTE CURRENT-STATE(I) = 2 * CURRENT-STATE(I)
               IF CELL(POS) NOT = SPACE
                   COMPUTE CURRENT-STATE(I) = 1 + CURRENT-STATE(I)
               END-IF
               ADD 1 TO J
      * J #bits, if collected more than 30, reset J
      * and go to next state
               IF J > 30
                   ADD 1 TO I
                   MOVE 1 TO J
               END-IF
           END-PERFORM
           .
      *-----------------------------------------------------------------
       SEARCH-PAST-WORLDS.
           SET SAME-STATE TO FALSE
           PERFORM VARYING GENERATION-IDX FROM 1 BY 1
                   UNTIL (GENERATION-IDX > (GENERATION - 1))
                         OR SAME-STATE
               SET SAME-STATE TO TRUE
               PERFORM VARYING I FROM 1 BY 1
                       UNTIL (I > 4) OR (NOT SAME-STATE)
                   IF CURRENT-STATE(I) NOT = 
                           PAST-STATE(GENERATION-IDX, I)
                      SET SAME-STATE TO FALSE
                   END-IF
               END-PERFORM
               IF SAME-STATE
                   MOVE GENERATION-IDX TO PREV-GENERATION
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
                   WHEN 'T'
                        SET TRANSITION TO TRUE
                   WHEN 'W'
                       ADD 1 TO POS
                       PERFORM GET-WIDTH
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
           DISPLAY 'RULE......: ' RULE
           DISPLAY 'WIDTH.....: ' WORLD-WIDTH
           DISPLAY 'VERBOSE...: ' SW-VERBOSE
           DISPLAY 'CYCLIC....: ' SW-CYCLIC-WORLD
           DISPLAY 'TRANSITION: ' SW-TRANSITION
           .
       GET-WIDTH.
           MOVE FUNCTION NUMVAL(ARGV(POS)) TO WORLD-WIDTH
           IF WORLD-WIDTH < 1 OR WORLD-WIDTH > 120
               MOVE 120 TO WORLD-WIDTH
           END-IF
           COMPUTE CENTER-POS = WORLD-WIDTH / 2
           .
       GET-RULE.
           MOVE FUNCTION NUMVAL(ARGV(POS)) TO RULE
           .
       GET-MAX-GENERATION.
           MOVE FUNCTION NUMVAL(ARGV(POS)) TO MAX-GENERATION
           IF MAX-GENERATION > 16384
               MOVE 16384 TO MAX-GENERATION
           END-IF
           .
      *-----------------------------------------------------------------
       CHECK-VIABLE.
           SET DODOID TO TRUE
           IF BORN(1) OR BORN(2) OR BORN(3) OR BORN(5)
               SET VIABLE TO TRUE
           END-IF
           .
      *-----------------------------------------------------------------
       SHOW-USAGE.
           DISPLAY 'USAGE: RULE [VQFCT] [W <WIDTH>]'
                   ' <RULE-NUMBER> [MAX-GENERATION]'
           MOVE 1000 TO RETURN-CODE
           GOBACK
           .
