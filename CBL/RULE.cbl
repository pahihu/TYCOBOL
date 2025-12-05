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
       01 WORLD.
          03 CELL        OCCURS 120 TIMES
                         PIC X     VALUE SPACE.
             88 ALIVE              VALUE '*'.
       01 PAST-WORLD     OCCURS 999 TIMES.
          03 PAST-STATE  OCCURS   4 TIMES
                         PIC 9(9)   BINARY VALUE ZERO.
       01 CURRENT-WORLD.
          03 CURRENT-STATE    OCCURS   4 TIMES
                              PIC 9(9)   BINARY VALUE ZERO.
       01 NEXT-WORLD.
          03 NEXT-CELL   OCCURS 120 TIMES
                         PIC X     VALUE SPACE.
             88 NEXT-ALIVE         VALUE '*'.
       01 POS            PIC 9(3)  BINARY VALUE ZERO.
       01 I              PIC 9(3)  BINARY VALUE ZERO.
       01 J              PIC 9(3)  BINARY VALUE ZERO.
       01 HP             PIC 9(1)  BINARY VALUE ZERO.
       01 PAST-GENERATION   PIC 9(3)  BINARY VALUE ZERO.
       01 GENERATION        PIC 9(3)  BINARY VALUE ZERO.
       01 MAX-GENERATION    PIC 9(3)  BINARY VALUE 60.
       01 RULE           PIC 9(3)  BINARY VALUE ZERO.
       01 DISP-RULE      PIC 9(3)  VALUE ZERO.
       01 DISP-MAX-GENERATION PIC 9(3)  VALUE ZERO.
       01 ODD            PIC 9     BINARY VALUE ZERO.
       01 RULE-PATTERNS.
          03 PATTERN     OCCURS 8 TIMES
                         PIC 9     VALUE ZERO.
             88 BORN               VALUE 1.
       01 ARGC           PIC 9(3)  VALUE 0.
       01 ARG            PIC X(80) VALUE SPACES.
       01 RULE-STATES.
          03 RULE-HEADER1.
             05 FILLER   OCCURS 8 TIMES.
                07 FILLER      PIC X(06) VALUE ' +---+'.
          03 RULE-HEADER2
             PIC X(48)
             VALUE ' |***| |** | |* *| |*  | | **| | * | |  *| |   |'.
          03 RULE-HEADER3.
             05 FILLER  OCCURS 8 TIMES.
                07 FILLER   PIC XXX VALUE '  |'.
                07 STATE    PIC X   VALUE SPACE.
                07 FILLER   PIC XX  VALUE '| '.
          03 RULE-HEADER4.
             05 FILLER  OCCURS 8 TIMES.
                07 FILLER      PIC X(06) VALUE '  +-+ '.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
           PERFORM INIT-WORLD
           DISPLAY WORLD
           PERFORM VARYING GENERATION FROM 1 BY 1
                   UNTIL GENERATION > MAX-GENERATION
               PERFORM ALIVE-OR-DEAD
               DISPLAY WORLD
               PERFORM CALC-CURRENT-WORLD-STATE
               PERFORM SEARCH-PAST-WORLDS
           END-PERFORM
           GOBACK
           .
      *-----------------------------------------------------------------
       INIT-WORLD.
           SET ALIVE(60) TO TRUE

           PERFORM GET-ARGS
           PERFORM INIT-RULE
           .
      *-----------------------------------------------------------------
       INIT-RULE.
           MOVE RULE TO DISP-RULE
           MOVE MAX-GENERATION TO DISP-MAX-GENERATION
           DISPLAY 'RULE ' DISP-RULE
                   ' MAX-GENERATION ' DISP-MAX-GENERATION
           PERFORM VARYING POS FROM 1 BY 1
                   UNTIL POS > 8
               COMPUTE ODD = FUNCTION REM(RULE, 2)
               IF 1 = ODD
                   SET BORN(POS) TO TRUE
                   MOVE '*' TO STATE(9 - POS)
               END-IF
               COMPUTE RULE = RULE / 2
           END-PERFORM
           PERFORM CHECK-VIABLE
           DISPLAY RULE-HEADER1
           DISPLAY RULE-HEADER2
           DISPLAY RULE-HEADER1
           DISPLAY RULE-HEADER3
           DISPLAY RULE-HEADER4
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
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 4
               INITIALIZE CURRENT-STATE(I)
               PERFORM VARYING J FROM 1 BY 1
                       UNTIL J > 30
                   COMPUTE CURRENT-STATE(I) =
                           2 * CURRENT-STATE(I)
                   IF '*' = CELL(POS)
                       COMPUTE CURRENT-STATE(I) =
                           1 + CURRENT-STATE(I)
                   END-IF
               END-PERFORM
               DISPLAY 'STATE(' I ')=' CURRENT-STATE(I)
           END-PERFORM
           .
      *-----------------------------------------------------------------
       SEARCH-PAST-WORLDS.
           INITIALIZE PAST-GENERATION
           PERFORM VARYING POS FROM 1 BY 1
                   UNTIL (POS = GENERATION) OR (PAST-GENERATION > 0)
               PERFORM COMPARE-WORLD-STATE
               IF PAST-GENERATION > 0
                   DISPLAY 'LAST SEEN AT ' PAST-GENERATION
                   GOBACK
               END-IF
           END-PERFORM
           MOVE CURRENT-WORLD TO PAST-WORLD(GENERATION)
           .
      *-----------------------------------------------------------------
       COMPARE-WORLD-STATE.
           MOVE POS TO PAST-GENERATION
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL (I > 4) OR (0 = PAST-GENERATION)
               IF CURRENT-STATE(I) NOT = PAST-STATE(POS, I)
                  INITIALIZE PAST-GENERATION
               END-IF
           END-PERFORM
           .
      *-----------------------------------------------------------------
       GET-ARGS.
           ACCEPT ARGC FROM ARGUMENT-NUMBER
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
           DISPLAY   1 UPON ARGUMENT-NUMBER
           ACCEPT  ARG FROM ARGUMENT-VALUE
           MOVE FUNCTION NUMVAL(ARG) TO RULE
           .
       GET-MAX-GENERATION.
           DISPLAY   2 UPON ARGUMENT-NUMBER
           ACCEPT  ARG FROM ARGUMENT-VALUE
           MOVE FUNCTION NUMVAL(ARG) TO MAX-GENERATION
           .
      *-----------------------------------------------------------------
       CHECK-VIABLE.
           IF BORN(1) OR BORN(2) OR BORN(3) OR BORN(5)
               CONTINUE
           ELSE
               DISPLAY 'THIS RULE IS NOT VIABLE'
               GOBACK
           END-IF
           .
      *-----------------------------------------------------------------
       SHOW-USAGE.
           DISPLAY 'USAGE: RULE <RULE-NUMBER>'
           MOVE 1000 TO RETURN-CODE
           GOBACK
           .
