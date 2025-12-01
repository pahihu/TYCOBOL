000000*AA1ABBB+----2----+----3----+----4----+----5----+----6----+----7--SEQUENCE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRMAN2.
       AUTHOR. Andras Pahi.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-STR          PIC X(30) VALUE 'TEST FIELD'.
       01 WS-STR-POINTER  PIC 9(02) VALUE ZEROS.
       01 LAST-DELIM      PIC X(30) VALUE SPACES.
       01 NUM-FIELDS      PIC 9(02) VALUE ZEROS.
       01 NUM-CHARS.
          03 NUM-CHARS-FIRST    PIC 9(02) VALUE ZEROS.
          03 NUM-CHARS-MIDDLE   PIC 9(02) VALUE ZEROS.
          03 NUM-CHARS-LAST     PIC 9(02) VALUE ZEROS.
       01 NAME.
          03 FIRST-NAME     PIC X(20) VALUE SPACES.
          03 MIDDLE-NAME    PIC X(20) VALUE SPACES.
          03 LAST-NAME      PIC X(05) VALUE SPACES.
       01 PHONE-NUMBER.
          03 AREA-CODE      PIC 999     VALUE 409.
          03 PREFIX-NUM     PIC 999     VALUE 555.
          03 LAST-FOUR      PIC 9(4)    VALUE 1212.
       01 FORMATTED-NUMBER  PIC X(14)   VALUE SPACES.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
           DISPLAY ' '
           DISPLAY 'STRING WITH POINTER'
           MOVE 6 TO WS-STR-POINTER
           STRING 'FILES' DELIMITED BY SIZE
                  INTO WS-STR
                  WITH POINTER WS-STR-POINTER
           DISPLAY 'WS-STR.......... : ' WS-STR
           DISPLAY 'WS-STR-POINTER...: ' WS-STR-POINTER
      *-----------------------------------------------------------------
           MOVE 1 TO WS-STR-POINTER
           IF AREA-CODE NOT = 0 THEN
               STRING '('       DELIMITED BY SIZE
                      AREA-CODE DELIMITED BY SIZE
                      ') '      DELIMITED BY SIZE
                      INTO FORMATTED-NUMBER
                      WITH POINTER WS-STR-POINTER
           END-IF
           STRING PREFIX-NUM DELIMITED BY SIZE
                  '-'        DELIMITED BY SIZE
                  LAST-FOUR  DELIMITED BY SIZE
                  INTO FORMATTED-NUMBER
                  WITH POINTER WS-STR-POINTER
           DISPLAY 'FORMATTED-NUMBER...: ' FORMATTED-NUMBER
      *-----------------------------------------------------------------
           DISPLAY ' '
           DISPLAY 'UNSTRING'
           MOVE 'John  Joe   Jones' TO WS-STR
           INSPECT WS-STR
               REPLACING ALL ' ' BY '*'
           DISPLAY 'NAME..........: ' WS-STR
           INSPECT WS-STR
               REPLACING ALL '*' BY ' '
           INITIALIZE NAME
           UNSTRING WS-STR DELIMITED BY ALL SPACE
               INTO FIRST-NAME, MIDDLE-NAME, LAST-NAME
           END-UNSTRING
           PERFORM SHOW-NAMES
      *-----------------------------------------------------------------
           MOVE 'Jones,Joe John' TO WS-STR
           MOVE 'Kowalsky,Joe John   Jeremy' TO WS-STR
           DISPLAY 'NAME..........: ' WS-STR
           INITIALIZE NAME, LAST-DELIM
           INITIALIZE NUM-FIELDS, NUM-CHARS
           MOVE 1 TO WS-STR-POINTER
      * NB. THE SEPARATOR IS EITHER SPACES OR COMMAS
      *     SO 'Jones, Joe John' IS 4 FIELDS
           UNSTRING WS-STR
               DELIMITED BY ALL SPACE OR ALL ','
               INTO
                   LAST-NAME
                       DELIMITER IN LAST-DELIM
                       COUNT IN NUM-CHARS-LAST,
                   MIDDLE-NAME
                       DELIMITER IN LAST-DELIM
                       COUNT IN NUM-CHARS-MIDDLE,
                   FIRST-NAME
                       DELIMITER IN LAST-DELIM
                       COUNT IN NUM-CHARS-FIRST
               WITH POINTER WS-STR-POINTER
               TALLYING IN NUM-FIELDS
               ON OVERFLOW
                   DISPLAY 'Split name overflow.'
                   DISPLAY 'String:=''' WS-STR ''''
                   MOVE '^' TO LAST-DELIM(WS-STR-POINTER:1)
                   DISPLAY '        ' LAST-DELIM(1:WS-STR-POINTER)
           END-UNSTRING
           PERFORM SHOW-NAMES
      *-----------------------------------------------------------------
           GOBACK
           .
      *-----------------------------------------------------------------
       SHOW-NAMES.
           DISPLAY '#FIELDS.......: ' NUM-FIELDS
           DISPLAY '#FIRST-CHARS..: ' NUM-CHARS-FIRST
           DISPLAY '#MIDDLE-CHARS.: ' NUM-CHARS-MIDDLE
           DISPLAY '#LAST-CHARS...: ' NUM-CHARS-LAST
           DISPLAY 'FIRST-NAME....: ' FIRST-NAME
           DISPLAY 'MIDDLE-NAME...: ' MIDDLE-NAME
           DISPLAY 'LAST-NAME.....: ' LAST-NAME
           DISPLAY ' '
           .
