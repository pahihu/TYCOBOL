000000*AA1ABBB+----2----+----3----+----4----+----5----+----6----+----7--SEQUENCE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INIT.
       AUTHOR. Andras Pahi.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-FIELD                      PIC 9(5) VALUE 11235.
       01 WS-NAME                       PIC X(20)
             VALUE '***DOOER, JOHN**'.
       01 WS-COUNTER                    PIC 9(2) VALUE ZERO.
       01 WS-DATE                       PIC X(10) VALUE '11/29/2025'.
       01 CURRENT-DATE                  PIC X(6).
       01 WS-FORMATTED-DATE             PIC X(10) VALUE 'Mm/Dd/Yy'.
       01 PHONE-NUMBER.
          03 AREA-CODE      PIC XXX     VALUE '409'.
          03 PREFIX-NUM     PIC XXX     VALUE '555'.
          03 LAST-FOUR      PIC X(4)    VALUE '1212'.
       01 FORMATTED-NUM     PIC X(14)   VALUE '(XXX) YYY-ZZZZ'.
       01 FORMATTED-ALT     PIC X(14)   VALUE '(XXX) XXX-XXXX'.
       01 DARLENE           PIC X(20)   VALUE 'Hubbel, Darlene'.
       01 SAMPLE-TEXT
             PIC X(36)
             VALUE 'INTER PYGMAEOS NOT PUDET ESSE BREVEM'.
       01 WORKING-VARIABLES.
          03 NUMERIC-VARIABLES.
             05 WS-NUM1                 PIC 9(5).
             05 WS-NUM2                 PIC 9(5).
          03 NUMERIC-EDITED-VARIABLES.
             05 WS-NUMED1               PIC ZZ,ZZZ.99.
          03 ALPHANUMERIC-VARIABLES.
             05 WS-ALNUM1               PIC X(20) VALUE ALL '*'.
             05 WS-ALNUM2               PIC X(20).
          03 ALHANUMERIC-EDITED-VARIABLES.
             05 WS-ALNUMED1             PIC XX/XXXBXXX.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
           PERFORM FILL-VARIABLES
           PERFORM SHOW-VARIABLES
           DISPLAY 'INITIALIZE ALL'
           PERFORM FILL-VARIABLES
           INITIALIZE WORKING-VARIABLES
           PERFORM SHOW-VARIABLES
           DISPLAY 'INITIALIZE REPLACE NUMERIC'
           PERFORM FILL-VARIABLES
           INITIALIZE WORKING-VARIABLES REPLACING NUMERIC DATA BY 9
           PERFORM SHOW-VARIABLES
           DISPLAY 'INITIALIZE WS-ALNUM1'
           PERFORM FILL-VARIABLES
           INITIALIZE WS-ALNUM1 REPLACING ALPHANUMERIC DATA BY ALL '+'
           PERFORM SHOW-VARIABLES
           DISPLAY 'INITIALIZE BY'
           PERFORM FILL-VARIABLES
           INITIALIZE WORKING-VARIABLES
               REPLACING NUMERIC DATA BY WS-FIELD
      *-----------------------------------------------------------------
           DISPLAY 'INSPECT TALLYING'
           INITIALIZE WS-COUNTER
           INSPECT WS-NAME TALLYING WS-COUNTER FOR ALL ','
           PERFORM SHOW-COUNTER
      *-----------------------------------------------------------------
           DISPLAY 'INSPECT TALLYING BEFORE'
           INITIALIZE WS-COUNTER
           INSPECT WS-NAME
               TALLYING WS-COUNTER
               FOR ALL 'O'
               BEFORE INITIAL ','
           PERFORM SHOW-COUNTER
      *-----------------------------------------------------------------
           DISPLAY 'INSPECT TALLYING AFTER'
           INITIALIZE WS-COUNTER
           INSPECT WS-NAME
               TALLYING WS-COUNTER
               FOR ALL 'O'
               AFTER INITIAL  ','
           PERFORM SHOW-COUNTER
      *-----------------------------------------------------------------
           DISPLAY 'INSPECT TALLYING LEADING'
           INITIALIZE WS-COUNTER
           INSPECT WS-NAME
               TALLYING WS-COUNTER
               FOR LEADING '*'
           PERFORM SHOW-COUNTER
      *-----------------------------------------------------------------
           DISPLAY 'INSPECT TALLYING FOR BEFORE'
           INITIALIZE WS-COUNTER
           INSPECT WS-NAME
               TALLYING WS-COUNTER
               FOR CHARACTERS
               BEFORE INITIAL ','
           PERFORM SHOW-COUNTER
      *-----------------------------------------------------------------
           DISPLAY 'INSPECT TALLYING FOR AFTER'
           INITIALIZE WS-COUNTER
           INSPECT WS-NAME
               TALLYING WS-COUNTER
               FOR CHARACTERS
               AFTER INITIAL ','
           PERFORM SHOW-COUNTER
      *-----------------------------------------------------------------
           DISPLAY 'INSPECT REPLACING'
           INITIALIZE WS-COUNTER
           INSPECT WS-DATE
               REPLACING ALL '/' BY '-'
           PERFORM SHOW-COUNTER
      *-----------------------------------------------------------------
           DISPLAY 'INSPECT REPLACING MULTIPLE'
           INSPECT FORMATTED-NUM
               REPLACING ALL 'XXX'  BY AREA-CODE
                         ALL 'YYY'  BY PREFIX-NUM
                         ALL 'ZZZZ' BY LAST-FOUR
           INSPECT FORMATTED-ALT
               REPLACING FIRST 'XXX'  BY AREA-CODE
                         FIRST 'XXX'  BY PREFIX-NUM
                         FIRST 'XXXX' BY LAST-FOUR
           PERFORM SHOW-PHONE-NUMBER
      *-----------------------------------------------------------------
           DISPLAY 'INSPECT REPLACING ALL'
           PERFORM SHOW-VARIABLES
           INSPECT WS-ALNUM1
               REPLACING CHARACTERS BY '%'
           PERFORM SHOW-VARIABLES
      *-----------------------------------------------------------------
           DISPLAY 'INSPECT TALLYING REPLACING'
           PERFORM SHOW-VARIABLES
           PERFORM SHOW-COUNTER
           INSPECT WS-ALNUMED1
               TALLYING WS-COUNTER FOR ALL SPACES AFTER '/'
               REPLACING ALL SPACES BY '*' AFTER '/'
           PERFORM SHOW-VARIABLES
           PERFORM SHOW-COUNTER
      *-----------------------------------------------------------------
           DISPLAY 'INSPECT CONVERTING'
           DISPLAY DARLENE
           DISPLAY ' '
           INSPECT DARLENE
               CONVERTING 'abcdefghijklmnopqrstuvwxyz'
               TO 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
           DISPLAY DARLENE
           DISPLAY ' '
           MOVE 'Hubbel, Darlene' TO DARLENE
           INSPECT DARLENE
               CONVERTING 'abcdefghijklmnopqrstuvwxyz'
               TO 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
               BEFORE INITIAL ','
           DISPLAY DARLENE
           DISPLAY ' '
      *-----------------------------------------------------------------
           DISPLAY 'CAESAR ROT 3'
           INSPECT SAMPLE-TEXT
               CONVERTING 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
               TO         'DEFGHIJKLMNOPQRSTUVWXYZABC'
           DISPLAY 'ENCODED...: ' SAMPLE-TEXT
           DISPLAY ' '
           INSPECT SAMPLE-TEXT
               CONVERTING 'DEFGHIJKLMNOPQRSTUVWXYZABC'
               TO         'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
           DISPLAY 'DECODED...: ' SAMPLE-TEXT
           DISPLAY ' '
      *-----------------------------------------------------------------
           DISPLAY 'FORMATTING DATE'
           ACCEPT CURRENT-DATE FROM DATE
           DISPLAY 'CURRENT-DATE........: ' CURRENT-DATE
           DISPLAY 'WS-FORMATTED-DATE...: ' WS-FORMATTED-DATE
           DISPLAY ' '
           INSPECT WS-FORMATTED-DATE
               CONVERTING 'YyMmDd'
               TO         CURRENT-DATE
           DISPLAY 'WS-FORMATTED-DATE...: ' WS-FORMATTED-DATE
           DISPLAY ' '
      *-----------------------------------------------------------------
           DISPLAY 'REFERENCE MODIFICATION'
           DISPLAY 'YEAR...: ' WS-FORMATTED-DATE(7:)
           DISPLAY 'MONTH..: ' WS-FORMATTED-DATE(1:2)
           DISPLAY 'DAY....: ' WS-FORMATTED-DATE(4:2)
           DISPLAY ' '
      *-----------------------------------------------------------------
           GOBACK
           .
      *-----------------------------------------------------------------
       FILL-VARIABLES.
           MOVE 12345 TO WS-NUM1
           MOVE  9876 TO WS-NUM2
           MOVE WS-NUM2 TO WS-NUMED1
           MOVE 'ALPHABETA' TO WS-ALNUM1
           MOVE 'GAMMADELTA' TO WS-ALNUM2
           MOVE WS-ALNUM1 TO WS-ALNUMED1
           .
       SHOW-PHONE-NUMBER.
           DISPLAY 'PHONE-NUMBER....: ' PHONE-NUMBER
           DISPLAY 'FORMATTED-NUM...: ' FORMATTED-NUM
           DISPLAY 'FORMATTED-ALT...: ' FORMATTED-ALT
           DISPLAY ' '
           .
       SHOW-COUNTER.
           DISPLAY 'WS-COUNTER.: ' WS-COUNTER
           DISPLAY 'WS-DATE....: ' WS-DATE
           DISPLAY ' '
           .
       SHOW-VARIABLES.
           DISPLAY 'WS-NUM1....: ' WS-NUM1
           DISPLAY 'WS-NUM2....: ' WS-NUM2
           DISPLAY 'WS-NUMED1..: ' WS-NUMED1
           DISPLAY 'WS-ALNUM1..: ' WS-ALNUM1
           DISPLAY 'WS-ALNUM2..: ' WS-ALNUM2
           DISPLAY 'WS-ALNUMED1: ' WS-ALNUMED1
           DISPLAY ' '
           .
