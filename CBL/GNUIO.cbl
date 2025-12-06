000000*AA1ABBB+----2----+----3----+----4----+----5----+----6----+----7--SEQUENCE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GNUIO.
       AUTHOR. Andras Pahi.
       DATE-WRITTEN. 2025-12-06.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-LEN    PIC 9(2) VALUE ZEROS.
       LINKAGE SECTION.
       COPY CALLIO.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION USING IO-PARAMS.
       HANDLE-OP.
           EVALUATE TRUE
               WHEN IOP-ACCEPT
                   INITIALIZE WS-LEN
                   INSPECT IO-P-STRING
                       TALLYING WS-LEN
                       FOR ALL CHARACTERS
                       BEFORE INITIAL '*'
                   SUBTRACT 1 FROM WS-LEN
                   DISPLAY IO-P-STRING (1:WS-LEN)
                   ACCEPT IO-P-CHAR
               WHEN IOP-GET-ARGC
                   ACCEPT IO-P-NUMBER FROM ARGUMENT-NUMBER
               WHEN IOP-GET-ARG
                   DISPLAY IO-P-NUMBER UPON ARGUMENT-NUMBER
                   ACCEPT  IO-P-STRING FROM ARGUMENT-VALUE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           GOBACK
           .
