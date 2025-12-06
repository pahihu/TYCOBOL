000000*AA1ABBB+----2----+----3----+----4----+----5----+----6----+----7--SEQUENCE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GNUMAIN.
       AUTHOR. Andras Pahi.
       DATE-WRITTEN. 2025-12-06.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 I            PIC 9(2) VALUE ZEROS.
       COPY ARGS.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
           ACCEPT ARGC FROM ARGUMENT-NUMBER
           IF ARGC < 1
               DISPLAY 'USAGE: GNUMAIN MODULE [ARG1...]'
               MOVE 1 TO RETURN-CODE
               GOBACK
           END-IF
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARGC
               DISPLAY     I  UPON ARGUMENT-NUMBER
               IF I = 1
                   ACCEPT      MODULE FROM ARGUMENT-VALUE
               ELSE
                   ACCEPT ARGV(I - 1) FROM ARGUMENT-VALUE
               END-IF
           END-PERFORM
           INSPECT MODULE
               CONVERTING 'abcdefghijklmnopqrstuvxyz'
               TO         'ABCDEFGHIJKLMNOPQRSTUVXYZ'
           SUBTRACT 1 FROM ARGC
           CALL MODULE USING ARGUMENTS
      *    PERFORM SHOW-ARGUMENTS
           GOBACK
           .
      *-----------------------------------------------------------------
       SHOW-ARGUMENTS.
           DISPLAY 'MODULE=' MODULE
           DISPLAY 'ARGC=' ARGC
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARGC
               DISPLAY 'ARGV(' I ')=' ARGV(I)
           END-PERFORM
           .
    
