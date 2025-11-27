       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIXDB2U.
       AUTHOR. YOU.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-RETRY-LIMIT          PIC 9     VALUE 3.
       01  WS-RETRY-COUNT          PIC 9     VALUE 0.
       01  WS-RESP                 PIC S9(9) COMP VALUE 0.
       01  WS-RESP2                PIC S9(9) COMP VALUE 0.

      *  Host variables for SQL
       EXEC SQL INCLUDE SQLCA END-EXEC.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  HV-ACCT-ID              PIC X(10).
       01  HV-AMOUNT               PIC S9(9)V99 COMP-3.
       EXEC SQL END DECLARE SECTION END-EXEC.

      *  Example COMMAREA layout (optional)
       01  DFHCOMMAREA.
           05  CA-ACCT-ID          PIC X(10).
           05  CA-AMOUNT           PIC S9(9)V99 COMP-3.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
      *      If passed a COMMAREA, take inputs from it; else use demo defaults
           IF EIBCALEN > 0
              MOVE CA-ACCT-ID TO HV-ACCT-ID
              MOVE CA-AMOUNT  TO HV-AMOUNT
           ELSE
              MOVE 'ACC0000123' TO HV-ACCT-ID
              MOVE +100.00      TO HV-AMOUNT
           END-IF

      *      Ensure DB2 work is controlled by CICS (no SQL COMMIT/ROLLBACK here!)

           PERFORM WITH TEST BEFORE UNTIL
               (SQLCODE = 0) OR
               (WS-RETRY-COUNT >= WS-RETRY-LIMIT)

              ADD 1 TO WS-RETRY-COUNT

      *         Do the business update
              EXEC SQL
                 UPDATE BANK.ACCOUNT
                    SET BALANCE = BALANCE + :HV-AMOUNT
                  WHERE ACCT_ID = :HV-ACCT-ID
              END-EXEC

              EVALUATE TRUE
                 WHEN SQLCODE = 0
      *               Success: commit the CICS unit of work → commits DB2 too
                    EXEC CICS SYNCPOINT
                         RESP(WS-RESP) RESP2(WS-RESP2)
                    END-EXEC

                 WHEN SQLCODE = -911 OR SQLCODE = -913
      *               Deadlock or timeout → backout and optionally retry
                    EXEC CICS SYNCPOINT ROLLBACK
                         RESP(WS-RESP) RESP2(WS-RESP2)
                    END-EXEC
      *               Small, polite backoff (CICS “wait”)
                    EXEC CICS DELAY FOR SECONDS(1) END-EXEC

                 WHEN OTHER
      *               Any other SQL error → backout and stop
                    EXEC CICS SYNCPOINT ROLLBACK
                         RESP(WS-RESP) RESP2(WS-RESP2)
                    END-EXEC
                    PERFORM SEND-ERROR
                    EXEC CICS RETURN END-EXEC
              END-EVALUATE

           END-PERFORM

           IF SQLCODE = 0
              PERFORM SEND-OK
           ELSE
              PERFORM SEND-ERROR
           END-IF

           EXEC CICS RETURN END-EXEC.
       *
        SEND-OK.
           DISPLAY 'CIXDB2U: UPDATE COMMITTED. ACCT=' HV-ACCT-ID
                   ' AMT=' HV-AMOUNT
                   ' TRIES=' WS-RETRY-COUNT.
           EXIT.
       *
        SEND-ERROR.
           DISPLAY 'CIXDB2U: ERROR. SQLCODE=' SQLCODE
                   ' SQLSTATE=' SQLSTATE
                   ' RESP=' WS-RESP
                   ' RESP2=' WS-RESP2
                   ' AFTER TRIES=' WS-RETRY-COUNT.
           EXIT.

