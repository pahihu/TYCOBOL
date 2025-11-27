       identification division.
       program-id.             comp5x.
       environment division.
       configuration section.
       source-computer.
           System76
      *           with debugging mode
           .
       repository.
           function all intrinsic.

       data division.
       working-storage section.
       01 cntx1                                    comp-5 pic x(01).
       01 cntx19 redefines cntx1                   comp-5 pic 9(01).
       01 cntx2                                    comp-5 pic x(02).
       01 cntx29 redefines cntx2                   comp-5 pic 9(02).
       01 cntx3                                    comp-5 pic x(03).
       01 cntx39 redefines cntx3                   comp-5 pic 9(03).
       01 cntx4                                    comp-5 pic x(04).
       01 cntx49 redefines cntx4                   comp-5 pic 9(04).
       01 cntx5                                    comp-5 pic x(05).
       01 cntx59 redefines cntx5                   comp-5 pic 9(05).
       procedure division.
           move 200 to cntx1
           move 276 to cntx2
           move 276 to cntx3
           move 276 to cntx4
           move 576 to cntx5
           display 'cntx1  = ' cntx1
           display 'cntx19 = ' cntx19
           display 'cntx2  = ' cntx2
           display 'cntx29 = ' cntx29
           display 'cntx3  = ' cntx3
           display 'cntx39 = ' cntx39
           display 'cntx4  = ' cntx4
           display 'cntx49 = ' cntx49
           display 'cntx5  = ' cntx5
           display 'cntx59 = ' cntx59
           call 'cobfdump' using cntx1
           call 'cobfdump' using cntx19
           call 'cobfdump' using cntx2
           call 'cobfdump' using cntx29
           call 'cobfdump' using cntx3
           call 'cobfdump' using cntx39
           call 'cobfdump' using cntx4
           call 'cobfdump' using cntx49
           call 'cobfdump' using cntx5
           call 'cobfdump' using cntx59
           move 0 to return-code
           goback
           .
