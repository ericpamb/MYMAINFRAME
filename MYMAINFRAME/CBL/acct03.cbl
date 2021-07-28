       IDENTIFICATION DIVISION.                                         01000000
       PROGRAM-ID. ACCT03.                                              02000000
      *<CR_TAG_DEMO_BEGIN>
      *
      *  Copyright (C) Micro Focus 2012.
      *  All rights reserved.
      *
      *  This sample code is supplied for demonstration purposes only
      *  on an "as is" basis and "is for use at your own risk".
      *
      *<CR_TAG_DEMO_END>
      *REMARKS. THIS PROGRAM IS THE FIRST INVOKED BY TRANSACTIONS       03000000
      *         'AC03', 'ACLG' AND 'AC05'. 'AC03' COMPLETES A REQUEST   04000000
      *         FOR PRINTING OF A CUSTOMER RECORD, WHICH WAS PROCESSED  05000000
      *         INITIALLY BY TRANSACTION 'AC01'.  'ACLG,' WHICH IS A    06000000
      *         USER REQUEST TO PRINT THE LOG, MERELY REQUESTS 'AC05'   07000000
      *         BE STARTED WHEN THE LOG PRINTER ('L86O') IS AVAILABLE.  08000000
      *         'AC05' TRANSFERS THE LOG DATA FROM TEMPORARY STORAGE TO 09000000
      *         THE PRINTER.                                            10000000
       ENVIRONMENT DIVISION.                                            11000000
       DATA DIVISION.                                                   12000000
       WORKING-STORAGE SECTION.                                         13000000
       01  COMMAREA-FOR-ACCT04.                                         14000000
           02  ERR-PGM                 PIC X(8) VALUE 'ACCT03'.         15000000
           02  ERR-FN                  PIC X.                           16000000
           02  ERR-RCODE               PIC X.                           17000000
       01  TS-LNG                      PIC S9(4) COMP VALUE +751.       18000000
           COPY ACCTSET.                                                19000000
       PROCEDURE DIVISION.                                              21000000
       INIT.                                                            24000000
           EXEC CICS HANDLE CONDITION ITEMERR(LOG-END)                  25000000
               QIDERR(RTRN) ERROR(NO-GOOD) END-EXEC.                    26000000
           IF EIBTRNID = 'AC03' GO TO AC03.                             29000000
           IF EIBTRNID = 'ACLG' GO TO ACLG, ELSE GO TO AC05.            30000000
       AC03.                                                            33000000
           EXEC CICS RETRIEVE INTO(ACCTDTLI) LENGTH(TS-LNG) END-EXEC.   34000000
           EXEC CICS SEND MAP('ACCTDTL') MAPSET('ACCTSET') PRINT        35000000
               ERASE END-EXEC.                                          36000000
           GO TO RTRN.                                                  37000000
       ACLG.                                                            42000000
           EXEC CICS START TRANSID('AC05') TERMID('L86O') END-EXEC.     44000000
           MOVE LOW-VALUES TO ACCTMSGO.                                 46000000
           MOVE 'PRINTING OF LOG HAS BEEN SCHEDULED' TO MSGO.           48000000
           EXEC CICS SEND MAP('ACCTMSG') MAPSET('ACCTSET')              50000000
               FREEKB END-EXEC.                                         52000000
           GO TO RTRN.                                                  54000000
       AC05.                                                            60000000
           EXEC CICS READQ TS QUEUE('ACCTLOG') INTO (ACCTDTLI)          62000000
               LENGTH(TS-LNG) NEXT END-EXEC.                            64000000
           EXEC CICS SEND MAP('ACCTDTL') MAPSET('ACCTSET') PRINT ERASE  66000000
               END-EXEC.                                                68000000
           GO TO AC05.                                                  70000000
       LOG-END.                                                         72000000
           EXEC CICS DELETEQ TS QUEUE('ACCTLOG') END-EXEC.              74000000
       RTRN.                                                            80000000
           EXEC CICS RETURN END-EXEC.                                   82000000
       NO-GOOD.                                                         88000000
           MOVE EIBFN TO ERR-FN, MOVE EIBRCODE TO ERR-RCODE.            90000000
           EXEC CICS HANDLE CONDITION ERROR END-EXEC.                   92000000
           EXEC CICS LINK PROGRAM('ACCT04')                             94000000
               COMMAREA(COMMAREA-FOR-ACCT04) LENGTH(10) END-EXEC.       96000000
           GOBACK.                                                      98000000
