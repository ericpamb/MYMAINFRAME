       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCT01.
      *<CR_TAG_DEMO_BEGIN>
      *
      *  Copyright (C) Micro Focus 2012.
      *  All rights reserved.
      *
      *  This sample code is supplied for demonstration purposes only
      *  on an "as is" basis and "is for use at your own risk".
      *
      *<CR_TAG_DEMO_END>
      *================================================================*
      *                                                                *
      *        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    *
      *        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    *
      *                                                                *
      *             WHEN MOVING UP AND DOWN FROM ES TO                 *
      *             MFE OR MAINFRAME                                   *
      *                                                                *
      *                                                                *
      *             REPLACE ALL X'0D25 BY X'0D0A'                      *
      *             CHANGE LRECL OF ACCTIX                             *
      *                                                                *
      *                                                                *
      *        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    *
      *        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    *
      *                                                                *
      *                                                                *
      *                                                                *
      *================================================================*

      *  THIS PROGRAM IS THE FIRST INVOKED BY THE 'AC01'
      *  TRANSACTION. IT ANALYZES ALL REQUESTS, AND COMPLETES
      *  THOSE FOR NAME INQUIRIES AND RECORD DISPLAYS.  FOR
      *  UPDATE TRANSACTIONS, IT SENDS THE APPROPRIATE DATA ENTRY
      *  SCREEN AND SETS THE NEXT TRANSACTION IDENTIFIER TO
      *  'AC02', WHICH COMPLETES THE UPDATE OPERATION. FOR PRINT
      *  REQUESTS, IT STARTS TRANSACTION 'AC03' TO DO THE ACTUAL
      *  PRINTING.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  MISC.
           02  MSG-NO                 PIC S9(4) COMP VALUE +0.
      *    02  ACCT-LNG               PIC S9(4) COMP VALUE +63.
           02  ACCT-LNG               PIC S9(4) COMP VALUE +383.
           02  DTL-LNG                PIC S9(4) COMP VALUE +751.
           02  STARS                  PIC X(12) VALUE '************'.
           02  USE-QID.
               04  USE-QID1           PIC X(3) VALUE 'AC0'.
               04  USE-QID2           PIC X(5).
           02  USE-REC.
               04  USE-TERM           PIC X(4) VALUE SPACES.
               04  USE-TIME           PIC S9(7) COMP-3.
               04  USE-DATE           PIC S9(7) COMP-3.
           02  USE-LIMIT              PIC S9(7) COMP-3 VALUE +1000.
           02  USE-ITEM               PIC S9(4) COMP VALUE +1.
           02  USE-LNG                PIC S9(4) COMP VALUE +12.
           02  IN-AREA.
               04  IN-TYPE            PIC X VALUE 'R'.
               04  IN-REQ.
                   06  REQC           PIC X VALUE SPACES.
                   06  ACCTC          PIC X(5) VALUE SPACES.
                   06  PRTRC          PIC X(4) VALUE SPACES.
               04  IN-NAMES.
                   06  SNAMEC         PIC X(18) VALUE SPACES.
                   06  FNAMEC         PIC X(12) VALUE SPACES.
           02  COMMAREA-FOR-ACCT04.
               04  ERR-PGRMID         PIC X(8) VALUE 'ACCT01'.
               04  ERR-FN             PIC X.
               04  ERR-RCODE          PIC X.
           02  LINE-CNT               PIC S9(4) COMP VALUE +0.
           02  MAX-LINES              PIC S9(4) COMP VALUE +6.
           02  IX                     PIC S9(4) COMP.
           02  SRCH-CTRL.
               04  FILLER             PIC X VALUE 'S'.
               04  BRKEY.
                   06  BRKEY-SNAME    PIC X(12).
               04  BRKEY-ACCT         PIC X(5).
               04  MAX-SNAME          PIC X(12).
               04  MAX-FNAME          PIC X(7).
               04  MIN-FNAME          PIC X(7).
           02  SUM-LINE.
               04  ACCTDO             PIC X(5).
               04  FILLER             PIC X(3) VALUE SPACES.
               04  SNAMEDO            PIC X(12).
               04  FILLER             PIC X(2) VALUE SPACES.
               04  FNAMEDO            PIC X(7).
               04  FILLER             PIC X(2) VALUE SPACES.
               04  MIDO               PIC X(1).
               04  FILLER             PIC X(2) VALUE SPACES.
               04  TTLDO              PIC X(4).
               04  FILLER             PIC X(2) VALUE SPACES.
               04  ADDR1DO            PIC X(24).
               04  FILLER             PIC X(2) VALUE SPACES.
               04  STATDO             PIC X(2).
               04  FILLER             PIC X(3) VALUE SPACES.
               04  LIMITDO            PIC X(8).
           02  PAY-LINE.
               04  BAL                PIC X(8).
               04  FILLER             PIC X(6) VALUE SPACES.
               04  BMO                PIC 9(2).
               04  FILLER             PIC X VALUE '/'.
               04  BDAY               PIC 9(2).
               04  FILLER             PIC X VALUE '/'.
               04  BYR                PIC 9(2).
               04  FILLER             PIC X(4) VALUE SPACES.
               04  BAMT               PIC X(8).
               04  FILLER             PIC X(7) VALUE SPACES.
               04  PMO                PIC 9(2).
               04  FILLER             PIC X VALUE '/'.
               04  PDAY               PIC 9(2).
               04  FILLER             PIC X VALUE '/'.
               04  PYR                PIC 9(2).
               04  FILLER             PIC X(4) VALUE SPACES.
               04  PAMT               PIC X(8).
           COPY DFHBMSCA.
           COPY DFHAID.
       01  ACCTREC.
           COPY ACCTREC.
           COPY ACCTSET.
       01  MSG-LIST.
           02  FILLER                PIC X(70) VALUE
               'NAMES MUST BE ALPHABETIC, AND SURNAME IS REQUIRED.'.
           02  FILLER                PIC X(70) VALUE
               'ENTER SOME INPUT AND USE ONLY "CLEAR" OR "ENTER".'.
           02  FILLER                PIC X(70) VALUE
           'REQUEST TYPE REQUIRED; MUST BE "D", "P", "A", "M" OR "X".'.
           02  FILLER                PIC X(70) VALUE
               'PRINTER NAME REQUIRED ON PRINT REQUESTS'.
           02  FILLER                PIC X(70) VALUE
               'ACCOUNT NUMBER REQUIRED (BETWEEN 10000 AND 79999)'.
           02  FILLER                PIC X(70) VALUE
               'ACCOUNT NO. MUST BE NUMERIC AND FROM 10000 TO 79999'.
           02  FILLER                PIC X(70) VALUE
               'NO NAMES ON LOCAL FILE MATCHING YOUR REQUEST, HOST LINK
      -    'NOT AVAILABLE'.
           02  FILLER                PIC X(70) VALUE
               'ENTER EITHER NAME OR A REQUEST TYPE AND ACCOUNT NUMBER'.
           02  FILLER                PIC X(70) VALUE
               'THIS ACCOUNT NUMBER ALREADY EXISTS'.
           02  FILLER                PIC X(70) VALUE
               'NO LOCAL RECORD OF THIS ACCOUNT NUMBER, HOST LINK NOT AV
      -    'AILABLE'.
           02  FILLER                PIC X(47) VALUE
               'THIS ACCOUNT NUMBER ALREADY IN USE AT TERMINAL '.
           02  MSG-TERM              PIC X(23).
           02  FILLER                PIC X(70) VALUE
               'PRINT REQUEST SCHEDULED'.
           02  FILLER                PIC X(70) VALUE
               'PRINTER NAME NOT RECOGNIZED'.
           02  FILLER                PIC X(70) VALUE
           'INPUT ERROR; PLEASE RETRY; USE ONLY "CLEAR" OR "ENTER" KEY'.
           02  FILLER                PIC X(70) VALUE
               'THERE ARE MORE MATCHING NAMES. PRESS PA2 TO CONTINUE.'.
       01  FILLER REDEFINES MSG-LIST.
           02  MSG-TEXT              PIC X(70) OCCURS 15.

       01 WS-TSNAME.
          05 WS-TS-PREFIX    PIC X(03) VALUE 'WEB'.
          05 WS-TS-TASKN     PIC 9(05).
       01 WS-TEMP                          PIC X(18).
       01 WS-TEMP-LEN                      PIC S9(04) COMP.
       01 WS-HOST                          PIC X(116).
       01 WS-HOST-LEN                      PIC 9(8) COMP.
       01 WS-CHAR-2-TRANSF                 PIC X.
       01 WS-INIT                          PIC XX.
          88 WS-INIT-UNKNOWN               VALUE '??'.
          88 WS-INIT-PLTPI                 VALUE 'U'.
          88 WS-INIT-TERMINAL              VALUE 'TD'.
          88 WS-START-TRAN                 VALUE 'SD'.
       01 WS-TS-LEN                PIC S9(08) COMP VALUE 534.
       01 WS-TS-LINES.
          05 WS-TS-FLAG          PIC X.
          05 WS-TS-INDICE        PIC 99.
          05 WS-TS-LINE OCCURS 6 PIC X(383).

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           02  SRCH-COMM             PIC X(44).
           02  IN-COMMG REDEFINES SRCH-COMM .
               05 IN-COMM            PIC X(41).
               05 FILLER             PIC X(03).
           02  CTYPEG REDEFINES SRCH-COMM.
               05 CTYPE              PIC X.
               05 FILLER             PIC X(43).
           02  FILLER                PIC X(08).
       PROCEDURE DIVISION.
      * Let's check if we are coming from a terminal?
           EXEC CICS ASSIGN
                     STARTCODE (WS-INIT)
           END-EXEC
           EXEC CICS HANDLE CONDITION MAPFAIL(NO-MAP)
               NOTFND(SRCH-ANY)
               ENDFILE(SRCH-DONE)
               QIDERR(RSRV-1)
               TERMIDERR(TERMID-ERR)
               ERROR(OTHER-ERRORS) END-EXEC.
           EXEC CICS IGNORE CONDITION DUPKEY END-EXEC.

           IF WS-INIT NOT = 'TD'
      * Not from a terminal, are we coming from the web?
              MOVE LENGTH OF WS-HOST   TO WS-HOST-LEN
              EXEC CICS WEB EXTRACT
                   HOST(WS-HOST)
                   HOSTLENGTH(WS-HOST-LEN)
                   NOHANDLE
              END-EXEC
              IF EIBRESP = DFHRESP(NORMAL)
      * from the web
                 MOVE EIBTASKN         TO WS-TS-TASKN
                 MOVE SPACE            TO WS-TS-FLAG
                 MOVE 0                TO WS-TS-INDICE
                 IF EIBCALEN > 0 AND CTYPE = 'S',
                    move DFHCOMMAREA(45:8)         TO WS-TSNAME
                    MOVE SRCH-COMM TO SRCH-CTRL GO TO SRCH-RESUME
                 END-IF
              END-IF
           END-IF
           .
           MOVE LOW-VALUES TO ACCTMNUI, ACCTDTLI.
           IF EIBAID = DFHCLEAR
               IF EIBCALEN = 0,
                   EXEC CICS SEND CONTROL FREEKB END-EXEC
                   EXEC CICS RETURN END-EXEC
               ELSE GO TO NEW-MENU.
           IF EIBAID = DFHPA2 AND EIBCALEN > 0 AND CTYPE = 'S',
               MOVE SRCH-COMM TO SRCH-CTRL, GO TO SRCH-RESUME.
           IF EIBCALEN > 0 AND CTYPE = 'R', MOVE IN-COMM TO IN-AREA.
      *    EXEC CICS RECEIVE MAP('ACCTMNU') MAPSET('ACCTSET') END-EXEC.
           IF WS-INIT = 'TD'
           EXEC CICS RECEIVE
                     MAP('ACCTMNU')
                     MAPSET('ACCTSET') END-EXEC
           ELSE
              MOVE DFHCOMMAREA(1:12) TO SNAMEC
              GO TO SRCH-INIT
           .
           IF REQML > 0 MOVE REQMI TO REQC.
           IF REQMF NOT = LOW-VALUE, MOVE SPACE TO REQC.
           IF ACCTML > 0 MOVE ACCTMI TO ACCTC.
           IF ACCTMF NOT = LOW-VALUE, MOVE SPACES TO ACCTC.
           IF PRTRML > 0 MOVE PRTRMI TO PRTRC.
           IF PRTRMF NOT = LOW-VALUE, MOVE SPACES TO PRTRC.
           IF SNAMEML > 0 MOVE SNAMEMI TO SNAMEC.
           IF SNAMEMF NOT = LOW-VALUE, MOVE SPACES TO SNAMEC.
           IF FNAMEML > 0 MOVE FNAMEMI TO FNAMEC.
           IF FNAMEMF NOT = LOW-VALUE, MOVE SPACES TO FNAMEC.
           MOVE LOW-VALUES TO ACCTMNUI.
           IF IN-NAMES = SPACES GO TO CK-ANY.
           IF FNAMEC NOT ALPHABETIC, MOVE 1 TO MSG-NO,
               MOVE -1 TO FNAMEML, MOVE DFHBMBRY TO FNAMEMA.
           IF SNAMEC = SPACES, MOVE STARS TO SNAMEMO,
           ELSE IF SNAMEC ALPHABETIC, GO TO CK-NAME.
           MOVE 1 TO MSG-NO.
           MOVE -1 TO SNAMEML, MOVE DFHBMBRY TO SNAMEMA.
       CK-NAME.
           IF MSG-NO > 0 GO TO MENU-RESEND.
       SRCH-INIT.
           MOVE SNAMEC TO BRKEY-SNAME, MAX-SNAME.
           MOVE LOW-VALUES TO BRKEY-ACCT.
      *    TRANSFORM MAX-SNAME FROM SPACES TO HIGH-VALUES.
           MOVE MAX-SNAME                  TO WS-TEMP.
           MOVE HIGH-VALUE                 TO WS-CHAR-2-TRANSF
           PERFORM DO-TRANSFORM.
           MOVE WS-TEMP                    TO MAX-SNAME
           MOVE FNAMEC TO MIN-FNAME, MAX-FNAME.
      *    DO-TRANSFORM MIN-FNAME FROM SPACES TO LOW-VALUES.
           MOVE MIN-FNAME                  TO WS-TEMP.
           MOVE LOW-VALUE                  TO WS-CHAR-2-TRANSF
           PERFORM DO-TRANSFORM.
           MOVE WS-TEMP                    TO MIN-FNAME
      *    DO-TRANSFORM MAX-FNAME FROM SPACES TO HIGH-VALUES.
           MOVE MAX-FNAME                  TO WS-TEMP.
           MOVE HIGH-VALUE                 TO WS-CHAR-2-TRANSF
           PERFORM DO-TRANSFORM.
           MOVE WS-TEMP                    TO MAX-FNAME.
       SRCH-RESUME.
           EXEC CICS STARTBR DATASET('ACCTIX') RIDFLD(BRKEY) GTEQ
               END-EXEC.
       SRCH-LOOP.
           EXEC CICS READNEXT DATASET('ACCTIX') INTO(ACCTREC)
               LENGTH(ACCT-LNG) RIDFLD(BRKEY) END-EXEC.
           IF SNAMEDO IN ACCTREC > MAX-SNAME GO TO SRCH-DONE.
           IF FNAMEDO IN ACCTREC < MIN-FNAME OR
               FNAMEDO IN ACCTREC > MAX-FNAME, GO TO SRCH-LOOP.
           ADD 1 TO LINE-CNT.
           IF LINE-CNT > MAX-LINES,
               MOVE MSG-TEXT (15) TO MSGMO,
               MOVE DFHBMBRY TO MSGMA, GO TO SRCH-DONE.
           MOVE CORRESPONDING ACCTREC TO SUM-LINE.
           MOVE SUM-LINE TO SUMLNMO (LINE-CNT).
           IF WS-INIT NOT = 'TD'
              ADD 1                   TO WS-TS-INDICE
              MOVE ACCTREC            TO WS-TS-LINE(WS-TS-INDICE).
           GO TO SRCH-LOOP.
       SRCH-DONE.
           EXEC CICS ENDBR DATASET('ACCTIX') END-EXEC.
       SRCH-ANY.
           IF LINE-CNT = 0, MOVE 7 TO MSG-NO,
               MOVE -1 TO SNAMEML, GO TO MENU-RESEND.
           MOVE DFHBMUNP TO SUMLNMA (1), SUMLNMA (2), SUMLNMA (3),
               SUMLNMA (4), SUMLNMA (5), SUMLNMA (6).
           MOVE DFHBMBRY TO MSGMA, MOVE DFHBMASB TO SUMTTLMA.
      *
           IF WS-INIT = 'TD'
           EXEC CICS SEND MAP('ACCTMNU') MAPSET('ACCTSET')
               FREEKB DATAONLY ERASEAUP END-EXEC
           IF LINE-CNT NOT > MAX-LINES,
               EXEC CICS RETURN TRANSID('AC01') END-EXEC
           ELSE
                EXEC CICS RETURN TRANSID('AC01') COMMAREA(SRCH-CTRL)
                   LENGTH(44) END-EXEC.

       WRITE-TS.
              EXEC CICS WRITEQ TS
                  QUEUE(WS-TSNAME)
                  FROM(WS-TS-LINES)
                  LENGTH(LENGTH OF WS-TS-LINES)
                  NOHANDLE
              END-EXEC
              IF LINE-CNT > MAX-LINES
                 MOVE SRCH-CTRL           TO DFHCOMMAREA
              ELSE
                 MOVE SPACE               TO DFHCOMMAREA
              END-IF
              EXEC CICS RETURN END-EXEC
           .

       CK-ANY.
           IF IN-REQ = SPACES, MOVE -1 TO SNAMEML,
               MOVE 8 TO MSG-NO, GO TO MENU-RESEND.
       CK-ACCTNO-1.
           IF ACCTC = SPACES, MOVE STARS TO ACCTMO,
               MOVE 5 TO MSG-NO, GO TO ACCT-ERR.
           IF (ACCTC < '10000' OR ACCTC > '79999' OR ACCTC NOT NUMERIC),
               MOVE 6 TO MSG-NO, GO TO ACCT-ERR.
       CK-ACCTNO-2.
           EXEC CICS HANDLE CONDITION NOTFND(NO-ACCT-RECORD) END-EXEC.
           EXEC CICS READ DATASET('ACCTFIL') RIDFLD(ACCTC)
               INTO(ACCTREC) LENGTH(ACCT-LNG) END-EXEC.
           IF REQC = 'A',
               MOVE 9 TO MSG-NO, GO TO ACCT-ERR,
           ELSE GO TO CK-REQ.
       NO-ACCT-RECORD.
           IF REQC = 'A', GO TO CK-REQ.
           MOVE 10 TO MSG-NO.
       ACCT-ERR.
           MOVE -1 TO ACCTML, MOVE DFHBMBRY TO ACCTMA.
       CK-REQ.
           IF REQC =  'D' OR 'P' OR 'A' OR 'M' OR 'X',
               IF MSG-NO = 0 GO TO CK-USE, ELSE GO TO MENU-RESEND.
           IF REQC = SPACE, MOVE STARS TO REQMO.
           MOVE -1 TO REQML, MOVE DFHBMBRY TO REQMA,
           MOVE 3 TO MSG-NO.
           GO TO MENU-RESEND.
       CK-USE.
           IF REQC = 'P' OR 'D' GO TO BUILD-MAP.
           MOVE ACCTC TO USE-QID2.
           EXEC CICS READQ TS QUEUE(USE-QID) INTO(USE-REC)
               ITEM(USE-ITEM) LENGTH(USE-LNG) END-EXEC.
           ADD USE-LIMIT TO USE-TIME.
           IF USE-TIME > 236000, ADD 1 TO USE-DATE,
               SUBTRACT 236000 FROM USE-TIME.
           IF USE-DATE > EIBDATE OR
               (USE-DATE = EIBDATE AND USE-TIME NOT < EIBTIME)
               MOVE USE-TERM TO MSG-TERM, MOVE 11 TO MSG-NO,
               MOVE -1 TO ACCTML, MOVE DFHBMBRY TO ACCTMA,
               GO TO MENU-RESEND.
       RSRV.
           MOVE EIBTRMID TO USE-TERM, MOVE EIBTIME TO USE-TIME.
           MOVE EIBDATE TO USE-DATE.
           EXEC CICS WRITEQ TS QUEUE(USE-QID) FROM(USE-REC)
               LENGTH(12) ITEM(USE-ITEM) REWRITE END-EXEC.
           GO TO BUILD-MAP.
       RSRV-1.
           MOVE EIBTRMID TO USE-TERM, MOVE EIBTIME TO USE-TIME.
           MOVE EIBDATE TO USE-DATE.
           EXEC CICS WRITEQ TS QUEUE(USE-QID) FROM(USE-REC)
               LENGTH(12) END-EXEC.
       BUILD-MAP.
           IF REQC = 'X' MOVE 'DELETION' TO TITLEDO,
               MOVE -1 TO VFYDL, MOVE DFHBMUNP TO VFYDA,
               MOVE 'ENTER "Y" TO CONFIRM OR "CLEAR" TO CANCEL'
                   TO MSGDO,
           ELSE MOVE -1 TO SNAMEDL.
           IF REQC = 'A' MOVE 'NEW RECORD' TO TITLEDO,
               MOVE DFHPROTN TO STATTLDA, LIMTTLDA, HISTTLDA,
               MOVE ACCTC TO ACCTDI,
               MOVE 'FILL IN AND PRESS "ENTER," OR "CLEAR" TO CANCEL'
                   TO MSGDO,
               GO TO SEND-DETAIL.
           IF REQC = 'M' MOVE 'RECORD CHANGE' TO TITLEDO,
               MOVE 'MAKE CHANGES AND "ENTER" OR "CLEAR" TO CANCEL'
                   TO MSGDO,
           ELSE IF REQC = 'D',
                   MOVE 'PRESS "CLEAR" OR "ENTER" WHEN FINISHED'
                       TO MSGDO.
           MOVE CORRESPONDING ACCTREC TO ACCTDTLO.
           MOVE CORRESPONDING PAY-HIST (1) TO PAY-LINE.
           MOVE PAY-LINE TO HIST1DO.
           MOVE CORRESPONDING PAY-HIST (2) TO PAY-LINE.
           MOVE PAY-LINE TO HIST2DO.
           MOVE CORRESPONDING PAY-HIST (3) TO PAY-LINE.
           MOVE PAY-LINE TO HIST3DO.
           IF REQC  = 'M' GO TO SEND-DETAIL,
           ELSE IF REQC = 'P' GO TO PRINT-PROC.
           MOVE DFHBMASK TO
               SNAMEDA, FNAMEDA, MIDA, TTLDA, TELDA, ADDR1DA,
               ADDR2DA, ADDR3DA, AUTH1DA, AUTH2DA, AUTH3DA,
               AUTH4DA, CARDSDA, IMODA, IDAYDA, IYRDA, RSNDA,
               CCODEDA, APPRDA, SCODE1DA, SCODE2DA, SCODE3DA.
       SEND-DETAIL.
           EXEC CICS SEND MAP('ACCTDTL') MAPSET('ACCTSET') ERASE FREEKB
               CURSOR END-EXEC.
           IF REQC = 'D', EXEC CICS RETURN TRANSID('ACCT') END-EXEC,
           ELSE
                EXEC CICS RETURN TRANSID('AC02')
                   COMMAREA(IN-REQ) LENGTH(6) END-EXEC.
       PRINT-PROC.
           IF PRTRC = SPACES, MOVE STARS TO PRTRMO
               MOVE 4 TO MSG-NO, GO TO TERMID-ERR1.
           EXEC CICS START TRANSID('AC03') FROM(ACCTDTLO)
               LENGTH(DTL-LNG) TERMID(PRTRC) END-EXEC.
           MOVE MSG-TEXT (12) TO MSGMO.
           EXEC CICS SEND MAP('ACCTMNU') MAPSET ('ACCTSET') DATAONLY
                ERASEAUP FREEKB END-EXEC.
           EXEC CICS RETURN TRANSID('AC01') END-EXEC.
       TERMID-ERR.
           MOVE 13 TO MSG-NO.
       TERMID-ERR1.
           MOVE -1 TO PRTRML, MOVE DFHBMBRY TO PRTRMA.
       MENU-RESEND.
           IF WS-INIT = 'TD'
              MOVE MSG-TEXT (MSG-NO) TO MSGMO
              EXEC CICS SEND MAP('ACCTMNU') MAPSET('ACCTSET')
                  CURSOR FRSET FREEKB END-EXEC
           EXEC CICS RETURN TRANSID('AC01') COMMAREA(IN-AREA)
                   LENGTH(41) END-EXEC
           ELSE
               EXEC CICS RETURN END-EXEC
           END-IF
           .

       NO-MAP.
           IF (EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3 OR DFHENTER)
               MOVE 2 TO MSG-NO, MOVE -1 TO SNAMEML, GO TO MENU-RESEND.
           MOVE MSG-TEXT (14) TO MSGMO.
       NEW-MENU.
           EXEC CICS SEND MAP('ACCTMNU') MAPSET('ACCTSET')
               FREEKB ERASE END-EXEC.
           EXEC CICS RETURN TRANSID ('AC01') END-EXEC.
       OTHER-ERRORS.
           IF WS-INIT = 'TD'
           MOVE EIBFN TO ERR-FN, MOVE EIBRCODE TO ERR-RCODE
           EXEC CICS HANDLE CONDITION ERROR END-EXEC
           EXEC CICS LINK PROGRAM('ACCT04')
               COMMAREA(COMMAREA-FOR-ACCT04) LENGTH(10) END-EXEC
           GOBACK
           ELSE
              MOVE 'E'                 TO WS-TS-FLAG
              MOVE 1                   TO WS-TS-INDICE
              STRING 'ACCT01 ERROR, FUNCTION: ' DELIMITED BY SIZE
                     ERR-FN                     DELIMITED BY SIZE
                     ', RETURN CODE: '          DELIMITED BY SIZE
                     ERR-RCODE                  DELIMITED BY SIZE
                 INTO WS-TS-LINE(WS-TS-INDICE)
              GO TO WRITE-TS
           END-IF
           .
       DO-TRANSFORM.
           MOVE LENGTH OF WS-TEMP TO WS-TEMP-LEN WS-TS-INDICE
           PERFORM UNTIL WS-TS-INDICE = 0
              IF WS-TEMP(WS-TS-INDICE:1) = SPACE
                 MOVE WS-CHAR-2-TRANSF  TO WS-TEMP(WS-TS-INDICE:1)
              END-IF
              ADD -1              TO WS-TS-INDICE
           END-PERFORM
           .
