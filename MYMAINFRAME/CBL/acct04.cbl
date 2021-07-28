       IDENTIFICATION DIVISION.                                         00600000
       PROGRAM-ID. ACCT04.                                              01300000
      *<CR_TAG_DEMO_BEGIN>
      *
      *  Copyright (C) Micro Focus 2012.
      *  All rights reserved.
      *
      *  This sample code is supplied for demonstration purposes only
      *  on an "as is" basis and "is for use at your own risk".
      *
      *<CR_TAG_DEMO_END>
      *REMARKS. THIS PROGRAM IS A GENERAL PURPOSE ERROR ROUTINE.        02000000
      *         CONTROL IS TRANSFERRED TO IT BY OTHER PROGRAMS IN THE   02700000
      *         ONLINE ACCOUNT FILE APPLICATION WHEN AN UNRECOVERABLE   03400000
      *         ERROR HAS OCCURRED.                                     04100000
      *         IT SENDS A MESSAGE TO INPUT TERMINAL DESCRIBING THE     04800000
      *         TYPE OF ERROR AND ASKS THE OPERATOR TO REPORT IT.       05500000
      *         THEN IT ABENDS, SO THAT ANY UPDATES MADE IN THE         06200000
      *         UNCOMPLETED TRANSACTION ARE BACKED OUT AND SO THAT AN   06900000
      *         ABEND DUMP IS AVAILABLE.                                07600000
       ENVIRONMENT DIVISION.                                            08300000
       DATA DIVISION.                                                   09000000
       WORKING-STORAGE SECTION.                                         09700000
           COPY ACCTSET.                                                10400000
       01  MISC.                                                        11100000
           02  I                   PIC S9(4) COMP.                      11800000
           02  IX                  PIC S9(4) COMP VALUE +31.            12500000
           02  DSN-MSG.                                                 13200000
               04  FILLER          PIC X(13) VALUE 'THE FILE IS: '.     13900000
               04  DSN             PIC X(8).                            14600000
               04  FILLER          PIC X VALUE '.'.                     15300000
           02  HEX-LIST.                                                16000000
               04  HEX-0601        PIC S9(4) COMP VALUE +1537.          16700000
               04  HEX-0602        PIC S9(4) COMP VALUE +1538.          17400000
               04  HEX-0608        PIC S9(4) COMP VALUE +1544.          18100000
               04  HEX-060C        PIC S9(4) COMP VALUE +1548.          18800000
               04  HEX-060F        PIC S9(4) COMP VALUE +1551.          19500000
               04  HEX-0680        PIC S9(4) COMP VALUE +1664.          20200000
               04  HEX-0681        PIC S9(4) COMP VALUE +1665.          20900000
               04  HEX-0682        PIC S9(4) COMP VALUE +1666.          21600000
               04  HEX-0683        PIC S9(4) COMP VALUE +1667.          22300000
               04  HEX-06E1        PIC S9(4) COMP VALUE +1761.          23000000
               04  HEX-0A01        PIC S9(4) COMP VALUE +2561.          23700000
               04  HEX-0A02        PIC S9(4) COMP VALUE +2562.          24400000
               04  HEX-0A04        PIC S9(4) COMP VALUE +2564.          25100000
               04  HEX-0A08        PIC S9(4) COMP VALUE +2568.          25800000
               04  HEX-0A20        PIC S9(4) COMP VALUE +2592.          26500000
               04  HEX-0AE1        PIC S9(4) COMP VALUE +2785.          27200000
               04  HEX-0E01        PIC S9(4) COMP VALUE +3585.          27900000
               04  HEX-0EE1        PIC S9(4) COMP VALUE +3809.          28600000
               04  HEX-1001        PIC S9(4) COMP VALUE +4097.          29300000
               04  HEX-1004        PIC S9(4) COMP VALUE +4100.          30000000
               04  HEX-1011        PIC S9(4) COMP VALUE +4113.          30700000
               04  HEX-1012        PIC S9(4) COMP VALUE +4114.          31400000
               04  HEX-1014        PIC S9(4) COMP VALUE +4116.          32100000
               04  HEX-1081        PIC S9(4) COMP VALUE +4225.          32800000
               04  HEX-10E1        PIC S9(4) COMP VALUE +4321.          33500000
               04  HEX-10E9        PIC S9(4) COMP VALUE +4329.          34200000
               04  HEX-10FF        PIC S9(4) COMP VALUE +4351.          34900000
               04  HEX-1804        PIC S9(4) COMP VALUE +6148.          35600000
               04  HEX-1808        PIC S9(4) COMP VALUE +6152.          36300000
               04  HEX-18E1        PIC S9(4) COMP VALUE +6369.          37000000
               04  HEX-MISC        PIC S9(4) COMP VALUE +0001.          37700000
           02  HEX-CODE REDEFINES HEX-LIST PIC X(2) OCCURS 31.          38400000
           02  ERR-LIST.                                                39100000
               04  MSG-0601        PIC X(60) VALUE                      39800000
                   'A PROGRAM OR FCT TABLE ERROR (INVALID FILE NAME).'. 40500000
               04  MSG-0602        PIC X(60) VALUE                      41200000
                   'A PROGRAM OR FILE ERROR (VSAM ILLOGIC).'.           41900000
               04  MSG-0608        PIC X(60) VALUE                      42600000
               'A PROGRAM OR FCT TABLE ERROR (INVALID FILE REQUEST).'.  43300000
               04  MSG-060C        PIC X(60) VALUE                      44000000
                   'A FILE BEING CLOSED THAT MUST BE OPEN.'.            44700000
               04  MSG-060F        PIC X(60) VALUE                      45400000
                   'A PROGRAM OR FILE ERROR (UNEXPECTED END-OF-FILE).'. 46100000
               04  MSG-0680        PIC X(60) VALUE                      46800000
                   'A FILE INPUT/OUTPUT ERROR.'.                        47500000
               04  MSG-0681        PIC X(60) VALUE                      48200000
                   'A PROGRAM OR FILE ERROR (RECORD NOT FOUND).'.       48900000
               04  MSG-0682        PIC X(60) VALUE                      49600000
                   'A PROGRAM OR FILE ERROR (DUPLICATE RECORD).'.       50300000
               04  MSG-0683        PIC X(60) VALUE                      51000000
                   'INADEQUATE SPACE IN A FILE.'.                       51700000
               04  MSG-06E1        PIC X(60) VALUE                      52400000
               'A PROGRAM OR FILE ERROR (LENGTH ERROR, FILE CONTROL).'. 53100000
               04  MSG-0A01        PIC X(60) VALUE                      53800000
               'A PROGRAM OR TEMPORARY STORAGE ERROR (ITEM ERROR).'.    54500000
               04  MSG-0A02        PIC X(60) VALUE                      55200000
               'A PROGRAM OR TEMPORARY STORAGE ERROR (UNKNOWN QUEUE).'. 55900000
               04  MSG-0A04        PIC X(60) VALUE                      56600000
                   'AN INPUT/OUTPUT ERROR IN TEMPORARY STORAGE.'.       57300000
               04  MSG-0A08        PIC X(60) VALUE                      58000000
                   'NO SPACE IN TEMPORARY STORAGE.'.                    58700000
               04  MSG-0A20        PIC X(60) VALUE                      59400000
               'A PROGRAM OR SYSTEM ERROR (INVALID REQUEST IN TS).'.    60100000
               04  MSG-0AE1        PIC X(60) VALUE                      60800000
               'A PROGRAM OR TEMPORARY STORAGE ERROR (TS LENGTH ERROR)'.61500000
               04  MSG-0E01        PIC X(60) VALUE                      62200000
               'A PROGRAM OR PPT TABLE ERROR (UNKNOWN PROGRAM NAME).'.  62900000
               04  MSG-0EE0        PIC X(60) VALUE                      63600000
                   'A PROGRAM ERROR (INVALID PROGRAM REQUEST).'.        64300000
               04  MSG-1001        PIC X(60) VALUE                      65000000
                   'A PROGRAM ERROR (END OF DATA, USING IC).'.          65700000
               04  MSG-1004        PIC X(60) VALUE                      66400000
               'AN INPUT/OUTPUT ERROR IN TEMPORARY STORAGE (USING IC).'.67100000
               04  MSG-1011        PIC X(60) VALUE                      67800000
               'A PROGRAM OR PCT TABLE ERROR (TRANSID ERROR USING IC).'.68500000
               04  MSG-1012        PIC X(60) VALUE                      69200000
                   'A PROGRAM OR TCT TABLE ERROR (TERMIDERR USING IC).'.69900000
               04  MSG-1014        PIC X(60) VALUE                      70600000
                   'A PROGRAM OR SYSTEM ERROR (INVTSREQ USING IC).'.    71300000
               04  MSG-1081        PIC X(60) VALUE                      72000000
                   'A PROGRAM OR SYSTEM ERROR (NOT FOUND USING IC).'.   72700000
               04  MSG-10E1        PIC X(60) VALUE                      73400000
               'A PROGRAM OR TEMP STORAGE ERROR (IC LENGTH ERROR).'.    74100000
               04  MSG-10E9        PIC X(60) VALUE                      74800000
                   'A PROGRAM ERROR (INVALID REQUEST USING IC).'.       75500000
               04  MSG-10FF        PIC X(60) VALUE                      76200000
                   'A PROGRAM ERROR (ENVDEFERR USING IC).'.             76900000
               04  MSG-1804        PIC X(60) VALUE                      77600000
                   'A PROGRAM ERROR (BMS MAPFAIL).'.                    78300000
               04  MSG-1808        PIC X(60) VALUE                      79000000
                   'A PROGRAM ERROR (INVALID MAP SIZE).'.               79700000
               04  MSG-18E1        PIC X(60) VALUE                      80400000
                   'A PROGRAM ERROR (BMS LENGTH ERROR).'.               81100000
               04  MSG-MISC        PIC X(60) VALUE                      81800000
                   'AN UNKNOWN TYPE OF ERROR.'.                         82500000
           02  ERR-MSG REDEFINES ERR-LIST PIC X(60) OCCURS 31.          83200000
       LINKAGE SECTION.                                                 83900000
       01  DFHCOMMAREA.                                                 84600000
           02  ERR-PGRMID          PIC X(8).                            85300000
           02  ERR-CODE.                                                86000000
               04  ERR-FN          PIC X.                               86700000
               04  ERR-RCODE       PIC X.                               87400000
       PROCEDURE DIVISION.                                              88100000
           MOVE LOW-VALUES TO ACCTERRO.                                 88800000
           PERFORM CODE-LOOKUP THROUGH CODE-END                         89500000
               VARYING I FROM 1 BY 1 UNTIL I NOT < IX.                  90200000
           MOVE ERR-MSG (IX) TO RSNEO.                                  90900000
           MOVE EIBTRNID TO TRANEO.                                     91600000
           MOVE ERR-PGRMID TO PGMEO.                                    92300000
           IF IX < 11 MOVE EIBDS TO DSN,                                93000000
               MOVE DSN-MSG TO FILEEO.                                  93700000
           EXEC CICS SEND MAP('ACCTERR') MAPSET('ACCTSET') ERASE FREEKB 94400000
               END-EXEC.                                                95100000
           EXEC CICS ABEND ABCODE('EACC') END-EXEC.                     95800000
       CODE-LOOKUP.                                                     96500000
           IF HEX-CODE (I) = ERR-CODE MOVE I TO IX.                     97200000
       CODE-END.  EXIT.                                                 97900000
       DUMMY-END.                                                       98600000
           GOBACK.                                                      99300000
