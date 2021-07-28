       IDENTIFICATION DIVISION.                                         06000000
       PROGRAM-ID. ACCT00.                                              12000000
      *<CR_TAG_DEMO_BEGIN>
      *
      *  Copyright (C) Micro Focus 2012.
      *  All rights reserved.
      *
      *  This sample code is supplied for demonstration purposes only
      *  on an "as is" basis and "is for use at your own risk".
      *
      *<CR_TAG_DEMO_END>
       ENVIRONMENT DIVISION.                                            48000000
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.                                              60000000
       INITIAL-MAP.                                                     66000000
           EXEC CICS SEND
                     MAP('ACCTMNU')
                     MAPSET('ACCTSET') FREEKB                           72000000
                     ERASE MAPONLY
           END-EXEC                                                     79000000
           EXEC CICS RETURN TRANSID('AC01') END-EXEC
           GOBACK.                                                      93000000
