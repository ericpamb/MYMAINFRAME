      * Copyright (C) Micro Focus 2012.
      * All rights reserved.
      *
      *  This sample code is supplied for demonstration purposes only on
      *  an "as is" basis and "is for use at your own risk".
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CPJCREAT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTFILE ASSIGN TO OUTFILE
                          FILE STATUS IS FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD OUTFILE.
       01 OUT-REC.
           03 OUT-REC-NO   	PIC 9(5).
           03 OUT-REC-TEXT 	PIC X(75).

       WORKING-STORAGE SECTION.
       01 FILE-STATUS      PIC X(2).
       01 RECORD-COUNT     PIC 9(9) COMP.

       PROCEDURE DIVISION.
           OPEN OUTPUT OUTFILE

           IF FILE-STATUS NOT = '00'
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF

           PERFORM VARYING RECORD-COUNT FROM 1 BY 1
                   UNTIL RECORD-COUNT > 10
               MOVE RECORD-COUNT TO OUT-REC-NO
               MOVE SPACES TO OUT-REC-TEXT
               WRITE OUT-REC

               IF FILE-STATUS NOT = '00'
                   MOVE 8 TO RETURN-CODE
                   CLOSE OUTFILE
                   GOBACK
               END-IF
           END-PERFORM

           CLOSE OUTFILE

           MOVE 0 TO RETURN-CODE
           GOBACK
           .
