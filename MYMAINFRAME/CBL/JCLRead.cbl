      * Copyright (C) Micro Focus 2012.
      * All rights reserved.
      *
      *  This sample code is supplied for demonstration purposes only on
      *  an "as is" basis and "is for use at your own risk".
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CPJREAD.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE ASSIGN TO INFILE
                         FILE STATUS IS FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD INFILE.
       01 IN-REC           PIC X(80).

       WORKING-STORAGE SECTION.
       01 FILE-STATUS      PIC X(2).

       PROCEDURE DIVISION.
           OPEN INPUT INFILE

           IF FILE-STATUS NOT = '00'
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF

           READ INFILE

           PERFORM UNTIL FILE-STATUS NOT = '00'
               DISPLAY IN-REC
               READ INFILE
           END-PERFORM

           CLOSE INFILE

           MOVE 0 TO RETURN-CODE
           GOBACK
           .
