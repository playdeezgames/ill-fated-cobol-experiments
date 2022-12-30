       >>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. READFILE.
ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT RoomFrameFile ASSIGN TO "roomframe.txt"
                   ORGANIZATION IS SEQUENTIAL.
               SELECT OutputFile ASSIGN TO "output.txt"
                   ORGANIZATION IS SEQUENTIAL.

DATA DIVISION.
       FILE SECTION.
       FD OutputFile.
       01 OutputBuffer.
          02 OutputBufferLine PIC X(56).
       FD RoomFrameFile.
       01 RoomFrameBuffer.
          02 BufferLine PIC X(56).
       WORKING-STORAGE SECTION.
       01 DoneReading PIC X VALUE "N".
           88 IsNotDoneReading VALUE "N".
           88 IsDoneReading VALUE "Y".

PROCEDURE DIVISION.
       OPEN INPUT RoomFrameFile
       OPEN EXTEND OutputFile
       SET IsNotDoneReading TO TRUE
       PERFORM UNTIL IsDoneReading
           READ RoomFrameFile
               AT END SET IsDoneReading TO TRUE
           END-READ
           IF IsNotDoneReading THEN
               MOVE RoomFrameBuffer TO OutputBuffer
               WRITE OutputBuffer
           END-IF
       END-PERFORM
       CLOSE RoomFrameFile, OutputFile
STOP RUN.
