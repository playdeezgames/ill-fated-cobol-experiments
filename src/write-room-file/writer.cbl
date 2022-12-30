       >>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. WRITER.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
       SELECT RoomImageFile ASSIGN TO "RoomImage.txt" SEQUENTIAL.
DATA DIVISION.
FILE SECTION.
       FD RoomImageFile.       
       01 ImageRow PIC X(56).
       WORKING-STORAGE SECTION.
       01 ImageData.
          02 RoomImage.
             03 RoomImageRows PIC X(56) OCCURS 24 TIMES.
       01 ScratchPad.
          02 RowNumber PIC 999.
PROCEDURE DIVISION.
       PERFORM InitializeRoomImage.
       PERFORM WriteRoomImageFile.
       
       STOP RUN.

WriteRoomImageFile.
       OPEN OUTPUT RoomImageFile
       PERFORM VARYING RowNumber FROM 1 BY 1 UNTIL RowNumber IS GREATER THAN 24
           MOVE RoomImageRows(RowNumber) TO ImageRow
           WRITE ImageRow
       END-PERFORM
       CLOSE RoomImageFile
       EXIT.

InitializeRoomImage.
       MOVE "\_                                                    _/" TO RoomImageRows(1)
       MOVE "  \_                                                _/  " TO RoomImageRows(2)
       MOVE "    \_                                            _/    " TO RoomImageRows(3)
       MOVE "      \_                                        _/      " TO RoomImageRows(4)
       MOVE "        \_                                    _/        " TO RoomImageRows(5)
       MOVE "          \_                                _/          " TO RoomImageRows(6)
       MOVE "            +------------------------------+            " TO RoomImageRows(7)
       MOVE "            |                              |            " TO RoomImageRows(8)
       MOVE "            |                              |            " TO RoomImageRows(9)
       MOVE "            |                              |            " TO RoomImageRows(10)
       MOVE "            |                              |            " TO RoomImageRows(11)
       MOVE "            |                              |            " TO RoomImageRows(12)
       MOVE "            |                              |            " TO RoomImageRows(13)
       MOVE "            |                              |            " TO RoomImageRows(14)
       MOVE "            |                              |            " TO RoomImageRows(15)
       MOVE "            |                              |            " TO RoomImageRows(16)
       MOVE "            |                              |            " TO RoomImageRows(17)
       MOVE "            +------------------------------+            " TO RoomImageRows(18)
       MOVE "          _/                                \_          " TO RoomImageRows(19)
       MOVE "        _/                                    \_        " TO RoomImageRows(20)
       MOVE "      _/                                        \_      " TO RoomImageRows(21)
       MOVE "    _/                                            \_    " TO RoomImageRows(22)
       MOVE "  _/                                                \_  " TO RoomImageRows(23)
       MOVE "_/                                                    \_" TO RoomImageRows(24)
EXIT.
