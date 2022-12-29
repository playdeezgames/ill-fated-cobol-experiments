       >>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. CRAWLER.

DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 FrameData.
           02 FrameRows OCCURS 24 TIMES.
               03 FrameColumns OCCURS 56 TIMES.
                  04 FramePixels PIC X.
       01 ImageData.
           02 RoomImage.
              03 RoomImageRows PIC X(56) OCCURS 24 TIMES.
           02 AheadDoorImage.
              03 AheadDoorImageRows OCCURS 10 TIMES.
                 04 AheadDoorImagePixels PIC X OCCURS 10 TIMES.
           02 LeftDoorImage.
              03 LeftDoorImageRows OCCURS 16 TIMES.
                 04 LeftDoorImagePixels PIC X OCCURS 6 TIMES.
           02 RightDoorImage.
              03 RightDoorImageRows OCCURS 16 TIMES.
                 04 RightDoorImagePixels PIC X OCCURS 6 TIMES.
           02 KoboldImage.
              03 KoboldImageRows OCCURS 12 TIMES.
                 04 KoboldImagePixels PIC X OCCURS 24 TIMES.
                    88 IsVisible VALUES " ", "#".
       01 ScratchPad.
          02 RenderRow PIC 99.
             88 NoMoreRows VALUES 25 THRU 99.
          02 RenderColumn PIC 99.
          02 FramePixel PIC X.
          02 SourceRow PIC 99.
          02 SourceColumn PIC 99.

PROCEDURE DIVISION.
       CALL "RNG"
       PERFORM InitializeImages
       PERFORM DrawRoom
       PERFORM PresentFrame
STOP RUN.

DrawRoom.
       PERFORM DrawRoomFrame
       PERFORM DrawAheadDoor
       PERFORM DrawLeftDoor
       PERFORM DrawRightDoor
       PERFORM DrawKobold
EXIT.

DrawKobold.
       PERFORM VARYING SourceRow FROM 1 BY 1 UNTIL SourceRow IS GREATER THAN 12 AFTER SourceColumn FROM 1 BY 1 UNTIL SourceColumn IS GREATER THAN 24
           IF IsVisible(SourceRow, SourceColumn) THEN
               MOVE KoboldImagePixels(SourceRow, SourceColumn) TO FramePixels(SourceRow + 9, SourceColumn + 16)
           END-IF
       END-PERFORM
EXIT.

DrawAheadDoor.
       PERFORM VARYING SourceRow FROM 1 BY 1 UNTIL SourceRow IS GREATER THAN 10 AFTER SourceColumn FROM 1 BY 1 UNTIL SourceColumn IS GREATER THAN 10
           MOVE AheadDoorImagePixels(SourceRow, SourceColumn) TO FramePixels(SourceRow + 9, SourceColumn + 23)
       END-PERFORM
EXIT.

DrawLeftDoor.
       PERFORM VARYING SourceRow FROM 1 BY 1 UNTIL SourceRow IS GREATER THAN 16 AFTER SourceColumn FROM 1 BY 1 UNTIL SourceColumn IS GREATER THAN 6
           MOVE LeftDoorImagePixels(SourceRow, SourceColumn) TO FramePixels(SourceRow + 7, SourceColumn + 3)
       END-PERFORM
EXIT.

DrawRightDoor.
       PERFORM VARYING SourceRow FROM 1 BY 1 UNTIL SourceRow IS GREATER THAN 16 AFTER SourceColumn FROM 1 BY 1 UNTIL SourceColumn IS GREATER THAN 6
           MOVE RightDoorImagePixels(SourceRow, SourceColumn) TO FramePixels(SourceRow + 7, SourceColumn + 47)
       END-PERFORM
EXIT.

DrawRoomFrame.
       MOVE RoomImage TO FrameData
EXIT.

PresentFrame.
       PERFORM PresentFrameRow VARYING RenderRow FROM 1 BY 1 UNTIL NoMoreRows
EXIT.

PresentFrameRow.
       DISPLAY FrameRows(RenderRow)
EXIT.

InitializeImages.
       PERFORM InitializeRoomImage
       PERFORM InitializeAheadDoorImage
       PERFORM InitializeLeftDoorImage
       PERFORM InitializeRightDoorImage
       PERFORM InitializeKoboldImage
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

InitializeAheadDoorImage.
       MOVE "+--------+" TO AheadDoorImageRows(1)
       MOVE "|        |" TO AheadDoorImageRows(2)
       MOVE "|        |" TO AheadDoorImageRows(3)
       MOVE "|        |" TO AheadDoorImageRows(4)
       MOVE "|        |" TO AheadDoorImageRows(5)
       MOVE "|        |" TO AheadDoorImageRows(6)
       MOVE "|        |" TO AheadDoorImageRows(7)
       MOVE "|        |" TO AheadDoorImageRows(8)
       MOVE "+        +" TO AheadDoorImageRows(9)
EXIT.

InitializeLeftDoorImage.
       MOVE "+\_   " TO LeftDoorImageRows(1)
       MOVE "|  \_ " TO LeftDoorImageRows(2)
       MOVE "|    +" TO LeftDoorImageRows(3)
       MOVE "|    |" TO LeftDoorImageRows(4)
       MOVE "|    |" TO LeftDoorImageRows(5)
       MOVE "|    |" TO LeftDoorImageRows(6)
       MOVE "|    |" TO LeftDoorImageRows(7)
       MOVE "|    |" TO LeftDoorImageRows(8)
       MOVE "|    |" TO LeftDoorImageRows(9)
       MOVE "|    |" TO LeftDoorImageRows(10)
       MOVE "|    |" TO LeftDoorImageRows(11)
       MOVE "|    |" TO LeftDoorImageRows(12)
       MOVE "|    +" TO LeftDoorImageRows(13)
       MOVE "|     " TO LeftDoorImageRows(14)
       MOVE "|     " TO LeftDoorImageRows(15)
       MOVE "+     " TO LeftDoorImageRows(16)
EXIT.

InitializeRightDoorImage.
       MOVE "   _/+" TO RightDoorImageRows(1)
       MOVE " _/  |" TO RightDoorImageRows(2)
       MOVE "+    |" TO RightDoorImageRows(3)
       MOVE "|    |" TO RightDoorImageRows(4)
       MOVE "|    |" TO RightDoorImageRows(5)
       MOVE "|    |" TO RightDoorImageRows(6)
       MOVE "|    |" TO RightDoorImageRows(7)
       MOVE "|    |" TO RightDoorImageRows(8)
       MOVE "|    |" TO RightDoorImageRows(9)
       MOVE "|    |" TO RightDoorImageRows(10)
       MOVE "|    |" TO RightDoorImageRows(11)
       MOVE "|    |" TO RightDoorImageRows(12)
       MOVE "+    |" TO RightDoorImageRows(13)
       MOVE "     |" TO RightDoorImageRows(14)
       MOVE "     |" TO RightDoorImageRows(15)
       MOVE "     +" TO RightDoorImageRows(16)
EXIT.

InitializeKoboldImage.
       MOVE "........................" TO KoboldImageRows(1)
       MOVE "........................" TO KoboldImageRows(2)
       MOVE "........##....##........" TO KoboldImageRows(3)
       MOVE "........########........" TO KoboldImageRows(4)
       MOVE "........  ##  ##........" TO KoboldImageRows(5)
       MOVE "........######.........." TO KoboldImageRows(6)
       MOVE "..........    ####......" TO KoboldImageRows(7)
       MOVE "........    ####..##...." TO KoboldImageRows(8)
       MOVE "......##..######..##...." TO KoboldImageRows(9)
       MOVE "..........######........" TO KoboldImageRows(10)
       MOVE "..........##..##........" TO KoboldImageRows(11)
       MOVE "........####..##........" TO KoboldImageRows(12)
EXIT.
