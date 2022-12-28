       >>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. MAZEGEN.

DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 Prng-Data.
           02 dateTimeString PIC X(16).
           02 dateTime PIC 9(16) USAGE IS COMP VALUE ZEROS.
           02 dummy PIC 999.
       01 MazeData.
           02 MazeColumns OCCURS 8 TIMES.
               03 MazeRows OCCURS 8 TIMES.
                   04 State PIC X.
                   04 Doors PIC X OCCURS 4 TIMES.
       01 ScratchPad.
           02 MazeColumn PIC 9.
           02 MazeRow PIC 9.
           02 Direction PIC 9.
           02 NextColumn PIC 9.
           02 NextRow PIC 9.
           02 MazeGenComplete PIC X.
           02 DoorCandidate PIC X occurs 4 times.

PROCEDURE DIVISION.
       PERFORM Seed-Rng
       PERFORM InitializeMaze
       PERFORM GenerateMaze
       PERFORM DrawMaze
STOP RUN.

DrawMaze.
       display " "
       perform varying MazeRow from 1 by 1 until mazerow is greater than 8
           perform varying MazeColumn from 1 by 1 until MazeColumn is greater than 8
               display "#" WITH NO advancing 
               evaluate doors(MazeColumn, MazeRow,1)
                   when "Y"
                       display " " with no advancing 
                   when other 
                       display "#" with no advancing 
               end-evaluate
           end-perform
           display "#"
           perform varying MazeColumn from 1 by 1 until MazeColumn is greater than 8
               evaluate doors(MazeColumn, MazeRow,4)
                   when "Y"
                       display " " with no advancing 
                   when other 
                       display "#" with no advancing 
               end-evaluate
               display " " WITH NO advancing 
           end-perform
           display "#"
       end-perform
       perform varying MazeColumn from 1 by 1 until MazeColumn is greater than 8
           display "##" WITH NO advancing 
       end-perform
       display "#"
EXIT.

InitializeMaze.
       PERFORM VARYING MazeColumn FROM 1 BY 1 UNTIL MazeColumn IS GREATER THAN 8
           PERFORM VARYING MazeRow FROM 1 BY 1 UNTIL MazeRow IS GREATER THAN 8
               MOVE "O" TO State(MazeColumn,MazeRow)
               PERFORM VARYING Direction FROM 1 BY 1 UNTIL Direction IS greater THAN 4
                   MOVE "N" TO Doors(MazeColumn,MazeRow,Direction)
               END-PERFORM
           END-PERFORM
       END-PERFORM
EXIT.

GenerateMaze.
       COMPUTE MazeColumn = FUNCTION RANDOM() * 8 + 1
       COMPUTE MazeRow = FUNCTION RANDOM() * 8 + 1
       MOVE "I" TO State(MazeColumn, MazeRow)
       PERFORM VARYING Direction FROM 1 BY 1 UNTIL Direction IS greater  THAN 4
           PERFORM DetermineNextPosition
           IF NextColumn IS GREATER THAN 0 AND NextRow IS GREATER THAN 0 AND NextColumn IS NOT GREATER THAN 8 AND NextRow IS NOT GREATER THAN 8 THEN
               MOVE "F" TO State(NextColumn, NextRow)
           END-IF
       END-PERFORM
       PERFORM WITH TEST AFTER UNTIL MazeGenComplete IS EQUAL TO "Y"
           PERFORM WITH TEST AFTER UNTIL State(MazeColumn, MazeRow) IS EQUAL TO "F"
               COMPUTE MazeColumn = FUNCTION RANDOM() * 8 + 1
               COMPUTE MazeRow = FUNCTION RANDOM() * 8 + 1
           END-PERFORM
           PERFORM VARYING Direction FROM 1 BY 1 UNTIL Direction IS greater than 4
               perform DetermineDoorCandidacy
           end-perform
           perform with test after until DoorCandidate(Direction) is equal to "Y"
               compute direction = function random() * 4 + 1
           end-perform
           MOVE "I" TO State(MazeColumn, MazeRow)
           MOVE "Y" TO Doors(MazeColumn, MazeRow, Direction)
           perform DetermineNextPosition
           perform DetermineOppositeDirection
           MOVE "Y" TO Doors(NextColumn, NextRow, Direction)
           PERFORM VARYING Direction FROM 1 BY 1 UNTIL Direction IS greater THAN 4
               PERFORM DetermineNextPosition
               IF NextColumn IS GREATER THAN 0 AND NextRow IS GREATER THAN 0 AND NextColumn IS NOT GREATER THAN 8 AND NextRow IS NOT GREATER THAN 8 and state(NextColumn, NextRow) IS EQUAL TO "O" THEN
                   MOVE "F" TO State(NextColumn, NextRow)
               END-IF
           END-PERFORM
           PERFORM CheckMazeGenComplete
       end-perform
EXIT.

DetermineOppositeDirection.
       add 2 to Direction
       if direction is greater than 4 then 
           subtract 4 from direction
       end-if
EXIT.

DetermineDoorCandidacy.
       MOVE "Y" TO DoorCandidate(Direction)
       perform DetermineNextPosition
       if NextColumn is less than 1 or nextrow is less than 1 or nextcolumn is greater than 8 or nextrow is greater than 8 then 
           MOVE "N" TO DoorCandidate(Direction)
           exit
       end-if
       if state(nextcolumn, NextRow) is not equal to "I" then 
           move "N" to DoorCandidate(Direction)
       end-if
Exit.

CheckMazeGenComplete.
       MOVE "Y" TO MazeGenComplete
       PERFORM VARYING MazeColumn FROM 1 BY 1 UNTIL MazeColumn is greater than 8
           perform varying MazeRow from 1 by 1 until mazerow is greater than 8
               if State(MazeColumn, MazeRow) is equal to "F" then 
                   move "N" to MazeGenComplete
                   exit
               end-if
           end-perform
       end-perform
EXIT.

DetermineNextPosition.
       MOVE MazeColumn TO NextColumn
       MOVE MazeRow TO NextRow
       EVALUATE Direction
           WHEN 1
               SUBTRACT 1 FROM NextRow
           WHEN 2
               ADD 1 TO NextColumn
           WHEN 3
               ADD 1 TO NextRow
           WHEN 4
               SUBTRACT 1 FROM NextColumn
       END-EVALUATE
EXIT.

Seed-Rng.
       MOVE FUNCTION CURRENT-DATE TO dateTimeString
       MOVE FUNCTION NUMVAL(dateTimeString) TO dateTime
       COMPUTE dummy = FUNCTION RANDOM(dateTime)
Exit.
