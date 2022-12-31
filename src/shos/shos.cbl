       >>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. SHOS.

DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 GameData.
          02 RoomNumber PIC 9.
       01 ScratchPad.
          02 Command PIC X.
PROCEDURE DIVISION.
TitleScreen.
       DISPLAY "*********************************"
       DISPLAY "*                               *"
       DISPLAY "* SUBTERRANEAN HOUSE OF SHADOWS *"
       DISPLAY "*                               *"
       DISPLAY "*        A PRODUCTION OF        *"
       DISPLAY "*       THEGRUMPYGAMEDEV        *"
       DISPLAY "*                               *"
       DISPLAY "*********************************"
       DISPLAY " "
       DISPLAY "THE HOUSE IS SITUATED AT A BEND OF THE WINDING BLOODSCAR."
       DISPLAY "CURRENTLY IS IS BADLY INFESTED BY LIONS."
       DISPLAY "WORD IS THAT ZARR, A LEGENDARY DOLL, IS STILL HIDDEN HERE.".

MainMenu.
       DISPLAY " "
       DISPLAY "MAIN MENU:"
       DISPLAY "[E]NTER THE SUBTERANEAN HOUSE OF SHADOWS"
       DISPLAY "[L]EARN HOW TO PLAY"
       DISPLAY "[Q]UIT THE GAME"
       ACCEPT Command
       EVALUATE Command
           WHEN "E"
               GO TO StartGame
           WHEN "L"
               GO TO LearnToPlay
           WHEN "Q"
               GO TO ConfirmQuit
           WHEN OTHER
               GO TO MainMenu
       END-EVALUATE.

StartGame.
       MOVE 1 TO RoomNumber
       GO TO ShowRoom.

ShowRoom.
       DISPLAY SPACE
       evaluate RoomNumber
           when 1
               GO TO FirstRoom
           when 2
               GO TO SecondRoom
           when 3
               GO TO ThirdRoom
           when other
               display RoomNumber
               stop run
       end-evaluate.

FirstRoom.
       DISPLAY "YER IN THE FIRST ROOM."
       DISPLAY "THERE IS AN OPEN PASSAGEWAY TO THE NORTH."
       DISPLAY "THERE ARE STAIRS TO THE EAST."
       DISPLAY "THERE IS A DOOR TO THE SOUTH."
       GO TO NowWhat.

SecondRoom.
       DISPLAY "YER IN THE SECOND ROOM."
       DISPLAY "THERE IS AN OPEN PASSAGEWAY TO THE SOUTH."
       GO TO NowWhat.

ThirdRoom.
       DISPLAY "YER IN THE THIRD ROOM."
       DISPLAY "THERE IS A DOOR TO THE NORTH."
       GO TO NowWhat.

NowWhat.
       DISPLAY SPACE
       IF RoomNumber IS EQUAL TO 1 OR RoomNumber IS EQUAL TO 3 THEN 
           DISPLAY "GO [N]ORTH."
       END-IF
       IF RoomNumber IS EQUAL TO 1 THEN 
           DISPLAY "GO [E]AST."
       END-IF
       IF RoomNumber IS EQUAL TO 1 OR RoomNumber IS EQUAL TO 2 THEN 
           DISPLAY "GO [S]OUTH."
       END-IF
       ACCEPT Command
       EVALUATE Command
           WHEN "E"
               GO TO MoveEast
           WHEN "N"
               GO TO MoveNorth
           WHEN "S"
               GO TO MoveSouth
           WHEN OTHER
               PERFORM InvalidCommand
               GO TO ShowRoom
       END-EVALUATE.

InvalidCommand.
       DISPLAY SPACE
       DISPLAY "INVALID COMMAND!"
       EXIT.

MoveEast.
       IF RoomNumber IS EQUAL TO 1 THEN 
           GO TO LeaveDungeon
       END-IF
       PERFORM InvalidCommand
       GO TO ShowRoom.

LeaveDungeon.
       DISPLAY " "
       DISPLAY "YOU LEAVE THE DUNGEON, ALIVE!"
       GO TO MainMenu.

MoveNorth.
       IF RoomNumber IS EQUAL TO 1 THEN
           MOVE 2 TO RoomNumber
           GO TO ShowRoom
       END-IF
       IF RoomNumber IS EQUAL TO 3 THEN
           MOVE 1 TO RoomNumber
           GO TO ShowRoom
       END-IF
       GO TO ShowRoom.

MoveSouth.
       IF RoomNumber IS EQUAL TO 1 THEN
           MOVE 3 TO RoomNumber
           GO TO ShowRoom
       END-IF
       IF RoomNumber IS EQUAL TO 2 THEN
           MOVE 1 TO RoomNumber
           GO TO ShowRoom
       END-IF
       PERFORM InvalidCommand
       GO TO ShowRoom.

LearnToPlay.
       GO TO MainMenu.

ConfirmQuit.
       DISPLAY SPACE
       DISPLAY "ARE YOU SURE YOU WANT TO QUIT?"
       DISPLAY "[Y]ES"
       DISPLAY "[N]O"
       ACCEPT Command
       EVALUATE Command
           WHEN "Y"
               GO TO QuitGame
           WHEN "N"
               GO TO MainMenu
           WHEN OTHER
               GO TO MainMenu
       END-EVALUATE.

QuitGame.
       DISPLAY " "
       DISPLAY "THANKS FOR PLAYING!"
       STOP RUN.
