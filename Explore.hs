import Control.Monad
import Rooms
import RoomFiles
import Navigate

movementEntries = ['N', 'n', 'E', 'e', 'S', 's', 'W', 'w']

editDescriptionEntries = ['D', 'd']

displayRoomText :: Floor -> Room -> IO ()
displayRoomText floor room = do
        let lines = displayRoom floor room
        sequence (map putStrLn lines)
        return ()

main = do
    let (floor, startRoom) = genFloorByDirs [N, N, E, W, N, E, W, N, E, W, N, N, W]
    saveFloor floor "floor.txt"
    mainLoop floor startRoom

mainLoop :: Floor -> Room -> IO ()
mainLoop floor room = do
    putStrLn " "
    displayRoomText floor room

    command <- getChar

    if (command `elem` movementEntries) then do
        let movedTo = tryMove floor room (fromChar command)
        mainLoop floor movedTo
    else if (command `elem` editDescriptionEntries) then do
        putStrLn ""
        newDesc <- getLine
        let updatedRoom = changeDescription room newDesc
        let updatedFloor = updateRoom floor updatedRoom
        saveFloor updatedFloor "floor.txt"
        mainLoop updatedFloor updatedRoom
    else do
        putStrLn "Unknown command"
        mainLoop floor room
