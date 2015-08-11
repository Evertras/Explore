import Control.Monad
import Rooms
import RoomFiles
import Navigate

validEntries = ['N', 'n', 'E', 'e', 'S', 's', 'W', 'w']

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
    displayRoomText floor room

    forever $ do
        dir <- getChar
        when (dir `elem` validEntries) $ do
            let movedTo = tryMove floor room (fromChar dir)
            mainLoop floor movedTo
