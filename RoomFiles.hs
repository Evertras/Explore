module RoomFiles (
    saveFloor
) where

import Rooms
import System.IO
import Data.Char
import Data.List
import Data.Maybe

descBegin = "--BEGIN--"

descEnd = "--END--"

separator = "----------------"

serializeRoom :: Room -> [String]
serializeRoom (Room roomId description position) =
        [
            show roomId,
            show position,
            descBegin,
            description,
            descEnd
        ]

serializeConnection :: Connection -> String
serializeConnection (a, b) = (show $ roomId a) ++ "," ++ (show $ roomId b)

saveFloor :: Floor -> FilePath -> IO ()
saveFloor (Floor rooms connections) filePath = do 
    writeFile filePath "1\n"
    let roomData = map unlines (map serializeRoom rooms)
    let connectionData = map serializeConnection connections
    appendFile filePath (unlines roomData)
    appendFile filePath (separator ++ "\n")
    appendFile filePath (unlines connectionData)

parseSections :: [String] -> (Int, [String], [String])
parseSections (x:xs) =
    (read x :: Int, fst spl, tail $ snd spl)
    where
        splitIndex = fromJust $ separator `elemIndex` xs 
        spl = splitAt splitIndex xs

loadFloor :: FilePath -> IO Floor
loadFloor filePath = do
    fileContents <- readFile filePath
    let linedContents = lines fileContents
    return $ Floor [] []
