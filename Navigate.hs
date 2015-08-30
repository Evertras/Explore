module Navigate (
    displayRoom,
    fromChar,
    genFloorByDirs,
    tryMove
) where

import AVLTree
import Data.List
import Data.Maybe
import Rooms

humanReadable :: Direction -> String
humanReadable dir
    | dir == N = "N"
    | dir == E = "E"
    | dir == S = "S"
    | dir == W = "W"

fromChar :: Char -> Direction
fromChar c = case c of
                'N' -> N
                'n' -> N
                'E' -> E
                'e' -> E
                'W' -> W
                'w' -> W
                'S' -> S
                's' -> S

displayExits :: Floor -> Room -> String
displayExits floor room = let roomExits = getDirectionsFromRoom floor room
                            in intercalate ", " (map humanReadable roomExits)

displayRoom :: Floor -> Room -> [String]
displayRoom floor room = [
                            "ID: " ++ (show . roomId $ room),
                            "Pos: " ++ (show . position $ room),
                            "Description: " ++ description room,
                            "Exits: " ++ displayExits floor room
                         ]

genTestFloor :: (Room, Floor)
genTestFloor = let startRoom = Room 1 "First room" (0, 0)
                   startFloor = Floor (makeTree startRoom) []
                   dugFloor = dig startRoom E startFloor
                   dugMore = dig startRoom N dugFloor
                in (startRoom, dugMore)

genFloorByDirs :: [Direction] -> (Floor, Room)
genFloorByDirs = let startRoom = Room 1 "Empty room" (0, 0)
                     startFloor = Floor (makeTree startRoom) []
                  in foldl digAndMove (startFloor, startRoom)

moveThrough :: Floor -> Room -> Connection -> Room
moveThrough floor source (a, b)
    | (roomId source) == a = fromJust $ findRoomById floor b
    | otherwise = fromJust $ findRoomById floor a

asBase :: Room -> Connection -> Connection
asBase room (a, b)
    | roomId room == a = (a, b)
    | roomId room == b = (b, a)
    | otherwise = error "Room is not a part of this connection"

tryMove :: Floor -> Room -> Direction -> Room
tryMove floor sourceRoom dir
    | exitsToDirection == [] = sourceRoom
    | otherwise = moveThrough floor sourceRoom (head exitsToDirection)
    where exits = map (asBase sourceRoom) (getExitsForRoom floor sourceRoom)
          exitsToDirection = [(a, b) | (a, b) <- exits, coordDirections (posFromId a) (posFromId b) == dir]
          posFromId = position . fromJust . findRoomById floor

digAndMove :: (Floor, Room) -> Direction -> (Floor, Room)
digAndMove (floor, room) dir = let newFloor = dig room dir floor
                                   newRoom = tryMove newFloor room dir
                                 in (newFloor, newRoom)
