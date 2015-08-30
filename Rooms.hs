module Rooms (
    Description,
    Connection,
    Direction(..),
    Room(..),
    Floor(..),

    containsRoom,
    coordDirections,
    dig,
    findRoomById,
    getDirectionsFromRoom,
    getExitsForRoom,
    updateRoom,
    changeDescription 
) where

import Data.List
import Data.Maybe
import AVLTree

type Description = String

data Direction = E | W | N | S deriving (Show, Eq)

type Coordinates = (Int, Int)

data Room = Room
            {
                roomId :: Int,
                description :: Description,
                position :: Coordinates
            } deriving (Show)

instance Eq Room where
    r1 == r2 = (roomId r1) == (roomId r2)

instance Ord Room where
    r1 `compare` r2 = (roomId r1) `compare` (roomId r2)

type Connection = (Int, Int)

data Floor = Floor
            {
                rooms :: AVLTree Room,
                connections :: [Connection]
            } deriving (Show)

containsRoom :: Room -> Connection -> Bool
containsRoom room (a, b) = id == a || id == b
                        where id = roomId room

findRoomById :: Floor -> Int -> Maybe Room
findRoomById (Floor rooms _) i = findVal rooms (Room { roomId = i, description = [], position = (0,0) })

getExitsForRoom :: Floor -> Room -> [Connection]
getExitsForRoom (Floor _ exits) room = [ex | ex <- exits, containsRoom room ex]

coordDirections :: Coordinates -> Coordinates -> Direction
coordDirections (x1, y1) (x2, y2)
    | x1 == x2 && y1 == y2 = error "No direction from same space"
    | x1 /= x2 && y1 /= y2 = error "Moving in two dimensions"
    | x1 > x2 = W
    | x1 < x2 = E
    | y1 > y2 = S
    | y1 < y2 = N

connectionDirection :: Floor -> Room -> Connection -> Direction
connectionDirection floor room (id1, id2)
                    | (roomId room) == id1 = coordDirections (position a) (position b)
                    | otherwise = coordDirections (position b) (position a)
                    where a = fromJust $ findRoomById floor id1
                          b = fromJust $ findRoomById floor id2

getDirectionsFromRoom :: Floor -> Room -> [Direction]
getDirectionsFromRoom floor room = let exits = getExitsForRoom floor room
                                    in map (connectionDirection floor room) exits

moveCoords :: Coordinates -> Direction -> Coordinates
moveCoords (x, y) dir
    | dir == E = (x + 1, y)
    | dir == W = (x - 1, y)
    | dir == N = (x, y + 1)
    | dir == S = (x, y - 1)

connSwap :: Connection -> Connection
connSwap (a, b) = (b, a)

dig :: Room -> Direction -> Floor -> Floor
dig _ _ (Floor Empty []) = Floor (makeTree (Room 1 "Empty room" (0, 0))) []
dig sourceRoom dir oldFloor@(Floor rooms connections)
    | isNothing existingTarget  =
                                    let newId = (roomId $ maxValue rooms) + 1
                                        newRoom = Room newId "Empty room" targetCoords
                                        newConnection = (newId, roomId sourceRoom)
                                    in
                                        Floor (AVLTree.insert rooms newRoom) (newConnection : connections)
    | otherwise                 =
                                    let justTarget = fromJust existingTarget
                                        newConnection = (roomId justTarget, roomId sourceRoom)
                                        alreadyExists = (newConnection `elem` connections) || ((connSwap newConnection) `elem` connections)
                                    in
                                         if alreadyExists then oldFloor
                                            else Floor rooms (newConnection : connections)
    where targetCoords = moveCoords (position sourceRoom) dir
          existingTarget = find (\room -> (position room) == targetCoords) (traverse rooms)

updateRoom :: Floor -> Room -> Floor
updateRoom (Floor roomTree connections) newRoom = Floor (AVLTree.insert roomTree newRoom) connections

changeDescription :: Room -> String -> Room
changeDescription oldRoom newDesc = oldRoom { description = newDesc }
