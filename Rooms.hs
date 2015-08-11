module Rooms (
    Description,
    Connection,
    Direction(..),
    Room(..),
    Floor(..),

    containsRoom,
    coordDirections,
    dig,
    getDirectionsFromRoom,
    getExitsForRoom
) where

import Data.List
import Data.Maybe

type Description = String

data Direction = E | W | N | S deriving (Show, Eq)

type Coordinates = (Int, Int)

data Room = Room
            {
                roomId :: Int,
                description :: Description,
                position :: Coordinates
            } deriving (Show, Eq)

type Connection = (Room, Room)

data Floor = Floor
            {
                rooms :: [Room],
                connections :: [Connection]
            } deriving (Show)

containsRoom :: Room -> Connection -> Bool
containsRoom room (a, b) = room == a || room == b

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

connectionDirection :: Room -> Connection -> Direction
connectionDirection room (a, b)
                    | room == a = coordDirections (position a) (position b)
                    | otherwise = coordDirections (position b) (position a)

getDirectionsFromRoom :: Floor -> Room -> [Direction]
getDirectionsFromRoom floor room = let exits = getExitsForRoom floor room
                                    in map (connectionDirection room) exits

moveCoords :: Coordinates -> Direction -> Coordinates
moveCoords (x, y) dir
    | dir == E = (x + 1, y)
    | dir == W = (x - 1, y)
    | dir == N = (x, y + 1)
    | dir == S = (x, y - 1)

connSwap :: Connection -> Connection
connSwap (a, b) = (b, a)

dig :: Room -> Direction -> Floor -> Floor
dig _ _ (Floor [] []) = Floor [Room 1 "Empty room" (0, 0)] []
dig sourceRoom dir (Floor rooms connections)
    | isNothing existingTarget  =
                                    let newId = (roomId $ head rooms) + 1
                                        newRoom = Room newId "Empty room" targetCoords
                                        newConnection = (newRoom, sourceRoom)
                                    in
                                        Floor (newRoom : rooms) (newConnection : connections)
    | otherwise                 =
                                    let justTarget = fromJust existingTarget
                                        newConnection = (justTarget, sourceRoom)
                                        alreadyExists = (newConnection `elem` connections) || ((connSwap newConnection) `elem` connections)
                                    in
                                         if alreadyExists then Floor rooms connections
                                            else Floor rooms (newConnection : connections)
    where targetCoords = moveCoords (position sourceRoom) dir
          existingTarget = find (\room -> (position room) == targetCoords) rooms
