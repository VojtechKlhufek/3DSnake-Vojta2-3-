module Main
  ( (++)
  , Explosion
  , Direction(..)
  , Coordinates3D
  , World
  , depth
  , draw
  , handleEvent
  , height
  , main
  , randomFoodCoords
  , randomFoodInitCoords
  , width
  )
  where

import Data.Int (toNumber)
import Data.List (List(..))
import Data.Tuple (Tuple(..), fst, snd)
import Prelude
import Math (sqrt, pow)
import Color (hsl)
import Data.Array as Arr
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Sequence (Seq, cons, filter, head, length)
import Data.Sequence as Seq
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Reactor (executeDefaultBehavior, getW, runReactor, updateW_)
import Reactor.Events (Event(..))
import Reactor.Graphics.Colors as Color
import Reactor.Graphics.Drawing (Drawing, fill, tile)
import Reactor.Reaction (Reaction, ReactionM)

infixr 4 cons as ++

type Coordinates3D
  = { x :: Int, y :: Int, z :: Int }

type Explosion 
  = {coords :: Coordinates3D, 
  explodingCoords :: List (Tuple Int Coordinates3D), 
  radius :: Int, 
  inExplosion :: Boolean}

type World
  = { snake :: Seq Coordinates3D, 
  food :: Coordinates3D, 
  direction :: Direction, 
  time :: Number, 
  explosion :: Explosion}

data Direction
  = Up
  | Down
  | Right
  | Left
  | Above
  | Below

width :: Int
width = 20
radiusConst :: Int
radiusConst = 7
height :: Int
height = 20

depth :: Int
depth = 10

startingSnake :: Seq { x :: Int, y :: Int, z :: Int}
startingSnake = { x: 0, y: 2, z: 0 } ++ { x: 0, y: 1, z: 0 } ++ pure { x: 0, y: 0, z: 0 }



main :: Effect Unit
main = do
  let
    snake = startingSnake
  food <- randomFoodInitCoords snake
  let
    initial = { snake, 
      food: food,
      direction: Down, 
      time: 0.0029, 
      explosion: {coords:{x:0,y:0,z:0}, 
        explodingCoords: Nil, 
        inExplosion: false, 
        radius: 0}}
  let
    reactor = { initial, draw, handleEvent, isPaused: const false }
  runReactor reactor { title: "Snake", width, height }



draw :: World -> Drawing
draw { snake, food, explosion:{radius: exRadius, inExplosion, explodingCoords}} =
    let conv3Dto2D {x, y} = {x, y}
        food2D = conv3Dto2D food
        currentZ = (fromMaybe { x: 0, y: 0, z: 0 } (head snake)).z
        snake2D = Seq.map conv3Dto2D $ filter (\x -> currentZ == x.z) snake
        snakeShadow = Seq.map conv3Dto2D $ filter (\x -> currentZ < x.z) snake 
    in do
        if currentZ == food.z then do
          fill Color.pink600 $ tile food2D
        else if currentZ < food.z then do
          fill Color.red300 $ tile food2D
        else pure unit

        for_ snakeShadow $ \block -> fill Color.green300 $ tile block
        for_ snake2D $ \block -> fill Color.green900 $ tile block
        when inExplosion 
          let 
            listing = List.filter (\a -> fst a <= exRadius) explodingCoords
            listingClrs = map (\a -> Tuple (hslVal a) a) listing 
          in 
            for_ listingClrs $ \block -> fill (fst block) $ tile $ conv3Dto2D (snd (snd block))

  where hslVal a = 
          let 
            exRadiusNum = toNumber exRadius
            division = 4.6416 / exRadiusNum
            fstANum = toNumber (fst a)

            hue = (60.0 - sqrt (60.0 * 60.0 / exRadiusNum  * fstANum))
            saturation = (100.0 - ((division * fstANum) * pow (division * fstANum) 2.0))/100.0
            light = (50.0 - sqrt (18.0 * 18.0 / exRadiusNum  * fstANum))/100.0 in

            hsl hue saturation light



handleEvent :: Event -> Reaction World
handleEvent event = do
  { snake, food, direction, time, explosion: {coords, explodingCoords, radius: exRadius, inExplosion}} <- getW
  if inExplosion then 
    case event of
      Tick {delta} -> 
        if exRadius == radiusConst - 1 && time > 3.0 then updateW_ {time: 0.0,
          snake: startingSnake, 
          direction: Down, 
          explosion: 
            {coords:{x:0,y:0,z:0}, 
            explodingCoords: Nil, 
            inExplosion: false, 
            radius: 0} }
        else if exRadius == radiusConst - 1 then updateW_ {time: time + delta} 
        else if time > 0.4 then
          updateW_ {time: 0.0, explosion: {coords, explodingCoords, radius: exRadius + 1, inExplosion}} 
        else 
          do
            updateW_ {time: time + delta} 
            executeDefaultBehavior
      _ -> executeDefaultBehavior
  else 
    case Seq.head snake of
      Nothing -> executeDefaultBehavior
      Just { x, y, z } -> case Seq.init snake of
        Nothing -> executeDefaultBehavior
        Just newTail -> case event of
          KeyPress { key: "a" } -> updateW_ {direction: Left} 
          KeyPress { key: "d" } -> updateW_ {direction: Right}
          KeyPress { key: "s" } -> updateW_ {direction: Down}
          KeyPress { key: "w" } -> updateW_ {direction: Up}
          KeyPress { key: " " } -> updateW_ {direction: Above}
          KeyPress { key: "Shift" } -> updateW_ {direction: Below}
          Tick {delta} ->
            let { xChange, yChange, zChange} = case direction of
                  Left -> { xChange: -1, yChange: 0, zChange: 0}
                  Right -> { xChange: 1, yChange: 0, zChange: 0}
                  Down-> { xChange: 0, yChange: 1, zChange: 0}
                  Up -> { xChange: 0, yChange: -1, zChange: 0}
                  Above -> { xChange: 0, yChange: 0, zChange: 1}
                  Below -> { xChange: 0, yChange: 0, zChange: -1} 
                passing = { xChange, yChange, zChange}
                snakeIsInSnake = (length $ filter (\n -> n == { x: x + xChange, y: y  + yChange, z: z  + zChange }) newTail) > 0
            in
              if time > 0.4 && (snakeIsWall direction || snakeIsInSnake) then
                updateW_ {explosion: 
                  {coords:{x,y,z}, 
                  explodingCoords: explodingCoordss createGrid {x,y,z} radiusConst 0, 
                  radius: exRadius, 
                  inExplosion: true}}
              else if time > 0.4 then do
                makeAMove passing
                updateW_ {time: 0.0} 
              else do
                updateW_ {time: time + delta} 
          _ -> executeDefaultBehavior

          where
          makeAMove { xChange, yChange, zChange} =
            let
              snakeHead = { x: x + xChange, y: y + yChange , z: z + zChange}
              newSnake = snakeHead ++ newTail
            in
              if snakeHead == food then do
                newFood <- randomFoodCoords newSnake
                updateW_ { snake: (food ++ newSnake), food: newFood}
              else
                updateW_ { snake: newSnake}
            
          snakeIsWall Left = x == 0
          snakeIsWall Right = x == width - 1
          snakeIsWall Down = y == height - 1
          snakeIsWall Up = y == 0
          snakeIsWall _ = false



explodingCoordss :: forall a b.  Array { x :: Int, y :: Int| a} ->
   { x :: Int, y :: Int | b } ->
    Int ->
    Int ->
    List(Tuple Int{ x :: Int , y :: Int| a })
explodingCoordss gridArr explosionInit radius arrIt = 
  case Arr.index gridArr arrIt of
    Nothing -> Nil
    Just a -> if distanceFromCords a < radius 
      then 
        (Tuple (distanceFromCords a) a) List.: (explodingCoordss gridArr explosionInit radius (arrIt + 1))
      else
        explodingCoordss gridArr explosionInit radius (arrIt + 1) 
  where
    distanceFromCords a = absoluteValue (a.x - explosionInit.x) + absoluteValue (a.y - explosionInit.y)
    absoluteValue i = if i < 0 then i * -1 else i


createGrid :: Array { x :: Int, y :: Int, z :: Int  }
createGrid = map (to3D) $ 0 Arr... (width * height * depth) 
  where 
    to3D :: Int -> Coordinates3D
    to3D i = do
      let wXH = (width * height)
      let modWXH = i `mod` wXH
    
      { x: modWXH `mod` width, y: modWXH/ width, z: i / wXH }


randomFoodInitCoords :: Seq { x :: Int, y :: Int, z :: Int } ->
  Effect { x :: Int, y :: Int, z :: Int }
randomFoodInitCoords snake = do
  randomX <- randomInt 0 (width - 1)
  randomY <- randomInt 0 (height - 1)
  randomZ <- randomInt 0 (depth - 1)
  let
    rec = { x: randomX, y: randomY, z: randomZ }
  if Seq.null (Seq.filter (\a -> a == rec) snake) then pure rec else randomFoodInitCoords snake


randomFoodCoords :: forall a. Seq { x :: Int, y :: Int, z :: Int}-> 
  ReactionM a { x :: Int , y :: Int , z :: Int }
randomFoodCoords snake = do

  numOfCoords <- liftEffect $ randomInt 0 ((width) * (height) * (depth))

  let {x: unmovedX, y: unmovedY, z: unmovedZ} = intToCoords numOfCoords

  let containingSnake = Seq.length $ filter (isSnakeContained unmovedX unmovedY unmovedZ) snake

  moveTillNotSnake (numOfCoords - containingSnake)

    where
      isSnakeContained x y z sPart = 
        case compare sPart.z z of
          LT -> true
          EQ -> case compare sPart.y y of
            LT -> true
            EQ -> case compare sPart.x x of
              LT -> true
              EQ -> true
              GT -> false
            GT -> false
          GT -> false

      moveTillNotSnake coordsInInt = 
        let 
          rec = intToCoords coordsInInt
        in
          if Seq.null (Seq.filter (\a -> a == rec) snake) then pure rec else moveTillNotSnake (coordsInInt + 1)

      intToCoords num = 
        let
          x = num - (z * ((width) * (height))) - y * (width)
          y = (num - (z * ((width) * (height)))) / (width)
          z = num / ((width) * (height)) in
          { x, y, z }
