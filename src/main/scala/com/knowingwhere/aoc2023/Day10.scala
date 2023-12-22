package com.knowingwhere.aoc2023

import com.knowingwhere.aoc2023.util.Position

import scala.annotation.tailrec
import scala.io.Source

object Day10 extends App {
  val tileRows = Source.fromResource("day10-input.txt").getLines().toList
  val tiles = tileRows.zipWithIndex.flatMap(eachLine => createTiles(eachLine))
  val start = tiles.find(_.tileType == 'S').get
  val startPos = start.location

  //starting the search in north direction, as looking at the input, we know it can only be north-south
  val path = tracePath(tiles, Position(startPos.x, startPos.y - 1), List(startPos))
  println(path.size / 2)

  //Part 2 - point-in-polygon problem - sweep from left to right in each row
  val insideCounts = tileRows.zipWithIndex.map(eachRow => findInsidePositionCount(eachRow._1, path, eachRow._2))
  println(insideCounts.sum)

  def findInsidePositionCount(tileRow: String, path: List[Position], rowIndex: Int): Int = {
    findInsidePositionCountRecurse(tileRow, path, rowIndex, 0, sweepingInside = false, "", 0)
  }


  // We are using the ray casting algorithm where we alternate between "sweeping outside" and "sweeping inside" whenever we CROSS a boundary
  // going from left to right
  //
  // The logic used is
  // Whenever we encounter a pipe (|) => we deem it as crossing a boundary
  // Whenever we encounter an 'F' that's a corner of a segment and the next corner decides whether we crossed the boundary
  // "F7" - indicates we just touched the boundary but did not cross (we continue to sweep where we were - inside or outside)
  // "FJ" - indicates we crossed the boundary so change the sweeping direction
  //
  // Whenever we encounter an 'L' that's a corner of a segment and the next corner decides whether we crossed the boundary
  // "LJ" - indicates we just touched the boundary but did not cross (we continue to sweep where we were - inside or outside)
  // "L7" - indicates we crossed the boundary so change the sweeping direction

  @tailrec
  private def findInsidePositionCountRecurse(remainingString: String, path: List[Position], rowIndex: Int, colIndex: Int, sweepingInside: Boolean, borderTileCombination: String, insideCount: Int): Int = {
    if (remainingString.isEmpty) {
      insideCount
    } else {
      val currentTile = remainingString.head
      val currentPos = Position(colIndex, rowIndex)

      if (path.contains(currentPos)) {
        currentTile match {
          case '|' => //border crossing, change the sweep direction
            findInsidePositionCountRecurse(remainingString.tail, path, rowIndex, colIndex + 1, !sweepingInside, "", insideCount)
          case '-' => // on the border, just keep moving
            findInsidePositionCountRecurse(remainingString.tail, path, rowIndex, colIndex + 1, sweepingInside, borderTileCombination, insideCount)
          case 'F' => //encountered a corner, push it to border combination and we will decide later
            findInsidePositionCountRecurse(remainingString.tail, path, rowIndex, colIndex + 1, sweepingInside, "F", insideCount)
          case 'L' => //encountered a corner, push it to border combination and we will decide later
            findInsidePositionCountRecurse(remainingString.tail, path, rowIndex, colIndex + 1, sweepingInside, "L", insideCount)
          case '7' => //encontered a closing corner, sweep direction will depend on what we already know
            if (borderTileCombination.head == 'F') {// no change in sweeping direction, reset the border combination
              findInsidePositionCountRecurse(remainingString.tail, path, rowIndex, colIndex + 1, sweepingInside, "", insideCount)
            } else { // this combination is one inetersection of ray, change sweeping direction, reset border combination
              findInsidePositionCountRecurse(remainingString.tail, path, rowIndex, colIndex + 1, !sweepingInside, "", insideCount)
            }
          case 'J' => //encontered a closing corner, sweep direction will depend on what we already know
            if (borderTileCombination.head == 'L') { // no change in sweeping direction, reset the border combination
              findInsidePositionCountRecurse(remainingString.tail, path, rowIndex, colIndex + 1, sweepingInside, "", insideCount)
            } else { // this combination is one inetersection of ray, change sweeping direction, reset border combination
              findInsidePositionCountRecurse(remainingString.tail, path, rowIndex, colIndex + 1, !sweepingInside, "", insideCount)
            }
          case 'S' => //little bit of cheating, for current input, we know 'S' is a '|', otherwise we should determine that first
            findInsidePositionCountRecurse(remainingString.tail, path, rowIndex, colIndex + 1, !sweepingInside, "", insideCount)
          case _ =>
            throw new IllegalStateException("if something is on path and not one of the tiles in cases above, we have a problem")
        }
      } else {
        if (sweepingInside) { // something not on path and sweeping inside. increment the count
          findInsidePositionCountRecurse(remainingString.tail, path, rowIndex, colIndex + 1, sweepingInside, "", insideCount + 1)
        } else {
          findInsidePositionCountRecurse(remainingString.tail, path, rowIndex, colIndex + 1, sweepingInside, "", insideCount)
        }
      }
    }
  }

  @tailrec
  def tracePath(tiles: List[Tile], nextPosition: Position, path: List[Position]): List[Position] = {
    if (path.contains(nextPosition)) { // we have completed a loop
      path
    } else {
      val nextTile = tiles.find(_.location == nextPosition).get
      val newNextPosition = nextTile.tileType match {
        case '|' => if (path.last.y < nextPosition.y) { // we traced from north
          Position(nextPosition.x, nextPosition.y + 1)
        } else {
          Position(nextPosition.x, nextPosition.y - 1)
        }
        case 'L' => if (path.last.x > nextPosition.x) { //we traced from east
          Position(nextPosition.x, nextPosition.y - 1)
        } else {
          Position(nextPosition.x + 1, nextPosition.y)
        }
        case 'F' => if (path.last.x > nextPosition.x) { //we traced from east
          Position(nextPosition.x, nextPosition.y + 1)
        } else {
          Position(nextPosition.x + 1, nextPosition.y)
        }
        case 'J' => if (path.last.y < nextPosition.y) { // we traced from north
          Position(nextPosition.x - 1, nextPosition.y)
        } else {
          Position(nextPosition.x, nextPosition.y - 1)
        }
        case '7' => if (path.last.y > nextPosition.y) { // we traced from south
          Position(nextPosition.x - 1, nextPosition.y)
        } else {
          Position(nextPosition.x, nextPosition.y + 1)
        }
        case '-' => if (path.last.x < nextPosition.x) { // we traced from west
          Position(nextPosition.x + 1, nextPosition.y)
        } else {
          Position(nextPosition.x - 1, nextPosition.y)
        }
        case _ => throw new IllegalStateException //should never happen
      }
      tracePath(tiles, newNextPosition, path :+ nextPosition)
    }
  }

  def createTiles(eachLine: (String, Int)) = {
    eachLine._1.toList.zipWithIndex.map(eachChar => createTile(eachChar._1, eachLine._2, eachChar._2))
  }

  def createTile(tileType: Char, row: Int, col: Int) = {
    Tile(Position(col, row), tileType)
  }
}

case class Tile(location: Position, tileType: Char)
