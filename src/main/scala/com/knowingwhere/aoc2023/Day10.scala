package com.knowingwhere.aoc2023

import com.knowingwhere.aoc2023.util.Position

import scala.annotation.tailrec
import scala.io.Source

object Day10 extends App {
  val tiles = Source.fromResource("day10-input.txt").getLines().zipWithIndex.map(eachLine=>createTiles(eachLine)).toList.flatten
  val start = tiles.find(_.tileType == 'S').get
  val startPos = start.location

  //starting the search in north direction, as looking at the input, we know it can only be north-south
  val path = tracePath(tiles, Position(startPos.x, startPos.y - 1), List(startPos))
  println(path.size / 2)

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