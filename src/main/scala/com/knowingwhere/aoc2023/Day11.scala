package com.knowingwhere.aoc2023

import com.knowingwhere.aoc2023.util.Position
import scala.annotation.tailrec
import scala.io.Source

object Day11 extends App {
  val compactUniverse = Source.fromResource("day11-input.txt").getLines().toList
  val universe = expand(compactUniverse)
  val galaxies = universe.zipWithIndex.flatMap(rowEntry => createGalaxyLocationsOfRow(rowEntry._2, rowEntry._1))

  // Part 1
  println(findAllPathDistanceSum(galaxies, BigInt(0)))

  /*
    For Part 1, we had expanded the universe and did the path distance counting, however that method would not finish
    in case of Part 2 when the universe is expanding a million times (i.e. each blank row / col is replaced by a million
    blank rows or columns ... which in turn means the path calculation will need to add 999999 new blank rows or columns
    for each corresponding blank row or column

    We have kept the calculation routine from the part 1 as it is and added new methods so that we can have a reference
    to what we had initially done later stage if needed
   */

  //Part2 - don't use expanded universe, of lines that need to expand.
  val rowsToExpand: List[Int] = compactUniverse.zipWithIndex.filter(!_._1.contains('#')).map(_._2)
  val colsToExpand: List[Int] = compactUniverse.transpose.zipWithIndex.filter(!_._1.contains('#')).map(_._2)
  val compactGalaxies = compactUniverse.zipWithIndex.flatMap(rowEntry => createGalaxyLocationsOfRow(rowEntry._2, rowEntry._1))
  println(findAllPathDistanceSumPart2(compactGalaxies, 1000000, rowsToExpand, colsToExpand, BigInt(0)))


  @tailrec
  def findAllPathDistanceSumPart2(remainingGalaxies: List[Position], expansionFactor: Int, rowsToExpand: List[Int], colsToExpand: List[Int], distanceSum: BigInt): BigInt= {
    if (remainingGalaxies.isEmpty) {
      distanceSum
    } else {
      val currentStart = remainingGalaxies.head
      val allDistanceSum = getDistancesPart2(currentStart, remainingGalaxies.tail, expansionFactor, rowsToExpand, colsToExpand, distanceSum)
      findAllPathDistanceSumPart2(remainingGalaxies.tail, expansionFactor, rowsToExpand, colsToExpand, allDistanceSum)
    }
  }

  @tailrec
  def getDistancesPart2(fromGalaxy: Position, remainingGalaxies: List[Position], expansionFactor:Int, rowsToExpand: List[Int], colsToExpand: List[Int], runningSum: BigInt): BigInt = {
    if (remainingGalaxies.isEmpty) {
      runningSum
    } else {
      val toGalaxy = remainingGalaxies.head
      val smallX = Math.min(fromGalaxy.x, toGalaxy.x)
      val bigX = Math.max(fromGalaxy.x, toGalaxy.x)
      val smallY = Math.min(fromGalaxy.y, toGalaxy.y)
      val bigY = Math.max(fromGalaxy.y, toGalaxy.y)
      val colCountToExpand = colsToExpand.count(colId => colId > smallX && colId < bigX)
      val rowCountToExpand = rowsToExpand.count(rowId => rowId > smallY && rowId < bigY)

      // each row or column is replaced by the number of rows or columns in expansionFactor, so the number of ADDITIONAL
      // ROWS or COLUMNS is (expansionFactor - 1)
      val distance = (bigX - smallX) + (bigY - smallY) + colCountToExpand * (expansionFactor - 1) + rowCountToExpand * (expansionFactor - 1)
      getDistancesPart2(fromGalaxy, remainingGalaxies.tail, expansionFactor, rowsToExpand: List[Int], colsToExpand: List[Int], runningSum + distance)
    }
  }

  @tailrec
  def findAllPathDistanceSum(remainingGalaxies: List[Position], distanceSum: BigInt): BigInt= {
    if (remainingGalaxies.isEmpty) {
      distanceSum
    } else {
      val currentStart = remainingGalaxies.head
      val allDistanceSum = getDistances(currentStart, remainingGalaxies.tail, distanceSum)
      findAllPathDistanceSum(remainingGalaxies.tail, allDistanceSum)
    }
  }

  @tailrec
  def getDistances(fromGalaxy: Position, remainingGalaxies: List[Position], runningSum: BigInt): BigInt = {
    if (remainingGalaxies.isEmpty) {
      runningSum
    } else {
      val toGalaxy = remainingGalaxies.head
      val distance = Math.abs(fromGalaxy.x - toGalaxy.x) + Math.abs(fromGalaxy.y - toGalaxy.y)
      getDistances(fromGalaxy, remainingGalaxies.tail, runningSum + distance)
    }
  }

  def createGalaxyLocationsOfRow(rowIndex: Int, rowString: String): List[Position] = {
    createGalaxyLocations(0, rowIndex, rowString, List.empty)
  }

  @tailrec
  def createGalaxyLocations(currentCol: Int, currentRow: Int, remainingString: String, galaxies: List[Position]): List[Position] = {
    if (remainingString.isEmpty) {
      galaxies
    } else {
      val currentPoint = remainingString.head
      if (currentPoint == '#') {
        createGalaxyLocations(currentCol + 1, currentRow, remainingString.tail, galaxies :+ Position(currentCol, currentRow))
      } else {
        createGalaxyLocations(currentCol + 1, currentRow, remainingString.tail, galaxies)
      }
    }
  }

  def expand(compactUniverse: List[String]) = {
    val rowsToExpand: List[Int] = compactUniverse.zipWithIndex.filter(!_._1.contains('#')).map(_._2)
    val colsToExpand: List[Int] = compactUniverse.transpose.zipWithIndex.filter(!_._1.contains('#')).map(_._2)
    val rowExpandedUniverse = expandRows(compactUniverse, rowsToExpand, 0, List.empty)
    expandRows(rowExpandedUniverse.transpose.map(_.mkString), colsToExpand, 0, List.empty).transpose.map(_.mkString)
  }

  @tailrec
  def expandRows(remainingUniverseToExpand: List[String], rowsToExpand: List[Int], currentIndex:Int, accumulatedList: List[String]): List[String] = {
    if (remainingUniverseToExpand.isEmpty) {
      accumulatedList
    } else {
      val currentString = remainingUniverseToExpand.head
      val newAccumulatedList = accumulatedList :+ currentString
      val indexOfRowToExpand = if (rowsToExpand.isEmpty) -1 else rowsToExpand.head
      if (indexOfRowToExpand == currentIndex) {
        expandRows(remainingUniverseToExpand.tail, rowsToExpand.tail, currentIndex + 1, newAccumulatedList :+ currentString)
      } else {
        expandRows(remainingUniverseToExpand.tail, rowsToExpand, currentIndex + 1, newAccumulatedList)
      }
    }
  }
}
