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
