package com.knowingwhere.aoc2023

import com.knowingwhere.aoc2023.util.Position

import scala.annotation.tailrec
import scala.io.Source

object Day3 extends App {
  val lines = Source.fromResource("day3-input.txt").getLines().toList
  val symbolPositions = getSymbolPositions(lines)

  val symbolCoverage = getPositionCoverage(symbolPositions)
  val numbersInGrid = getNumbersInGridWithPositions(lines)

  val partNumbers = numbersInGrid.filter(_.coverage.intersect(symbolCoverage.toList).nonEmpty)
  println(partNumbers.map(_.theValue).sum)

  private def isSymbol(eachChar: Char):Boolean = {
     !eachChar.equals('.') && !eachChar.isDigit
  }

  private def getPositions(eachLineWithIndex: (String, Int)):List[Position] = {
    val positions = for (characterIndex <- eachLineWithIndex._1.zipWithIndex
        if isSymbol(characterIndex._1)
        ) yield Position(eachLineWithIndex._2, characterIndex._2)
    positions.toList
  }

  def getSymbolPositions(lines: List[String]):List[Position] = {
    lines.zipWithIndex.flatMap(eachLineWithIndex => getPositions(eachLineWithIndex))
  }

  private def getCoverageArea(eachPos: Position): List[Position] = {
    List(Position(eachPos.x - 1, eachPos.y - 1), Position(eachPos.x - 1, eachPos.y), Position(eachPos.x - 1, eachPos.y + 1),
      Position(eachPos.x, eachPos.y - 1), Position(eachPos.x, eachPos.y), Position(eachPos.x, eachPos.y + 1),
      Position(eachPos.x + 1, eachPos.y - 1), Position(eachPos.x + 1, eachPos.y), Position(eachPos.x + 1, eachPos.y + 1))
  }

  /**
   * Gets the coverage from the points. Each coordinate is supposed to "cover" its own position
   * as well as one place to the left, right, up, down and diagonal
   *
   * @param positions seed positions
   * @return List of Positions as coverage of the list of positions passed in
   */
  def getPositionCoverage(positions: List[Position]):Set[Position] = {
    positions.flatMap(getCoverageArea).toSet
  }

  def getNumbersInGridWithPositions(lines: List[String]):List[NumberInGrid] = {
    lines.zipWithIndex.flatMap(getNumbersWithPositionInGrid)
  }

  def getNumbersWithPositionInGrid(lineWithIndex: (String, Int)):List[NumberInGrid] = {
    val numberPositions = generateNumberAndPosition(lineWithIndex._1, Position(lineWithIndex._2, 0), "", Nil, Nil)
    numberPositions
  }

  @tailrec
  def generateNumberAndPosition(remainingString: String, currentPosition: Position, currentNumString: String, currentNumbers: List[NumberInGrid], positionsList: List[Position]): List[NumberInGrid] = {
    if (remainingString.isEmpty) {
      if (currentNumString.isEmpty)
        currentNumbers
      else
        currentNumbers :+ NumberInGrid(currentNumString.toInt, positionsList)
    } else {
      if (remainingString.head.isDigit) {
        generateNumberAndPosition(remainingString.tail, Position(currentPosition.x, currentPosition.y + 1), currentNumString + remainingString.head, currentNumbers, positionsList :+ currentPosition)
      } else {
        if (currentNumString.isEmpty) { //not a digit and nothing in our number buffer, simply go to next char
          generateNumberAndPosition(remainingString.tail, Position(currentPosition.x, currentPosition.y + 1), currentNumString, currentNumbers, positionsList)
        } else { //we have reached a place where our buffer has a number and positions
          val numberInGrid = NumberInGrid(currentNumString.toInt, positionsList)
          generateNumberAndPosition(remainingString.tail, Position(currentPosition.x, currentPosition.y + 1), "", currentNumbers :+ numberInGrid, Nil)
        }
      }
    }
  }}



/**
 *
 * @param theValue the value of the number
 * @param coverage list of coordinates that the number spans on the grid
 *                 e.g. given a grid below,
 *                 467..114..
 *                 ...*......
 *                 ..35..633.
 *
 *  A numberInGrid will be: (467, [(0,0),(0,1),(0,2)]
 *  A numberInGrid will be: (35, [(2,2),(2,3)]
 */
case class NumberInGrid(theValue: Int, coverage: List[Position])
