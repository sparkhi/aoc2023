package com.knowingwhere.aoc2023

import com.knowingwhere.aoc2023.util.Position

import scala.annotation.tailrec
import scala.io.Source
import scala.util.chaining.scalaUtilChainingOps

object Day18 extends App {
  val digPlan = Source.fromResource("day18-input.txt").getLines().map(createInstruction).toList

  val positionLoop = getDigLoop(digPlan, List(Position(0, 0)))

  val minY = positionLoop.map(_.y).min
  val maxY = positionLoop.map(_.y).max
  val minX = positionLoop.map(_.x).min
  val maxX = positionLoop.map(_.x).max

  //Uncomment following lines to see visualization
//  val visuals = List.range(minY, maxY + 1).map(constructVisualization(_, minX, maxX, positionLoop)).toList
//  visuals.map(_.pipe(println))

  //Part 1
  val insideCounts = List.range(minY, maxY + 1).map(findInsideCount(_, minX, maxX, positionLoop, RayCastingPosition.SWEEPING, isInside = false, runningCountOfInside = 0))
  println(insideCounts.sum + positionLoop.toSet.size) //position loop contains start and end element as same hence converting to set

  @tailrec
  def findInsideCount(rowNum: Int, currentX: Int, maxX: Int, positionLoop: List[Position], currentRay: RayCastingPosition.Value, isInside: Boolean, runningCountOfInside: Int): Int = {
    if (currentX > maxX) {
      runningCountOfInside
    } else {
      val currentChar = if (positionLoop.contains(Position(currentX, rowNum))) '#' else '.'
      currentChar match {
        case '#' =>
          currentRay match {
            case RayCastingPosition.SWEEPING =>
              val edgeFromBottom = isEdgeFromBottom(rowNum, currentX, positionLoop)
              if (edgeFromBottom)
                findInsideCount(rowNum, currentX + 1, maxX, positionLoop, RayCastingPosition.APPROACHED_EDGE_BOTTOM, isInside, runningCountOfInside)
              else
                findInsideCount(rowNum, currentX + 1, maxX, positionLoop, RayCastingPosition.APPROACHED_EDGE_TOP, isInside, runningCountOfInside)
            case RayCastingPosition.APPROACHED_EDGE_BOTTOM => findInsideCount(rowNum, currentX + 1, maxX, positionLoop, RayCastingPosition.ON_EDGE_FROM_BOTTOM, isInside, runningCountOfInside)
            case RayCastingPosition.APPROACHED_EDGE_TOP => findInsideCount(rowNum, currentX + 1, maxX, positionLoop, RayCastingPosition.ON_EDGE_FROM_TOP, isInside, runningCountOfInside)
            case RayCastingPosition.ON_EDGE_FROM_TOP => findInsideCount(rowNum, currentX + 1, maxX, positionLoop, RayCastingPosition.ON_EDGE_FROM_TOP, isInside, runningCountOfInside)
            case RayCastingPosition.ON_EDGE_FROM_BOTTOM => findInsideCount(rowNum, currentX + 1, maxX, positionLoop, RayCastingPosition.ON_EDGE_FROM_BOTTOM, isInside, runningCountOfInside)
          }
        case '.' =>
          currentRay match {
            case RayCastingPosition.SWEEPING =>
              if (isInside)
                findInsideCount(rowNum, currentX + 1, maxX, positionLoop, RayCastingPosition.SWEEPING, isInside, runningCountOfInside + 1)
              else
                findInsideCount(rowNum, currentX + 1, maxX, positionLoop, RayCastingPosition.SWEEPING, isInside, runningCountOfInside)
            case RayCastingPosition.APPROACHED_EDGE_TOP =>
              if (isInside)
                findInsideCount(rowNum, currentX + 1, maxX, positionLoop, RayCastingPosition.SWEEPING, isInside = false, runningCountOfInside)
              else
                findInsideCount(rowNum, currentX + 1, maxX, positionLoop, RayCastingPosition.SWEEPING, isInside = true, runningCountOfInside + 1)
            case RayCastingPosition.APPROACHED_EDGE_BOTTOM =>
              if (isInside)
                findInsideCount(rowNum, currentX + 1, maxX, positionLoop, RayCastingPosition.SWEEPING, isInside = false, runningCountOfInside)
              else
                findInsideCount(rowNum, currentX + 1, maxX, positionLoop, RayCastingPosition.SWEEPING, isInside = true, runningCountOfInside + 1)
            case RayCastingPosition.ON_EDGE_FROM_TOP =>
              val edgeFromBottom = isEdgeFromBottom(rowNum, currentX - 1, positionLoop)
              if (edgeFromBottom) {
                if (isInside)
                  findInsideCount(rowNum, currentX + 1, maxX, positionLoop, RayCastingPosition.SWEEPING, isInside = false, runningCountOfInside)
                else
                  findInsideCount(rowNum, currentX + 1, maxX, positionLoop, RayCastingPosition.SWEEPING, isInside = true, runningCountOfInside + 1)
              } else {
                if (isInside)
                  findInsideCount(rowNum, currentX + 1, maxX, positionLoop, RayCastingPosition.SWEEPING, isInside = true, runningCountOfInside + 1)
                else
                  findInsideCount(rowNum, currentX + 1, maxX, positionLoop, RayCastingPosition.SWEEPING, isInside = false, runningCountOfInside)
              }
            case RayCastingPosition.ON_EDGE_FROM_BOTTOM =>
              val edgeFromBottom = isEdgeFromBottom(rowNum, currentX - 1, positionLoop)
              if (edgeFromBottom) {
                if (isInside)
                  findInsideCount(rowNum, currentX + 1, maxX, positionLoop, RayCastingPosition.SWEEPING, isInside = true, runningCountOfInside + 1)
                else
                  findInsideCount(rowNum, currentX + 1, maxX, positionLoop, RayCastingPosition.SWEEPING, isInside = false, runningCountOfInside)
              } else {
                if (isInside)
                  findInsideCount(rowNum, currentX + 1, maxX, positionLoop, RayCastingPosition.SWEEPING, isInside = false, runningCountOfInside)
                else
                  findInsideCount(rowNum, currentX + 1, maxX, positionLoop, RayCastingPosition.SWEEPING, isInside = true, runningCountOfInside + 1)
              }
          }
        case _ => throw new IllegalStateException("how did we get here")
      }
    }
  }

  def isEdgeFromBottom(rowNum: Int, currentX: Int, positionLoop: List[Position]): Boolean = {
    positionLoop.contains(Position(currentX, rowNum + 1))
  }

  def constructVisualization(rowNum: Int, minX: Int, maxX: Int, positionLoop: List[Position]) = {
    val rowElements = positionLoop.filter(_.y == rowNum)
    val line = constructString(minX, maxX, rowNum, rowElements, "")
    line
  }

  @tailrec
  def constructString(currentX: Int, maxX: Int, rowNum: Int, rowElements: List[Position], visualLine: String): String = {
    if (currentX > maxX) {
      visualLine
    } else {
      if (rowElements.contains(Position(currentX, rowNum))) {
        constructString(currentX + 1, maxX, rowNum, rowElements, visualLine + "#")
      } else {
        constructString(currentX + 1, maxX, rowNum, rowElements, visualLine + ".")
      }
    }
  }


  def createInstruction(instructionString: String) = {
    val instructions = instructionString.split(" ").toList
    Instruction(instructions.head.trim, instructions.tail.head.trim.toInt, instructions.tail.tail.head.trim)
  }


  @tailrec
  def moveRight(steps: Int, positionList: List[Position]) : List[Position] = {
    steps match {
      case 0 => positionList
      case _ =>
        val lastPosition = positionList.last
        moveRight(steps - 1, positionList :+ Position(lastPosition.x + 1, lastPosition.y))
    }
  }

  @tailrec
  def moveLeft(steps: Int, positionList: List[Position]): List[Position] = {
    steps match {
      case 0 => positionList
      case _ =>
        val lastPosition = positionList.last
        moveLeft(steps - 1, positionList :+ Position(lastPosition.x - 1, lastPosition.y))
    }
  }

  @tailrec
  def moveUp(steps: Int, positionList: List[Position]): List[Position] = {
    steps match {
      case 0 => positionList
      case _ =>
        val lastPosition = positionList.last
        moveUp(steps - 1, positionList :+ Position(lastPosition.x, lastPosition.y - 1))
    }
  }

  @tailrec
  def moveDown(steps: Int, positionList: List[Position]): List[Position] = {
    steps match {
      case 0 => positionList
      case _ =>
        val lastPosition = positionList.last
        moveDown(steps - 1, positionList :+ Position(lastPosition.x, lastPosition.y + 1))
    }
  }

  @tailrec
  def getDigLoop(digPlan: List[Instruction], positionList: List[Position]): List[Position] = {
    digPlan.length match {
      case 0 => positionList
      case _ =>
        val currentPlan = digPlan.head
        val newPositionList = currentPlan.direction match {
          case "R" => moveRight(currentPlan.steps, positionList)
          case "L" => moveLeft(currentPlan.steps, positionList)
          case "U" => moveUp(currentPlan.steps, positionList)
          case "D" => moveDown(currentPlan.steps, positionList)
        }
        getDigLoop(digPlan.tail, newPositionList)
    }
  }
}
case class Instruction(direction: String, steps: Int, colour: String)

object RayCastingPosition extends Enumeration {
  val SWEEPING: RayCastingPosition.Value = Value(0)
  val APPROACHED_EDGE_BOTTOM: RayCastingPosition.Value = Value(1)
  val APPROACHED_EDGE_TOP: RayCastingPosition.Value = Value(2)
  val ON_EDGE_FROM_BOTTOM: RayCastingPosition.Value = Value(3)
  val ON_EDGE_FROM_TOP: RayCastingPosition.Value = Value(4)
}