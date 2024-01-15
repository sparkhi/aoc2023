package com.knowingwhere.aoc2023

import com.knowingwhere.aoc2023.util.{BigPosition, Position, StringUtil}

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
  //val visuals = List.range(minY, maxY + 1).map(constructVisualization(_, minX, maxX, positionLoop)).toList
  //visuals.map(_.pipe(println))

  //Part 1 Good algorithm for "point in polygon", but not efficient to find area of a 2D shape
  // After implementing Part 2, this implementation of part 1 is very inefficient, however leaving this in here for reference
  val insideCounts = List.range(minY, maxY + 1).map(findInsideCount(_, minX, maxX, positionLoop, RayCastingPosition.SWEEPING, isInside = false, runningCountOfInside = 0))
  println(insideCounts.sum + positionLoop.toSet.size) //position loop contains start and end element as same hence converting to set


  //part 2
  // It was quite clear that the part 1 algorithm won't work efficiently for part 2. Going back to the GIS and studying area of 2D shape based on
  // vector cross product, it appears to be the best way to solve part 2. After studying the vectors and cross product concept, the practically
  // helpful page ended up being this one ==> https://www.101computing.net/the-shoelace-algorithm/
  // However, the above calculates the area but we need to include the trenches at the border too. This was quite difficult to figure out so
  //
  //   . . . .   Consider the simple picture on the left, we start at top left and move 3R, 2D, 3L and 2U. so the coordinates are
  //   . . . .   (0,0), (3,0), (3,2), (0,2), (0,0) Area from shoelace = 6. However for our purpose, each point is 1 sqm and we actually need a count
  //   . . . .   of all points. If we use pick's theorem for area => (count of internal points + 1/2 (count of points on boundary) - 1
  //             From these two, we can get hold of the number of internal points. Adding the perimeter to it should give us a count of all points
  //             which will be our answer.

  val digPlan2 = Source.fromResource("day18-input.txt").getLines().map(createInstruction2).toList
  val vertices = getVerticesOfPolygon(digPlan2, List(BigPosition(0,0)))
  val coverage = calculateCoverage(vertices)

  println(coverage)

  def calculateCoverage(vertices: List[BigPosition]): BigInt = {
    val perimeter = vertices.sliding(2).map(eachWindow => getStraightLineDistance(eachWindow)).sum
    val area = vertices.sliding(2).map(eachWindow => calculateArea(eachWindow)).sum / 2
    val internalPoints = area + 1 - (perimeter / 2)
    perimeter + internalPoints
  }

  def getStraightLineDistance(eachWindow: List[BigPosition]) = {
    val one = eachWindow.head
    val two = eachWindow.tail.head
    if (one.x == two.x) {
      (one.y - two.y).abs
    } else {
      (one.x - two.x).abs
    }
  }

  def calculateArea(eachWindow: List[BigPosition]): BigInt = {
    val one = eachWindow.head
    val two = eachWindow.tail.head
    (one.x * two.y) - (one.y * two.x)
  }


  @tailrec
  def getVerticesOfPolygon(digPlan2: List[BigInstruction], vertices: List[BigPosition]) : List[BigPosition] = {
    digPlan2.length match {
      case 0 => vertices
      case _ =>
        val currentPlan = digPlan2.head
        val lastPoint = vertices.last
        currentPlan.direction match {
          case "R" => getVerticesOfPolygon(digPlan2.tail, vertices :+ BigPosition(lastPoint.x + currentPlan.steps, lastPoint.y))
          case "L" => getVerticesOfPolygon(digPlan2.tail, vertices :+ BigPosition(lastPoint.x - currentPlan.steps, lastPoint.y))
          case "U" => getVerticesOfPolygon(digPlan2.tail, vertices :+ BigPosition(lastPoint.x, lastPoint.y - currentPlan.steps))
          case "D" => getVerticesOfPolygon(digPlan2.tail, vertices :+ BigPosition(lastPoint.x, lastPoint.y + currentPlan.steps))
        }
    }
  }

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
    Instruction(instructions.head.trim, instructions.tail.head.trim.toInt)
  }

  def createInstruction2(instructionString: String) = {
    val soCalledColourString = StringUtil.trimTrailing(instructionString.split(" \\(#").tail.head, ')')
    val hexValue = BigInt(soCalledColourString.substring(0, soCalledColourString.length - 1), 16)
    val directionValue = soCalledColourString.substring(soCalledColourString.length - 1)

    directionValue match {
      case "0" => BigInstruction("R", hexValue)
      case "1" => BigInstruction("D", hexValue)
      case "2" => BigInstruction("L", hexValue)
      case "3" => BigInstruction("U", hexValue)
    }
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
case class Instruction(direction: String, steps: Int)
case class BigInstruction(direction: String, steps: BigInt)


object RayCastingPosition extends Enumeration {
  val SWEEPING: RayCastingPosition.Value = Value(0)
  val APPROACHED_EDGE_BOTTOM: RayCastingPosition.Value = Value(1)
  val APPROACHED_EDGE_TOP: RayCastingPosition.Value = Value(2)
  val ON_EDGE_FROM_BOTTOM: RayCastingPosition.Value = Value(3)
  val ON_EDGE_FROM_TOP: RayCastingPosition.Value = Value(4)
}