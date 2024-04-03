package com.knowingwhere.aoc2023

import com.knowingwhere.aoc2023.RayDirection.{EASTWEST, NORTHSOUTH, SOUTHNORTH, WESTEAST}
import com.knowingwhere.aoc2023.util.Position

import scala.annotation.tailrec
import scala.io.Source

object Day16 extends App {
  val lines = Source.fromResource("day16-input.txt").getLines().toList
  val grid: List[GridElement] = lines.zipWithIndex.flatMap(eachLineWithIndex => createGridElements(eachLineWithIndex))
  val maxX = grid.map(_.position.x).max
  val maxY = grid.map(_.position.y).max

  val startingRayInstance: List[RayInstance] = List(RayInstance(Position(0, 0), RayDirection.WESTEAST))
  val processed: List[RayInstance] = List.empty
  val enlightened = processRays(startingRayInstance, maxX, maxY, grid, processed)
  //part 1
  println(enlightened.map(_.position).toSet.size)


  //part 2
  // prepare a list of all starting points and run the process on each
  val xRange = Range.inclusive(0, maxX)
  val yRange = Range.inclusive(0, maxY)

  def CreatePoint(x: Int, y: Int,  direction: RayDirectionADT) = {
    RayInstance(Position(x, y), direction)
  }

  val startingPoints: List[RayInstance] = xRange.map(CreatePoint(_, 0, NORTHSOUTH)).toList :::
    xRange.map(CreatePoint(_, maxY, SOUTHNORTH)).toList :::
    yRange.map(CreatePoint(0, _, WESTEAST)).toList :::
    yRange.map(CreatePoint(maxX, _, EASTWEST)).toList


  val maxEnergy = startingPoints.map(beginProcessing(_, maxX, maxY, grid)).max
  println(maxEnergy)

  def beginProcessing(instance: RayInstance, maxX: Int, maxY: Int, grid: List[GridElement]) = {
    val beginList = List(instance)
    val processed: List[RayInstance] = List.empty
    processRays(beginList, maxX, maxY, grid, processed).map(_.position).toSet.size
  }

  def isWithinGrid(rayInstance: RayInstance, maxX: Int, maxY: Int) = {
    rayInstance.position.x >= 0 && rayInstance.position.x <= maxX && rayInstance.position.y >= 0 && rayInstance.position.y <= maxY
  }

  @tailrec
  def processRays(rayInstances: List[RayInstance], maxX: Int, maxY: Int, grid: List[GridElement], processed: List[RayInstance]) : List[RayInstance] = {
    if (rayInstances.isEmpty)
      processed
    else {
      val instance = rayInstances.head
      if (processed.contains(instance)) {
        processRays(rayInstances.tail, maxX, maxY, grid, processed)
      } else {
        val nextInstances: List[RayInstance] = getNextInstances(instance, grid)
        val validInstances = nextInstances.filter(rayInstance => isWithinGrid(rayInstance, maxX, maxY))
        val newProcessed = processed :+ instance
        processRays(rayInstances.tail ::: validInstances, maxX, maxY, grid, newProcessed)
      }
    }
  }

  def getNextInstances(instance: RayInstance, grid: List[GridElement]): List[RayInstance] = {
    val currentElement = grid.find(_.position == instance.position).get
    currentElement.piece.arrangement match {
      case "." =>
        instance.direction match {
          case EASTWEST => List(RayInstance(Position(instance.position.x - 1, instance.position.y), EASTWEST))
          case WESTEAST => List(RayInstance(Position(instance.position.x + 1, instance.position.y), WESTEAST))
          case NORTHSOUTH => List(RayInstance(Position(instance.position.x, instance.position.y + 1), NORTHSOUTH))
          case SOUTHNORTH => List(RayInstance(Position(instance.position.x, instance.position.y - 1), SOUTHNORTH))
        }
      case "-" =>
        instance.direction match {
          case EASTWEST => List(RayInstance(Position(instance.position.x - 1, instance.position.y), EASTWEST))
          case WESTEAST => List(RayInstance(Position(instance.position.x + 1, instance.position.y), WESTEAST))
          case NORTHSOUTH => List(
            RayInstance(Position(instance.position.x - 1, instance.position.y), EASTWEST),
            RayInstance(Position(instance.position.x + 1, instance.position.y), WESTEAST))
          case SOUTHNORTH => List(
            RayInstance(Position(instance.position.x - 1, instance.position.y), EASTWEST),
            RayInstance(Position(instance.position.x + 1, instance.position.y), WESTEAST))
        }
      case "/" =>
        instance.direction match {
          case EASTWEST => List(RayInstance(Position(instance.position.x, instance.position.y + 1), NORTHSOUTH))
          case WESTEAST => List(RayInstance(Position(instance.position.x, instance.position.y - 1), SOUTHNORTH))
          case NORTHSOUTH => List(RayInstance(Position(instance.position.x - 1, instance.position.y), EASTWEST))
          case SOUTHNORTH => List(RayInstance(Position(instance.position.x + 1, instance.position.y), WESTEAST))
        }
      case "\\" =>
        instance.direction match {
          case EASTWEST => List(RayInstance(Position(instance.position.x, instance.position.y - 1), SOUTHNORTH))
          case WESTEAST => List(RayInstance(Position(instance.position.x, instance.position.y + 1), NORTHSOUTH))
          case NORTHSOUTH => List(RayInstance(Position(instance.position.x + 1, instance.position.y), WESTEAST))
          case SOUTHNORTH => List(RayInstance(Position(instance.position.x - 1, instance.position.y), EASTWEST))
        }
      case "|" =>
        instance.direction match {
          case EASTWEST => List(
            RayInstance(Position(instance.position.x, instance.position.y + 1), NORTHSOUTH),
            RayInstance(Position(instance.position.x, instance.position.y - 1), SOUTHNORTH))
          case WESTEAST => List(
            RayInstance(Position(instance.position.x, instance.position.y + 1), NORTHSOUTH),
            RayInstance(Position(instance.position.x, instance.position.y - 1), SOUTHNORTH))
          case NORTHSOUTH => List(RayInstance(Position(instance.position.x, instance.position.y + 1), NORTHSOUTH))
          case SOUTHNORTH => List(RayInstance(Position(instance.position.x, instance.position.y - 1), SOUTHNORTH))
        }
      case _ => throw new RuntimeException("how did we get here")
    }
  }

  def createGridElements(eachLineWithIndex: (String, Int)) = {
    eachLineWithIndex._1.toList.zipWithIndex.map(eachChar => createGridElement(eachChar._1.toString, eachChar._2, eachLineWithIndex._2))
  }

  def createGridElement(arrangement: String, xPosition: Int, yPosition: Int): GridElement = {
    GridElement(Position(xPosition, yPosition), ContraptionPiece(arrangement))
  }


}

/*
 * Various data structures related to the grid and ray passing over it are given below
 */

/**
 * Represents one element of the grid. An element comprises a position and a piece of contraption
 * at that position
 * @param position location on the grid
 * @param piece Contraption piece at that location
 */
case class GridElement(position: Position, piece: ContraptionPiece)

/**
 * Represents a piece from the contraption. A piece is any one of ". - | \ /"
 * @param arrangement The arrangement of the contraption
 */
case class ContraptionPiece(arrangement: String)

/**
 * Instance of a ray at any one point in time. At any given time, the ray is at a specific
 * position and travelling in a specific direction
 * @param position Position where the ray is present at this moment
 * @param direction direction in which the ray is travelling
 */
case class RayInstance(position: Position, direction: RayDirectionADT)
abstract class RayDirectionADT(val name: String)

/**
 * Enumeration representing direction of travel for the ray of light
 */
object RayDirection {
  case object EASTWEST extends RayDirectionADT("East-West")
  case object NORTHSOUTH extends RayDirectionADT("North-South")
  case object WESTEAST extends RayDirectionADT("West-East")
  case object SOUTHNORTH extends RayDirectionADT("South-North")
}