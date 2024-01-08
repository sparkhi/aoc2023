package com.knowingwhere.aoc2023

import scala.annotation.tailrec
import scala.io.Source
import scala.util.chaining.scalaUtilChainingOps

object Day15 extends App {

  val inputs = Source.fromResource("day15-input.txt").getLines().toList.head.split(",").toList
  inputs.map(eachString => generateHash(eachString.trim)).sum.pipe(println)

  val boxes: Map[Int, Box] = List.range(0, 256).map(id => id -> createBox(id)).toMap

  val processedBoxes = processBoxes(inputs, boxes)

  processedBoxes.map(calculateProcessingPower).sum.pipe(println)

  def calculateProcessingPower(mapEntry: (Int, Box)) = {
    val orderedSlots = mapEntry._2.lenses.keys.toList.sorted
    calculateProcessingPowerRecurse(orderedSlots, mapEntry._1, mapEntry._2.lenses, slotPosition = 1, power = 0)
  }

  @tailrec
  def calculateProcessingPowerRecurse(orderedSlots: List[Int], boxId: Int, lenses: Map[Int, Lens], slotPosition: Int, power: Int): Int = {
    orderedSlots.length match {
      case 0 => power
      case _ =>
        val currentfocalLength = lenses(orderedSlots.head).focalLength
        val lensPower = (boxId + 1) * slotPosition * currentfocalLength
        calculateProcessingPowerRecurse(orderedSlots.tail, boxId, lenses, slotPosition + 1, power + lensPower)
    }
  }

  @tailrec
  def processBoxes(remainingInstructions: List[String], currentBoxMap: Map[Int, Box]): Map[Int, Box] = {
    remainingInstructions.length match {
      case 0 => currentBoxMap
      case _ =>
        val newBoxesMap = processItem(remainingInstructions.head, currentBoxMap)
        processBoxes(remainingInstructions.tail, newBoxesMap)
    }
  }


  def processItem(eachItem: String, boxes: Map[Int, Box]): Map[Int, Box] = {
    if (eachItem.contains('=')) {
      val label = eachItem.substring(0, eachItem.indexOf('='))
      val focalLength = eachItem.substring(eachItem.indexOf('=') + 1).toInt
      val boxId = generateHash(label)
      val existingBox = boxes(boxId)
      val newListOfLenses = getNewMapOfLenses(label, focalLength, existingBox.lenses)
      boxes.-(boxId) + (boxId -> Box(boxId, newListOfLenses))

    } else if (eachItem.contains('-')) {
      val label = eachItem.substring(0, eachItem.indexOf('-'))
      val boxId = generateHash(label)
      val existingBox = boxes(boxId)
      val newListOfLenses = removeLens(label, existingBox.lenses)
      boxes.-(boxId) + (boxId -> Box(boxId, newListOfLenses))
    } else {
      throw new IllegalAccessException("how did we get here")
    }
  }

  def getNewMapOfLenses(label: String, focalLength: Int, lenses: Map[Int, Lens]): Map[Int, Lens] = {
    val existing = lenses.filter(_._2.label == label)
    if (existing.isEmpty) {
      val lensPosition = if (lenses.keys.isEmpty) 0 else lenses.keys.max + 1
      lenses + (lensPosition -> Lens(label, focalLength))
    } else {
      val lensPosition = existing.head._1
      lenses.-(lensPosition) + (lensPosition -> Lens(label, focalLength))
    }
  }

  def removeLens(label: String, lenses: Map[Int, Lens]): Map[Int, Lens] = {
    val filteredLenses = lenses.filter(_._2.label == label)
    val lensPosition = if (filteredLenses.isEmpty) -1 else filteredLenses.head._1
    if (lensPosition == -1) lenses else lenses.-(lensPosition)
  }

  private def createBox(index: Int): Box = {
    Box(index, Map.empty[Int, Lens])
  }

  /*
     * The hash generation sequence is
     * Start with a "currentValue" of 0
     * -- Get first letter
     * -- find its ASCII value and add it to currentValue
     * -- Multiply the currentValue by 17
     * -- Divide by 256 to get a remainder
     * -- Pass the remainder as a currentValue for the next letter
     */
  def generateHash(inputString: String) : Int = {
    generateHashRecurse(inputString, 0)
  }

  @tailrec
  def generateHashRecurse(inputString: String, currentValue: Int): Int = {
    inputString.length match {
      case 0 => currentValue
      case _ =>
        val letterValue = inputString.head.toInt
        val added = currentValue + letterValue
        val multiplied = added * 17
        val newCurrentValue = multiplied % 256
        generateHashRecurse(inputString.tail, newCurrentValue)
    }
  }
}

case class Lens(label: String, focalLength: Int)
case class Box (id: Int, lenses: Map[Int, Lens])

