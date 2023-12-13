package com.knowingwhere.aoc2023

import com.knowingwhere.aoc2023.util.LinesSplitter

import scala.annotation.tailrec
import scala.io.Source

object Day8 extends App {
  val lines = Source.fromResource("day8-input.txt").getLines().toList
  val groups = new LinesSplitter().splitAtBlankLines(lines)

  val cycleString = groups.head.head
  val navigationMap = groups.tail.head.map(eachLine => getMapEntry(eachLine)).toMap

  val stepCount = countSteps("AAA", navigationMap, cycleString, 0)
  @tailrec
  def countSteps(currentKey: String, navigationMap: Map[String, (String, String)], currentCycle: String, stepsTraversed: Int):Int = {
    val totalSteps = stepsTraversed + 1
    val currentDirection = currentCycle.head
    val nextKey = currentDirection match {
      case 'L' => navigationMap(currentKey)._1
      case 'R' => navigationMap(currentKey)._2

    }
    if (nextKey == "ZZZ") {
      totalSteps
    } else {
      val newCycle = currentCycle.tail + currentCycle.head
      countSteps(nextKey, navigationMap, newCycle, totalSteps)
    }
  }

  def getMapEntry(eachLine: String) = {
    val mapKey = eachLine.split("=").head.trim
    val mapValString = eachLine.split("=").tail.head.trim.substring(1, 9)
    val mapVal = mapValString.split(",").head.trim -> mapValString.split(",").tail.head.trim
    mapKey -> mapVal
  }

  println(stepCount)
}
