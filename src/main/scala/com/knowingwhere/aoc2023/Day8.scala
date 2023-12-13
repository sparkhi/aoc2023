package com.knowingwhere.aoc2023

import com.knowingwhere.aoc2023.util.{LinesSplitter, MathsUtil}

import scala.annotation.tailrec
import scala.io.Source

object Day8 extends App {
  val lines = Source.fromResource("day8-input.txt").getLines().toList
  val groups = new LinesSplitter().splitAtBlankLines(lines)

  val cycleString = groups.head.head
  val navigationMap = groups.tail.head.map(eachLine => getMapEntry(eachLine)).toMap

  //part 1
  val stepCount = countSteps("AAA", navigationMap, cycleString, equalsPredicate, 0)
  println(stepCount)

  //part 2 - multiple paths. get all the paths then figure out where the cycles meet
  val startPositions = navigationMap.keys.filter(_.endsWith("A")).toList
  val steps= startPositions.map(startKey => countSteps(startKey, navigationMap, cycleString, endsWithPredicate, 0))

  // the different cycles will meet at the LCM of all cycle counts
    val lcm = findLcm(steps, BigInt(1))
  println(lcm)

  @tailrec
  def findLcm(steps: List[BigInt], accLCM: BigInt): BigInt = {
    if (steps.isEmpty)
      accLCM
    else {
      val stepLCM = MathsUtil.lcm(steps.head, accLCM)
      findLcm(steps.tail, stepLCM)
    }

  }


  def equalsPredicate: String => Boolean = param => param == "ZZZ"
  def endsWithPredicate: String => Boolean = param => param.endsWith("Z")

  @tailrec
  def countSteps(currentKey: String, navigationMap: Map[String, (String, String)], currentCycle: String, somePred: String => Boolean,  stepsTraversed: BigInt):BigInt = {
    val totalSteps = stepsTraversed + 1
    val currentDirection = currentCycle.head
    val nextKey = currentDirection match {
      case 'L' => navigationMap(currentKey)._1
      case 'R' => navigationMap(currentKey)._2

    }
    if (somePred(nextKey)) {
      totalSteps
    } else {
      val newCycle = currentCycle.tail + currentCycle.head
      countSteps(nextKey, navigationMap, newCycle, somePred, totalSteps)
    }
  }

  def getMapEntry(eachLine: String) = {
    val mapKey = eachLine.split("=").head.trim
    val mapValString = eachLine.split("=").tail.head.trim.substring(1, 9)
    val mapVal = mapValString.split(",").head.trim -> mapValString.split(",").tail.head.trim
    mapKey -> mapVal
  }

}
