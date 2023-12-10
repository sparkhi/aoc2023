package com.knowingwhere.aoc2023

import scala.io.Source

object Day6 extends App{
  val lines = Source.fromResource("day6-input.txt").getLines().toList
  val times = lines.head.substring("Time:".length).trim.split(" ").filter(!_.isBlank).map(_.toLong).toList
  val distance = lines.tail.head.substring("Distance:".length).trim.split(" ").filter(!_.isBlank).map(_.toLong).toList
  val timesDistanceMap = (times zip distance).toMap

  //part 1
  val betterDistances = timesDistanceMap.map(entry => findBetterDistanceCount(entry)).toList
  println(betterDistances.product)

  //part 2 - brute force option slow but works
  // Possible optimization would be to start from the beginning to find starting index of more wins. Then start at end
  // and work backwards to find end index of more wins, the difference will be count of winning ways
  //NOTE: Ideally, use the quadratic formula to find 2 indices where the distance travelled equals the current record
  val times2 = lines.head.substring("Time:".length).trim.split(" ").filter(!_.isBlank).mkString.toLong
  val distance2 = lines.tail.head.substring("Distance:".length).trim.split(" ").filter(!_.isBlank).mkString.toLong
  val betterDistances2 = findBetterDistanceCount(times2, distance2)
  println(betterDistances2)

  def findBetterDistanceCount(entry: (Long, Long)): Long = {
    findBetterDistanceCount(entry._1, entry._2)
  }

  def findBetterDistanceCount(totalTime: Long, currentRecord: Long): Long = {
    val range = (1L to totalTime).toList
    val distances = range.map(powerUpTime => findDistanceForEachVal(powerUpTime, totalTime - powerUpTime))

    distances.count(_ > currentRecord)
  }

  def findDistanceForEachVal(powerUpTime: Long, runningTime: Long): Long = {
    powerUpTime * runningTime
  }

}
