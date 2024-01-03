package com.knowingwhere.aoc2023

import com.knowingwhere.aoc2023.util.StringUtil

object Day12 extends App {
  lazy val ruleList: List[(String, List[Int]) => Option[Int]] = List(ruleOfExactCount, ruleOfSplitOnDotToMatchExactPattern)

  def getArrangementVariationsCount(line: String): Int = {
    val springPattern = line.split(" ").head
    val counts = line.split(" ").tail.head.trim.split(",").map(_.trim.toInt).toList

    val result = applyAllRules(springPattern, counts)
    val thingy: Int = result.getOrElse(-1)
    thingy
  }

  /*
    e.g. "???.### 1,1,3" => the combination of 1 1 3 can be distributed over minimum of 7 spaces
    as the length of spring pattern is 7, there can be only 1 variation
   */
  def ruleOfExactCount(springPattern: String, counts: List[Int]): Option[Int] = {
    if (springPattern.length == counts.sum + counts.size - 1) {
      Some(1)
    } else {
      Some(-1)
    }
  }

  /*
    e.g. "????.#...#... 4,1,1" => Once we separate the pattern on dots, we should simply get
    a combination of 4 1 1 which matches the counts, so only one variation possible
   */
  def ruleOfSplitOnDotToMatchExactPattern(springPattern: String, counts: List[Int]): Option[Int] = {
    val patternLengths = springPattern.split("\\.+").map(_.length).toList
    if (patternLengths.equals(counts)) {
      Some(1)
    } else {
      Some(-1)
    }
  }

  def ruleOfTryingOutAllPossibilitiesRecursively(springPattern: String, counts: List[Int]) : Option[Int] = {
    val trimmedPattern = StringUtil.trimLeading(springPattern, '.')
    //is there any # within the first set
    val firstSet = counts.head
    if (trimmedPattern.substring(0, firstSet).contains('#')){
      println("found #")
    } else{
      println("no hashes")
    }
    Some(-1)
  }

  def applyAllRules(springPattern: String, counts: List[Int]): Option[Int] = {
    for {
      rule <- ruleList
      result <- rule(springPattern, counts)
      if result > 0
    } yield result
  }.headOption
}
