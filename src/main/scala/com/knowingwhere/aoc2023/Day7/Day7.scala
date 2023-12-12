package com.knowingwhere.aoc2023.Day7

import scala.io.Source

object Day7 extends App {
  val lines = Source.fromResource("day7-input.txt").getLines().toList
  val sortedLines = lines.sortBy(eachLine => getHand(eachLine))
  val answer = sortedLines.zipWithIndex.map(entry => entry._1.trim.split(" ").tail.head.toLong * (entry._2 + 1)).sum
  println(answer)

  val sortedLines2 = lines.sortBy(eachLine => getHand2(eachLine))
  val answer2 = sortedLines2.zipWithIndex.map(entry => entry._1.trim.split(" ").tail.head.toLong * (entry._2 + 1)).sum
  println(answer2)

  def getHand(eachLine: String) = {
    val handSequence = eachLine.trim.split(" ").head
    Hand(handSequence)
  }
  def getHand2(eachLine: String) = {
    val handSequence = eachLine.trim.split(" ").head
    Hand2(handSequence)
  }

}