package com.knowingwhere.aoc2023

import scala.annotation.tailrec
import scala.io.Source

object Day9 extends App {
  val sequences = Source.fromResource("day9-input.txt").getLines().toList.map(eachline => getIntList(eachline))

  //part 1
  println(sequences.map(findNextNumberOfSequence).sum)


  //part2 - just reverse the sequences and find "next number"
  val reversed = sequences.map(_.reverse)
  println(reversed.map(findNextNumberOfSequence).sum)

  private def getIntList(eachline: String) = {
    eachline.trim.split(" ").filter(_.nonEmpty).map(_.toInt).toList
  }

  def findNextNumberOfSequence(someSeq: List[Int]) = {
    findNextNumberOfSequenceRecurse(someSeq, someSeq.last)
  }

  @tailrec
  private def findNextNumberOfSequenceRecurse(someSeq: List[Int], runningTotal: Int) : Int = {
    val nextSequence = someSeq.sliding(2).map { case List(a, b) => b - a }.toList
    if ((nextSequence.sum == 0) && (nextSequence.distinct.size == 1)) {
      runningTotal
    } else {
      findNextNumberOfSequenceRecurse(nextSequence, runningTotal + nextSequence.last)
    }
  }
}

