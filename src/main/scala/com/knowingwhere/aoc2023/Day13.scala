package com.knowingwhere.aoc2023

import com.knowingwhere.aoc2023.util.LinesSplitter

import scala.annotation.tailrec
import scala.io.Source

object Day13 extends App {
  val inputText = Source.fromResource("day13-input.txt").getLines().toList
  val blocks = new LinesSplitter().splitAtBlankLines(inputText)
  val verticalLinesOfSymmetry = blocks.map(findVerticalLineOfSymmetryTryEach).filter(_ != -1)
  val horizontalLinesOfSymmetry = blocks.map(findHorizontalLineOfSymmetryTryEach).filter(_ != -1)

  println(verticalLinesOfSymmetry.sum + horizontalLinesOfSymmetry.map(_ * 100).sum)

  @tailrec
  def getDiffCount(string1: String, string2: String, runningCount: Int): Int = {
    if (string1.length != string2.length) throw new IllegalArgumentException()

    if (string1.isEmpty) {
      runningCount
    } else {
      val char1 = string1.head
      val char2 = string2.head
      if (char1 == char2) {
        getDiffCount(string1.tail, string2.tail, runningCount)
      } else {
        getDiffCount(string1.tail, string2.tail, runningCount + 1)
      }
    }
  }

  @tailrec
  def validMirrorDiffCounter(block: List[String], currentCandidate: Int, matchIndexSmall: Int, matchIndexBig: Int, runningDiffCounter: Int): Boolean = {
    if ((matchIndexSmall < 0) || (matchIndexBig == block.size)) {
      //at termination of the recursion, if the running diff is 0, it means all comparisons are exact mirrors,
      // for part 2 of the problem, the running difference need to be exactly 1 (smudge),
      // so change the following line to compare with 1 instead of 0 to get answer for part 2
      runningDiffCounter == 0
    } else {
      val stringsWithIndex = block.zipWithIndex
      val string1 = stringsWithIndex.filter(_._2 == matchIndexSmall).head._1
      val string2 = stringsWithIndex.filter(_._2 == matchIndexBig).head._1
      val diffCount = getDiffCount(string1, string2, 0)
      validMirrorDiffCounter(block, currentCandidate, matchIndexSmall - 1, matchIndexBig + 1, runningDiffCounter + diffCount)
    }
  }

  @tailrec
  def findLinesOfSymmetry(block: List[String], currentCandidate: Int, accumulatedLines: List[Int]): List[Int] = {
    if (currentCandidate == block.size) {
      accumulatedLines
    } else {
      if (validMirrorDiffCounter(block, currentCandidate, currentCandidate - 1, currentCandidate, 0)) {
        findLinesOfSymmetry(block, currentCandidate + 1, accumulatedLines :+ currentCandidate)
      } else {
        findLinesOfSymmetry(block, currentCandidate + 1, accumulatedLines)
      }
    }
  }


  def findHorizontalLineOfSymmetryTryEach(block: List[String]) = {
    val linesOfSymmetry = findLinesOfSymmetry(block, 1, List.empty)
    if (linesOfSymmetry.isEmpty) {
      -1
    } else {
      if (linesOfSymmetry.size > 1) {
        throw new IllegalStateException()
      } else {
        linesOfSymmetry.head
      }
    }
  }

  def findVerticalLineOfSymmetryTryEach(block: List[String]) = {
    val transposed = block.transpose.map(_.mkString)
    findHorizontalLineOfSymmetryTryEach(transposed)
  }

}