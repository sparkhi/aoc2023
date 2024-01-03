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
  def isValidMirror(block: List[String], currentCandidate: Int, matchIndexSmall: Int, matchIndexBig: Int, isValidSoFar: Boolean): Boolean = {
    if ((matchIndexSmall < 0) || (matchIndexBig == block.size)) {
      isValidSoFar
    } else {
      if (!isValidSoFar) {
        isValidSoFar
      } else {
        val stringsWithIndex = block.zipWithIndex
        val string1 = stringsWithIndex.filter(_._2 == matchIndexSmall).head._1
        val string2 = stringsWithIndex.filter(_._2 == matchIndexBig).head._1
        if (string1 == string2) {
          isValidMirror(block, currentCandidate, matchIndexSmall - 1, matchIndexBig + 1, isValidSoFar)
        } else {
          isValidMirror(block, currentCandidate, matchIndexSmall - 1, matchIndexBig + 1, isValidSoFar = false)
        }
      }
    }
  }
  @tailrec
  def findLinesOfSymmetry(block: List[String], currentCandidate: Int, accumulatedLines: List[Int]): List[Int] = {
    if (currentCandidate == block.size) {
      accumulatedLines
    } else {
      if (isValidMirror(block, currentCandidate, currentCandidate - 1, currentCandidate, isValidSoFar = true)) {
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