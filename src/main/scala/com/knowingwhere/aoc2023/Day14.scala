package com.knowingwhere.aoc2023

import scala.annotation.tailrec
import scala.io.Source

object Day14 extends App {
  val grid = Source.fromResource("day14-input.txt").getLines().toList.transpose.map(_.mkString)

  //part 1
  println(grid.map(calculateLoad).sum)

  def calculateLoad(oneRow: String) = {
    calculateLoadRecurse(oneRow, oneRow.length, 0)
  }

  /* Recursively calculate the load without actually moving anything by simply using the "potential values if the
   * stone was rolled. Quick and easy
   */
  @tailrec
  private def calculateLoadRecurse(remainingRow: String, currentLoadValue: Int, runningTotalOfLoad: Int): Int  = {
    remainingRow.length match {
      case 0 => runningTotalOfLoad
      case _ =>
        remainingRow.head match {
          case '.' => calculateLoadRecurse(remainingRow.tail, currentLoadValue, runningTotalOfLoad)
          case 'O' => calculateLoadRecurse(remainingRow.tail, currentLoadValue - 1, runningTotalOfLoad + currentLoadValue)
          case '#' => calculateLoadRecurse(remainingRow.tail, remainingRow.tail.length, runningTotalOfLoad)
        }
    }
  }
}
