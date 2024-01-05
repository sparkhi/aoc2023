package com.knowingwhere.aoc2023

import com.knowingwhere.aoc2023.Day14.rollStonesToWest

import scala.annotation.tailrec
import scala.io.Source

object Day14 extends App {
  val originalGrid = Source.fromResource("day14-input.txt").getLines().toList

  //part 1
  val rolled = rollStonesToNorth(originalGrid)
  println(calculateLoadOnNorth(rolled, 0))

  //part 2
  runCycle(originalGrid, List(0 -> 0))

  // cheating: (ONLY TRUE FOR MY INPUT) Ran the simulation for 2000 cycles, looked at the values and noticed a cyclic
  // pattern of 34 values, which means every 34th value is same once we hit the cyclic pattern. Although we could look
  // at programmatically finding the cycle, its not implemented yet. instead,
  // here's what I did ((1000000000 - 2000) % 34) = 30. It means the 1000000000 element would be the same as 30th
  // element in our cycle. Noted that manually and answered on website


  @tailrec
  def calculateLoadOnNorth(remainingLines: List[String], runningSumOfLoad: Int): Int = {
    remainingLines.length match {
      case 0 => runningSumOfLoad
      case _ =>
        val lineLoad = remainingLines.head.chars().filter(_ == 'O').count.toInt * remainingLines.length
        calculateLoadOnNorth(remainingLines.tail, runningSumOfLoad + lineLoad)
    }
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

  @tailrec
  def rollStonesToLeft(source: String) : String = {
    val replaced = source.replace(".O", "O.")
    if (source == replaced) {
      replaced
    } else {
      rollStonesToLeft(replaced)
    }
  }

  @tailrec
  def rollStonesToRight(source: String): String = {
    val replaced = source.replace("O.", ".O")
    if (source == replaced) {
      replaced
    } else {
      rollStonesToRight(replaced)
    }
  }

  def rollStonesToWest(grid: List[String]) : List[String] = {
    // roll to left
    grid.map(rollStonesToLeft)
  }

  def rollStonesToEast(grid: List[String]): List[String] = {
    // roll to right
    grid.map(rollStonesToRight)
  }

  def rollStonesToNorth(grid: List[String]): List[String] = {
    //transpose, roll to left, transpose back and return grid
    grid.transpose.map(_.mkString).map(rollStonesToLeft).transpose.map(_.mkString)
  }

  def rollStonesToSouth(grid: List[String]): List[String] = {
    //transpose, roll to right, transpose back and return grid
    grid.transpose.map(_.mkString).map(rollStonesToRight).transpose.map(_.mkString)
  }

  @tailrec
  def runCycle(grid: List[String], lastFew: List[(Int, Int)]): Unit = {
    if (lastFew.size > 2000) {
      println("done")
    } else {
      val n = rollStonesToNorth(grid)
      val w = rollStonesToWest(n)
      val s = rollStonesToSouth(w)
      val e = rollStonesToEast(s)
      val northLoad = calculateLoadOnNorth(e, 0)
      // uncomment following line to see the output of first 2000 cycles and find out a repeating pattern
//      println( " " + (lastFew.last._1 + 1 -> northLoad))
      runCycle(e, lastFew :+ (lastFew.last._1 + 1 -> northLoad))
    }
  }

}
