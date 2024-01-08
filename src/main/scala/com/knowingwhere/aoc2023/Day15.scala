package com.knowingwhere.aoc2023

import scala.annotation.tailrec
import scala.io.Source

object Day15 extends App {

  val inputs = Source.fromResource("day15-input.txt").getLines().toList.head.split(",").toList
  println(inputs.map(eachString => generateHash(eachString.trim)).sum)


  /*
     * The hash generation sequence is
     * Start with a "currentValue" of 0
     * -- Get first letter
     * -- find its ASCII value and add it to currentValue
     * -- Multiply the currentValue by 17
     * -- Divide by 256 to get a remainder
     * -- Pass the remainder as a currentValue for the next letter
     */
  def generateHash(inputString: String) : Int = {
    generateHashRecurse(inputString, 0)
  }

  @tailrec
  def generateHashRecurse(inputString: String, currentValue: Int): Int = {
    inputString.length match {
      case 0 => currentValue
      case _ =>
        val letterValue = inputString.head.toInt
        val added = currentValue + letterValue
        val multiplied = added * 17
        val newCurrentValue = multiplied % 256
        generateHashRecurse(inputString.tail, newCurrentValue)
    }
  }

}
