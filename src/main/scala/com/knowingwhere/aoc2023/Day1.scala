package com.knowingwhere.aoc2023

import scala.io.Source


object Day1 extends App {
  val calibrationLines = Source.fromResource("day1-input.txt").getLines().toList

  //part 1
  val numbers = calibrationLines.map(eachLine => extractNumber(eachLine))
  println(numbers.sum)


  //part 2
  val linesWithAllNumberConvertedToDigits = calibrationLines.map(eachLine => generateEquivalentDigits(eachLine))
  val numbers2 = linesWithAllNumberConvertedToDigits.map(eachLine => extractNumber(eachLine))
  println(numbers2.sum)

  def extractNumber(eachLine: String): Int = {
    val digit1 = eachLine.filter(_.isDigit).head
    val digit2 = eachLine.reverse.filter(_.isDigit).head
    s"$digit1$digit2".toInt
  }

  /**
   * Some numbers overlap in their first and last letter so a direct replacement could mangle the following or preceeding
   * number, so the replacement need to take care of this. The following list shows the overlaps and potential replacement
   * e.g. (as zero ends in 0 and eight begins with e, we need to preserve those 2 characters)
   * "one - zero, eight"                      => o1e
   * "two" - eight                            => t2
   * "three" - eight, eight                   => t3e
   * "four" -                                 => 4
   * "five" - eight                           => 5e
   * "six"                                    => 6
   * "seven" - nine                           => 7n
   * "eight" - three, five, nine, two, three  => e8t
   * "nine" - eight                           => 9e
   * "zero" - one                             => 0o
   *
   * @param eachLine line containing the digits and worded numbers
   * @return string with worded number replaced by corresponding digits
   */
  def generateEquivalentDigits(eachLine: String): String = {
    eachLine.replace("one", "o1e")
      .replace("two", "t2")
      .replace("three", "t3e")
      .replace("four", "4")
      .replace("five", "5e")
      .replace("six", "6")
      .replace("seven", "7n")
      .replace("eight", "e8t")
      .replace("nine", "9e")
      .replace("zero", "0o")
  }
}
