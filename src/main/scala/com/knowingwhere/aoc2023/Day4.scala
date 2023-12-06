package com.knowingwhere.aoc2023

import scala.io.Source

object Day4 extends App{

  val lines = Source.fromResource("day4-input.txt").getLines().toList
  val scratchcards = getScratchcards(lines)

  //part1
  println(scratchcards.map(_.score).sum)

  def createScratchcard(cardline: String):Scratchcard = {
    val idAndNumbers = cardline.split(":")
    val id = idAndNumbers.head.substring("Card ".length).trim.toInt
    val numbers = idAndNumbers.tail.head.split("\\|")
    val winningNumbers = ("""\d+""".r findAllIn numbers.head).map(_.toInt).toList
    val chosenNumbers = ("""\d+""".r findAllIn numbers.tail.head).map(_.toInt).toList
    Scratchcard(id, winningNumbers, chosenNumbers)
  }

  def getScratchcards(lines: List[String]) = {
    val scratchcards = lines.map(createScratchcard)
    scratchcards
  }
}

case class Scratchcard (Id: Int, winningNumbers: List[Int], chosenNumbers: List[Int]) {
  def matchingNumberCount: Int = {
    winningNumbers.intersect(chosenNumbers).size
  }

  def score: Int = {
    if (matchingNumberCount == 0)
      0
    else
      Math.pow(2, matchingNumberCount - 1).toInt
  }
}
