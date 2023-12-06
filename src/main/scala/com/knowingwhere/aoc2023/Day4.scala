package com.knowingwhere.aoc2023

import java.util
import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day4 extends App{

  val lines = Source.fromResource("day4-input.txt").getLines().toList
  val scratchcards = getScratchcards(lines)

  //part1
  println(scratchcards.map(_.score).sum)

  //part 2 - find total number of cards
  val scratchcardCountMap = collection.mutable.Map(scratchcards.map(_ -> 1): _*)
  processScratchCards(scratchcardCountMap, 1)

  println(scratchcardCountMap.values.toList.sum)

  //FIXME - "Unit" - not happy with it but for now its okay
  @tailrec
  def IncrementCardCountForNext(scratchcardCountMap: mutable.Map[Scratchcard, Int], currentCardId: Int, score: Int, currentCardCopiesCount: Int): Unit = {
    val currentCardOption = scratchcardCountMap.keySet.find(_.Id == currentCardId)
    if (score > 0 && currentCardOption.isDefined) {
      val currentCard = currentCardOption.get
      scratchcardCountMap(currentCard) += currentCardCopiesCount
      IncrementCardCountForNext(scratchcardCountMap, currentCardId + 1, score - 1, currentCardCopiesCount)
    }
  }

  @tailrec
  def processScratchCards(scratchcardCountMap: mutable.Map[Scratchcard, Int], currentCardId: Int): mutable.Map[Scratchcard, Int] = {
    val currentCardOption = scratchcardCountMap.keySet.find(_.Id == currentCardId)
    if (currentCardOption.isDefined) {
      val currentCard = currentCardOption.get
      val score = currentCard.matchingNumberCount
      val currentCardCopiesCount = scratchcardCountMap(currentCard)
      IncrementCardCountForNext(scratchcardCountMap, currentCardId + 1, score, currentCardCopiesCount)
      processScratchCards(scratchcardCountMap, currentCardId + 1)
    } else {
      scratchcardCountMap
    }
  }


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
