package com.knowingwhere.aoc2023

import org.scalatest.Matchers._
import org.scalatest._

import scala.io.Source
class Day4Test extends WordSpec with BeforeAndAfterEach {
  "Day 4" should {
    val lines = Source.fromResource("day4-test-input.txt").getLines().toList
    "create correct scratchcard " in {
      val scratchcard = Day4.createScratchcard("Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1")
      scratchcard.Id shouldBe 3
      scratchcard.winningNumbers should contain allElementsOf List(1, 21, 53, 59, 44)
      scratchcard.chosenNumbers should contain allElementsOf List(69, 82, 63, 72, 16, 21, 14,  1)
      scratchcard.score shouldBe 2

      Day4.createScratchcard("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53").score shouldBe 8
      Day4.createScratchcard("Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83").score shouldBe 1
      Day4.createScratchcard("Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11").score shouldBe 0
    }

    "part 2 should increment the scratch card numbers" in {
      val scratchcards = Day4.getScratchcards(lines)
      val scratchcardCountMap = collection.mutable.Map(scratchcards.map(_ -> 1): _*)
      val answer = Day4.processScratchCards(scratchcardCountMap, 1)
      println(answer.values.toList.sum)
    }
  }
}
