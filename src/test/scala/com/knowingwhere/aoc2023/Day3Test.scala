package com.knowingwhere.aoc2023

import com.knowingwhere.aoc2023.util.Position
import org.scalatest.Matchers._
import org.scalatest._

import scala.io.Source

class Day3Test extends WordSpec with BeforeAndAfterEach  {
  "Day 3" should  {
    val lines = Source.fromResource("day3-test-input.txt").getLines().toList
    val position = Day3.getSymbolPositions(lines)
    val symbolCoverage = Day3.getPositionCoverage(position)
    "have correct number of symbols  " in {
      position.size shouldBe 6
    }

    "have correct locations for the symbols" in {
      position.contains(Position(1,3)) shouldBe true
      position.contains(Position(4,2)) shouldBe false
      position.contains(Position(4,3)) shouldBe true
    }

    "have correct coverage size" in {
      symbolCoverage.size shouldBe 47
    }

    "get correct number" in {
      val numbersWithPos = Day3.getNumbersWithPositionInGrid("467..114..", 0)
      numbersWithPos.size shouldBe 2
      numbersWithPos.head.theValue shouldBe 467
      numbersWithPos.head.coverage should contain allElementsOf Set(Position(0,0), Position(0,1), Position(0,2))
    }

    "get correct number and positions " in {
      val numbersWithPos = Day3.getNumbersInGridWithPositions(lines)
      numbersWithPos.size shouldBe 10
    }
  }
}
