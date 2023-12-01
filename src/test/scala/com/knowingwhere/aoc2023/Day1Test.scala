package com.knowingwhere.aoc2023

import org.scalatest.Matchers._
import org.scalatest._

class Day1Test extends WordSpec with BeforeAndAfterEach {
  "Day 1 " should {
    "give correct priority value for code" in {
      Day1.extractNumber("1abc2") shouldBe 12
      Day1.extractNumber("sdjfkhs1abc2fgdgfd") shouldBe 12
      Day1.extractNumber("OnlyOneDigitInThi5LongString") shouldBe 55
    }

    "give correct replacement value for worded string" in {
      Day1.generateEquivalentDigits("two1") shouldBe "t21"
      Day1.generateEquivalentDigits("zeroeighttwo1") shouldBe "0oe8tt21"

      Day1.extractNumber(Day1.generateEquivalentDigits("eightzeroeighttwo1")) shouldBe 81
      Day1.extractNumber(Day1.generateEquivalentDigits("5tck83cseven9nine")) shouldBe 59
      Day1.extractNumber(Day1.generateEquivalentDigits("2shhzch")) shouldBe 22
      Day1.extractNumber(Day1.generateEquivalentDigits("onlyonedigitinthi5longstring")) shouldBe 15
    }
  }
}
