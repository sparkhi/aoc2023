package com.knowingwhere.aoc2023

import org.scalatest.Matchers._
import org.scalatest.{BeforeAndAfterEach, WordSpec}

class Day9Test extends WordSpec with BeforeAndAfterEach {
  "thingy " should {
    "thingify " in {
      Day9.findNextNumberOfSequence(List(0, 3, 6, 9, 12, 15)) shouldBe 18
      Day9.findNextNumberOfSequence(List(1,   3,   6,  10,  15,  21)) shouldBe 28
      Day9.findNextNumberOfSequence(List(10,  13,  16,  21,  30,  45)) shouldBe 68

      Day9.findNextNumberOfSequence(List(11, 3, -14, -44, -91)) shouldBe -159

      Day9.findNextNumberOfSequence(List(-6, -7, -8, -9, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -20, -21, -22, -23, -24, -25, -26)) shouldBe -27
    }
  }
}
