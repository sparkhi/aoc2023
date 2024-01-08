package com.knowingwhere.aoc2023

import org.scalatest.Matchers._
import org.scalatest.{BeforeAndAfterEach, WordSpec}

class Day15Test extends WordSpec with BeforeAndAfterEach {
  "Day 15" should {
    "generate hash using the custom algorithm " in {
      Day15.generateHash("rn=1") shouldBe 30
      Day15.generateHash("HASH") shouldBe 52
      Day15.generateHash("rn=1") shouldBe 30
      Day15.generateHash("cm-") shouldBe 253
      Day15.generateHash("qp=3") shouldBe 97
      Day15.generateHash("cm=2") shouldBe 47
      Day15.generateHash("qp-") shouldBe 14
      Day15.generateHash("pc=4") shouldBe 180
      Day15.generateHash("ot=9") shouldBe 9
      Day15.generateHash("ab=5") shouldBe 197
      Day15.generateHash("pc-") shouldBe 48
      Day15.generateHash("pc=6") shouldBe 214
      Day15.generateHash("ot=7") shouldBe 231
    }
  }

}
