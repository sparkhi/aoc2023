package com.knowingwhere.aoc2023.Day7

import org.scalatest._
import org.scalatest.Matchers._

//best hand is highest -- STRONGEST
class HandTest extends WordSpec with BeforeAndAfterEach {
  "Hands " should {
    "Consecutive distinct hand types should be ordered by their type " in {
      Hand("AAAAA") > Hand("QQQQ8")  shouldBe true //five of a kind
      Hand("AA8AA") > Hand("23332")  shouldBe true // four of a kind
      Hand("23332") > Hand("TTT98")  shouldBe true //Full house
      Hand("TTT98") > Hand("23432")  shouldBe true //three of a kind
      Hand("23432") > Hand("A23A4")  shouldBe true //two pairs
      Hand("A23A4") > Hand("23456")  shouldBe true //one pair
    }
    "Non consecutive hand types are ordered by their type ranking " in {
      Hand("AAAAA") > Hand("23456")  shouldBe true //five of a kind is better than High card
      Hand("23432") < Hand("23332")  shouldBe true
    }
    "Same type of hands are ordered by their first card value" in {
      Hand("KK677") > Hand("KTJJT") shouldBe true
      Hand("T55J5") < Hand("QQQJA") shouldBe true
    }
  }
}
