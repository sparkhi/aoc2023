package com.knowingwhere.aoc2023.Day7

object Hands extends Enumeration {
  val FiveOfAKind: Hands.Value = Value(6)
  val FourOfAKind: Hands.Value = Value(5)
  val FullHouse: Hands.Value = Value(4)
  val ThreeOfAKind: Hands.Value = Value(3)
  val TwoPair: Hands.Value = Value(2)
  val OnePair: Hands.Value = Value(1)
  val HighCard: Hands.Value = Value(0)
}
