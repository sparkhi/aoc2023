package com.knowingwhere.aoc2023.Day7

object Cards extends Enumeration {
  val ACE: Cards.Value = Value(14, "A")
  val KING: Cards.Value = Value(13, "K")
  val QUEEN: Cards.Value = Value(12, "Q")
  val JACK: Cards.Value = Value(11, "J")
  val TEN: Cards.Value = Value(10, "T")
  val NINE: Cards.Value = Value(9, "9")
  val EIGHT: Cards.Value = Value(8, "8")
  val SEVEN: Cards.Value = Value(7, "7")
  val SIX: Cards.Value = Value(6, "6")
  val FIVE: Cards.Value = Value(5, "5")
  val FOUR: Cards.Value = Value(4, "4")
  val THREE: Cards.Value = Value(3, "3")
  val TWO: Cards.Value = Value(2, "2")
}
