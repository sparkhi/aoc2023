package com.knowingwhere.aoc2023.Day7

case class Hand (cardSequence: String) extends Ordered[Hand] {

  def calculateHandType(cardSequence: String): Hands.Value = {
    val chars = cardSequence.toCharArray.toList
    val groups = chars.groupBy(identity).values.map(_.length).toList
    groups.size match {
      case 1 => //all cards are of same denomination
        Hands.FiveOfAKind
      case 2 => //the groups are either 4 - 1 or 3 - 2
        if (groups.contains(4))
          Hands.FourOfAKind
        else
          Hands.FullHouse
      case 3 => //the groups are 3 - 1 - 1 or 2 - 2 - 1
          if (groups.contains(3))
            Hands.ThreeOfAKind
          else
            Hands.TwoPair
      case 4 => //only possible combination of groups is 2 - 1 - 1 - 1
        Hands.OnePair
      case _ =>
        Hands.HighCard
    }
  }

  lazy val handType: Int = calculateHandType(cardSequence).id


  def compareCardSequence(thisSequence: String, thatSequence: String): Int = {
    val thisChar = thisSequence.head.toString
    val thatChar = thatSequence.head.toString
    if (thisChar != thatChar) {
      Cards.withName(thisChar).id - Cards.withName(thatChar).id
    } else {
      compareCardSequence(thisSequence.tail, thatSequence.tail)
    }
  }

  override def compare(that: Hand): Int = {
    val equality = this.handType - that.handType
    if (equality != 0) {
      equality
    } else {
      compareCardSequence(this.cardSequence, that.cardSequence)
    }
  }
}

