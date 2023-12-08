package com.knowingwhere.aoc2023.util

import scala.annotation.tailrec

class LinesSplitter {
  @tailrec
  private def splitRecurse(remainingLines: List[String], currentList: List[List[String]], currentBlock: List[String]): List[List[String]] = {
    if (remainingLines.isEmpty) {
      currentList :+ currentBlock
    } else{
      val currentLine = remainingLines.head
      if (currentLine.isBlank) {
        if (currentBlock.isEmpty) { //consecutive blank lines should be ignored
          splitRecurse(remainingLines.tail, currentList, Nil)
        } else {
          splitRecurse(remainingLines.tail, currentList :+ currentBlock, Nil)
        }
      } else {
        splitRecurse(remainingLines.tail, currentList, currentBlock :+ currentLine)
      }
    }
  }

  def splitAtBlankLines(lines: List[String]): List[List[String]] = {
    splitRecurse(lines, Nil, Nil)
  }
}
