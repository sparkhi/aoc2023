package com.knowingwhere.aoc2023.util

import scala.annotation.tailrec

object StringUtil {
  @tailrec
  def trimLeading(source: String, charToBeTrimmed: Char): String = {
    if (source.isEmpty) {
      ""
    } else {
      val leadingChar = source.head
      if (leadingChar.equals(charToBeTrimmed)) {
        trimLeading(source.tail, charToBeTrimmed)
      } else {
        source
      }
    }
  }

  def trimTrailing(source: String, charToBeTrimmed: Char): String = {
    trimLeading(source.reverse, charToBeTrimmed).reverse
  }

  def trim(source: String, charToBeTrimmed: Char): String = {
    trimTrailing(trimLeading(source, charToBeTrimmed), charToBeTrimmed)
  }
}
