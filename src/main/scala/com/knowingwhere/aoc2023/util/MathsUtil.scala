package com.knowingwhere.aoc2023.util

import scala.annotation.tailrec

object MathsUtil {

  @tailrec
  def gcd(a: BigInt, b: BigInt): BigInt = {
    if (b == 0) a else gcd(b, a % b)
  }

  def lcm(a: BigInt, b: BigInt): BigInt = {
    a * b / gcd(a, b)
  }

}
