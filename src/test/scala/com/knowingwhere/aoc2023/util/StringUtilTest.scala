package com.knowingwhere.aoc2023.util

import org.scalatest.Matchers._
import org.scalatest.{BeforeAndAfterEach, WordSpec}

class StringUtilTest extends WordSpec with BeforeAndAfterEach {
  "String utils " should {
    "trim leading character from string" in {
      StringUtil.trimLeading(".??..??...?##.", '.') shouldBe "??..??...?##."
      StringUtil.trimLeading(".....??..??...?##.", '.') shouldBe "??..??...?##."
    }
  }
}
