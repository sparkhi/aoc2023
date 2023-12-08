package com.knowingwhere.aoc2023.util

import org.scalatest._
import org.scalatest.Matchers._

class LinesSplitterTest extends WordSpec with BeforeAndAfterEach{
  "Lines splitter " should {
    val splitter = new LinesSplitter()
    "split the lines into groups based on a blank line" in {
      val lines =
        """|Here is a multiline
          |String that
          |
          |Hopefully will be
          |parsed into two groups
          |""".stripMargin.split("\n").toList
      val groups = splitter.splitAtBlankLines(lines)
      groups.size shouldBe 2
      groups.head.size shouldBe 2
      groups.head.head shouldBe "Here is a multiline"

      groups.tail.head.size shouldBe 2
    }

    "ignore consecutive blank lines when splitting" in {
      val lines =
        """|Here is a multiline
           |String that
           |
           |
           |Hopefully will be
           |parsed into two groups
           |""".stripMargin.split("\n").toList

      val groups = splitter.splitAtBlankLines(lines)
      groups.size shouldBe 2
    }

    "ignore blank lines at the beginning and end when splitting" in {
      val lines =
        """|
           |Here is a multiline
           |String that
           |
           |
           |Hopefully will be
           |parsed into two groups
           |
           |""".stripMargin.split("\n").toList

      val groups = splitter.splitAtBlankLines(lines)
      groups.size shouldBe 2
    }
  }
}
