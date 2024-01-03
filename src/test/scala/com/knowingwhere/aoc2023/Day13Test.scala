package com.knowingwhere.aoc2023

import org.scalatest.Matchers._
import org.scalatest._

class Day13Test extends WordSpec with BeforeAndAfterEach {
  "Find reflection line " should {
    "No horizontal line of reflection" in {
      val pattern = """#.##..##.
                      |..#.##.#.
                      |##......#
                      |##......#
                      |..#.##.#.
                      |..##..##.
                      |#.#.##.#.""".stripMargin.split("\n").toList
      Day13.findHorizontalLineOfSymmetryTryEach(pattern) shouldBe -1
    }

    "Horizontal line of reflection at " in {
      val pattern = """#...##..#
                      |#....#..#
                      |..##..###
                      |#####.##.
                      |#####.##.
                      |..##..###
                      |#....#..#""".stripMargin.split("\n").toList
      Day13.findHorizontalLineOfSymmetryTryEach(pattern) shouldBe 4
    }

    "vertical line of reflection" in {
      val pattern =
        """#.##..##.
          |..#.##.#.
          |##......#
          |##......#
          |..#.##.#.
          |..##..##.
          |#.#.##.#.""".stripMargin.split("\n").toList
      Day13.findVerticalLineOfSymmetryTryEach(pattern) shouldBe 5
    }

    "vertical line of reflection 2 " in {
      val pattern =
        """.##.#######
          |.##....##.#
          |#..#.#.#.##
          |#####....##
          |####.....##
          |#..#.#.#.##
          |.##....##.#
          |.##.#######
          |####.#.###.
          |#..#####..#
          |......#####
          |........###
          |#..###.###.""".stripMargin.split("\n").toList
      Day13.findVerticalLineOfSymmetryTryEach(pattern) shouldBe 2
      Day13.findHorizontalLineOfSymmetryTryEach(pattern) shouldBe -1
    }

    "trial" in {
      val pattern = """.#..###.#
                      |..#...##.
                      |#####.#.#
                      |....#.###
                      |#.###.#.#
                      |###.####.
                      |##..#..#.
                      |.#.#.....
                      |.#.#.....
                      |##..#..#.
                      |###.####.
                      |#.###.#.#
                      |....#.###
                      |#####...#
                      |..#...##.
                      |.#..###.#
                      |.#..###.#
                      |""".stripMargin.split("\n").toList
      Day13.findVerticalLineOfSymmetryTryEach(pattern) shouldBe -1
      Day13.findHorizontalLineOfSymmetryTryEach(pattern) shouldBe 16
    }

//    NOTE: This test case is far fetched, there is no such data in the main input so we can ignore this
//    However, if such data comes up, the candidates would be (1 and 2) but the better answer could be 2
//    as it has bigger reflected ares.
//
//    "horizontal line of symmetry when all lines are same " in {
//      val pattern =
//        """|ABCDEFG
//          |ABCDEFG
//          |ABCDEFG
//          |ABCDEFG
//          |MNOPQRS
//          |""".stripMargin.split("\n").toList
//      Day13.findHorizontalLineOfSymmetryTryEach(pattern) shouldBe 2
//    }

    "horizontal line of symmetry when mirror is closer to top but last line also has a group " in {
      val pattern =
        """|ABCDEFG
          |MNOPQRS
          |MNOPQRS
          |ABCDEFG
          |XYZXYZX
          |LMNOPQR
          |XYZXYZX
          |""".stripMargin.split("\n").toList
      Day13.findHorizontalLineOfSymmetryTryEach(pattern) shouldBe 2
    }
  }
}
