package com.knowingwhere.aoc2023

import org.scalatest.Matchers._
import org.scalatest._

class Day14Test extends WordSpec with BeforeAndAfterEach {
  "Day 14 " should {
    "roll strings to left " in {
      Day14.rollStonesToLeft("...#.#.O....#.#.OO.OOO.O..O.......O#OO#.#..#....O..") shouldBe "...#.#O.....#.#OOOOOOOO............#OO#.#..#O......"
    }

    "roll north then calculate load " in {
      val grid = """O....#....
                   |O.OO#....#
                   |.....##...
                   |OO.#O....O
                   |.O.....O#.
                   |O.#..O.#.#
                   |..O..#O..O
                   |.......O..
                   |#....###..
                   |#OO..#....""".stripMargin.split("\n").toList
      val rolled = Day14.rollStonesToNorth(grid)
      Day14.calculateLoadOnNorth(rolled, 0) shouldBe 136
    }

    "Roll cycles " in {
      val grid = """O....#....
                   |O.OO#....#
                   |.....##...
                   |OO.#O....O
                   |.O.....O#.
                   |O.#..O.#.#
                   |..O..#O..O
                   |.......O..
                   |#....###..
                   |#OO..#....""".stripMargin.split("\n").toList
      Day14.runCycle(grid, List(0 -> 0))
    }


  }
}
