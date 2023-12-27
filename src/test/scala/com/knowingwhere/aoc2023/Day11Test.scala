package com.knowingwhere.aoc2023

import org.scalatest.Matchers._
import org.scalatest._

class Day11Test extends WordSpec with BeforeAndAfterEach{
  "Day 11" should {
    "expand the universe where there are no galaxies present" in {
      val galaxyMap = """...#......
                        |.......#..
                        |#.........
                        |..........
                        |......#...
                        |.#........
                        |.........#
                        |..........
                        |.......#..
                        |#...#.....""".stripMargin

      val compactUniverse = galaxyMap.split("\n").toList

      val universe = Day11.expand(compactUniverse)
      val galaxies = universe.zipWithIndex.flatMap(rowEntry => Day11.createGalaxyLocationsOfRow(rowEntry._2, rowEntry._1))
      galaxies.size shouldBe 9
      Day11.findAllPathDistanceSum(galaxies, BigInt(0)) shouldBe 374
    }
  }
}
