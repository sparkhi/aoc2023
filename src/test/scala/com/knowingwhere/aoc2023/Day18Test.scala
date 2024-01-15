package com.knowingwhere.aoc2023

import com.knowingwhere.aoc2023.util.{BigPosition, MathsUtil}
import org.scalatest.Matchers._
import org.scalatest._

import scala.util.chaining.scalaUtilChainingOps

class Day18Test extends WordSpec with BeforeAndAfterEach {
  "Day 18 " should {
    "calculate area using shoelace method for simple square" in {
      val vertices = List(BigPosition(0, 0), BigPosition(2,0), BigPosition(2,2), BigPosition(0, 2), BigPosition(0, 0))
      vertices.sliding(2).map(eachWindow => Day18.calculateArea(eachWindow)).sum shouldBe 8
    }
    "calculate area using shoelace method for a concave shape " in {
      val vertices = List(BigPosition(0, 0), BigPosition(1, 0), BigPosition(1, 2), BigPosition(0, 2), BigPosition(0, 3),
        BigPosition(1, 3), BigPosition(1, 5), BigPosition(0, 5), BigPosition(0, 4), BigPosition(-1, 4), BigPosition(-1, 1),
        BigPosition(0, 1), BigPosition(0,0))
        vertices.sliding(2).map(eachWindow => Day18.calculateArea(eachWindow)).sum shouldBe 14
    }

    "calculate big numbers area with example" in {
      val digPlan2 = """R 6 (#70c710)
                       |D 5 (#0dc571)
                       |L 2 (#5713f0)
                       |D 2 (#d2c081)
                       |R 2 (#59c680)
                       |D 2 (#411b91)
                       |L 5 (#8ceee2)
                       |U 2 (#caa173)
                       |L 1 (#1b58a2)
                       |U 2 (#caa171)
                       |R 2 (#7807d2)
                       |U 3 (#a77fa3)
                       |L 2 (#015232)
                       |U 2 (#7a21e3)""".stripMargin.split("\n").toList.map(Day18.createInstruction2)
//      digPlan2.map(_.pipe(println))

      val vertices = Day18.getVerticesOfPolygon(digPlan2, List(BigPosition(0, 0)))
      val coverage = Day18.calculateCoverage(vertices)

      coverage shouldBe BigInt("952408144115", 10)
    }

    "all square lines with a concave area " in {
      val vertices = List(
        BigPosition(0, 0),
        BigPosition(2, 0),
        BigPosition(2, 2),
        BigPosition(4, 2),
        BigPosition(4, 4),
        BigPosition(2, 4),
        BigPosition(2, 3),
        BigPosition(1, 3),
        BigPosition(1, 4),
        BigPosition(0, 4),
        BigPosition(0, 0))
      val areas = vertices.sliding(2).map(eachWindow => Day18.calculateArea(eachWindow)).toList
      val area = areas.sum
      area / 2 shouldBe BigInt("11", 10)
    }

  }
}
