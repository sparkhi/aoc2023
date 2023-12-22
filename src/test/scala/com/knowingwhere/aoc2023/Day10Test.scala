package com.knowingwhere.aoc2023

import com.knowingwhere.aoc2023.util.Position
import org.scalatest.Matchers._
import org.scalatest._

class Day10Test extends WordSpec with BeforeAndAfterEach {
  "point in polygon implementation" should {
    "find count of places in sweeping from left to right" in {
      val line = ".F-------7."
      val path = List(Position(1,0), Position(2,0), Position(3,0), Position(4,0), Position(5,0),
        Position(6,0), Position(7,0), Position(8,0), Position(9,0))
      Day10.findInsidePositionCount(line, path, 0) shouldBe 0

      val path2 = List(Position(1,0), Position(2,0), Position(8,0), Position(9,0))
      Day10.findInsidePositionCount(".||.....||.", path2, 0) shouldBe 0

      val path3 = List(Position(1,0), Position(4,0), Position(6,0), Position(9,0))
      Day10.findInsidePositionCount(".|..|.|..|.", path3, 0) shouldBe 4

    }

    "calculate path then find the inside points " in {
      //as we have not implemented finding the type of tile at 'S', for the following input, it is sumply changed to 'F' at 1,1
      val grid = """...........
                   |.F-------7.
                   |.|F-----7|.
                   |.||.....||.
                   |.||.....||.
                   |.|L-7.F-J|.
                   |.|..|.|..|.
                   |.L--J.L--J.
                   |...........
                   |""".stripMargin

      val tileRows = grid.split("\n").toList
      val tiles = tileRows.zipWithIndex.flatMap(eachLine => Day10.createTiles(eachLine))
      val startPos = Position(1,1)

      //starting the search in north direction, as looking at the input, we know it can only be north-south
      val path = Day10.tracePath(tiles, Position(startPos.x, startPos.y + 1), List(startPos))

      val insideCounts = tileRows.zipWithIndex.map(eachRow => Day10.findInsidePositionCount(eachRow._1, path, eachRow._2))
      insideCounts.sum shouldBe 4

    }
    "calculate path2 then find the inside points " in {

      //as we have not implemented finding the type of tile at 'S', for the following input, it is sumply changed to 'F' at 12,4

      val grid = """.F----7F7F7F7F-7....
                   |.|F--7||||||||FJ....
                   |.||.FJ||||||||L7....
                   |FJL7L7LJLJ||LJ.L-7..
                   |L--J.L7...LJF7F-7L7.
                   |....F-J..F7FJ|L7L7L7
                   |....L7.F7||L7|.L7L7|
                   |.....|FJLJ|FJ|F7|.LJ
                   |....FJL-7.||.||||...
                   |....L---J.LJ.LJLJ...""".stripMargin

      val tileRows = grid.split("\n").toList
      val tiles = tileRows.zipWithIndex.flatMap(eachLine => Day10.createTiles(eachLine))
      val startPos =  Position(12, 4)

      //starting the search in north direction, as looking at the input, we know it can only be north-south
      val path = Day10.tracePath(tiles, Position(startPos.x, startPos.y + 1), List(startPos))

      val insideCounts = tileRows.zipWithIndex.map(eachRow => Day10.findInsidePositionCount(eachRow._1, path, eachRow._2))
      println(insideCounts)
      insideCounts.sum shouldBe 8
    }

  }
}
