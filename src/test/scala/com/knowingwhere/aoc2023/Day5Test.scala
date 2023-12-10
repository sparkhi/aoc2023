package com.knowingwhere.aoc2023

import com.knowingwhere.aoc2023.Day5.createMapping
import com.knowingwhere.aoc2023.util.LinesSplitter
import org.scalatest.Matchers._
import org.scalatest._

import scala.io.Source

class Day5Test extends WordSpec  with BeforeAndAfterEach {
  "Day 5 " should {
    val lines = Source.fromResource("day5-test-input.txt").getLines().toList
    val stringGroups = new LinesSplitter().splitAtBlankLines(lines)
    val seeds = stringGroups.head.head.split(":").tail.head.trim.split(" ").map(_.toLong).toList
    val almanacMappings = Day5.createAlmanac(stringGroups)

    "find soil from seeds " in {
      Day5.findSoilFromSeed(almanacMappings,79) shouldBe 81
      Day5.findSoilFromSeed(almanacMappings, 14) shouldBe 14
      Day5.findSoilFromSeed(almanacMappings, 55) shouldBe 57
      Day5.findSoilFromSeed(almanacMappings,13) shouldBe 13
    }
    "pars into groups " in {
      val soils = seeds.map(Day5.findSoilFromSeed(almanacMappings, _))
      val fertilizer = soils.map(Day5.findFertilizerFromSoil(almanacMappings, _))
      val water = fertilizer.map(Day5.findWaterFromFertilizer(almanacMappings, _))
      val light = water.map(Day5.findLightFromWater(almanacMappings, _))
      val temperature = light.map(Day5.findTemperatureFromLight(almanacMappings, _))
      val humidity = temperature.map(Day5.findHumidityFromTemperature(almanacMappings, _))
      val location = humidity.map(Day5.findLocationFromHumidity(almanacMappings, _))

      println(seeds)
      println(soils)
      println(fertilizer)
      println(water)
      println(light)
      println(temperature)
      println(humidity)
      println(location)

    }
  }
}
