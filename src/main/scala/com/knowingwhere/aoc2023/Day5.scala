package com.knowingwhere.aoc2023

import com.knowingwhere.aoc2023.util.LinesSplitter

import scala.io.Source

object Day5 extends App {
  val lines = Source.fromResource("day5-input.txt").getLines.toList
  val stringGroups = new LinesSplitter().splitAtBlankLines(lines)
  val seeds = stringGroups.head.head.split(":").tail.head.trim.split(" ").map(_.toLong).toList
  val almanacMappings = createAlmanac(stringGroups)
  println(seeds)

  val soils = seeds.map(findSoilFromSeed(almanacMappings, _))
  val fertilizer = soils.map(findFertilizerFromSoil(almanacMappings, _))
  val water = fertilizer.map(findWaterFromFertilizer(almanacMappings, _))
  val light = water.map(findLightFromWater(almanacMappings, _))
  val temperature = light.map(findTemperatureFromLight(almanacMappings, _))
  val humidity = temperature.map(findHumidityFromTemperature(almanacMappings, _))
  val location = humidity.map(findLocationFromHumidity(almanacMappings, _))

  println(location.min)
  println(location)


  def createAlmanac(stringGroups: List[List[String]]) = {
    stringGroups.tail.map(eachGroup => createMapping(eachGroup))
  }

  private def findDestinationValue(almanacMapping: List[AlmanacMapping], source: Long, sourceType: String, destinationType: String): Long = {
    val mapName = sourceType + "-to-" + destinationType
    val almanac = almanacMapping.find(_.name == mapName)
    almanac.head.findDestinationValueForSource(source)
  }

  def findSoilFromSeed(almanacMapping: List[AlmanacMapping], source: Long): Long = {
    findDestinationValue(almanacMapping, source, "seed", "soil")
  }

  def findFertilizerFromSoil(almanacMapping: List[AlmanacMapping], source: Long): Long = {
    findDestinationValue(almanacMapping, source, "soil", "fertilizer")
  }

  def findWaterFromFertilizer(almanacMapping: List[AlmanacMapping], source: Long): Long = {
    findDestinationValue(almanacMapping, source, "fertilizer", "water")
  }

  def findLightFromWater(almanacMapping: List[AlmanacMapping], source: Long): Long = {
    findDestinationValue(almanacMapping, source, "water", "light")
  }

  def findTemperatureFromLight(almanacMapping: List[AlmanacMapping], source: Long): Long = {
    findDestinationValue(almanacMapping, source, "light", "temperature")
  }

  def findHumidityFromTemperature(almanacMapping: List[AlmanacMapping], source: Long): Long = {
    findDestinationValue(almanacMapping, source, "temperature", "humidity")
  }

  def findLocationFromHumidity(almanacMapping: List[AlmanacMapping], source: Long): Long = {
    findDestinationValue(almanacMapping, source, "humidity", "location")
  }

  def createMapping(eachGroup: List[String]): AlmanacMapping = {
    val nameString = eachGroup.head
    val name = nameString.substring(0, nameString.length - "map:".length).trim

    val entries = eachGroup.tail.map(eachEntry => createMappingEntry(eachEntry))

    AlmanacMapping(name, entries)
  }

  def createMappingEntry(eachEntry: String): MappingEntry = {
    val elements = eachEntry.split(" ").map(_.trim.toLong).toList
    MappingEntry(elements.head, elements.tail.head, elements.tail.tail.head)
  }


}

case class AlmanacMapping(name: String, mappings: List[MappingEntry]) {
  def findDestinationValueForSource(sourceVal: Long): Long = {
    val possibleMappings = mappings.find(entry => entry.sourceVal <= sourceVal && entry.getMaxSource >= sourceVal).toList
    if (possibleMappings.isEmpty) {
      sourceVal
    } else {
      val selectedMapping = possibleMappings.head
      sourceVal + (selectedMapping.desinationVal - selectedMapping.sourceVal)
    }
  }
}

case class MappingEntry(desinationVal: Long, sourceVal: Long, range: Long) {
  def getMaxSource: Long = sourceVal + range - 1
}