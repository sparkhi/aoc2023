package com.knowingwhere.aoc2023

import scala.io.Source

object Day2 extends App {
  val gamesData = Source.fromResource("day2-input.txt").getLines().toList
  val games = getGames(gamesData)
  val possibleGames = games.filter(_.maxRed <= 12).filter(_.maxGreen <= 13).filter(_.maxBlue <= 14)
  val sumOfIds = possibleGames.map(_.id).sum
  println(sumOfIds)

  //part 2
  val sumOfPower = games.map(eachGame => eachGame.maxRed * eachGame.maxGreen * eachGame.maxBlue).sum
  println(sumOfPower)

  /**
   *
   * @param eachCubeset cubeset description, the colours are not in sequence, and not all colours may be present
   *      e.g. 1 blue, 2 green;
   *      3 green, 4 blue, 1 red;
   *      1 green, 1 blue
   * @return
   */
  def parseCubset(eachCubeset: String): CubeSet = {
    val colourBalls = eachCubeset.split(",").toList
    val red = if (!colourBalls.exists(_.endsWith(" red"))) 0 else
      colourBalls.filter(_.endsWith(" red")).head.trim.split(" ").head.toInt
    val green = if (!colourBalls.exists(_.endsWith(" green"))) 0 else
      colourBalls.filter(_.endsWith(" green")).head.trim.split(" ").head.toInt
    val blue = if (!colourBalls.exists(_.endsWith(" blue"))) 0 else
      colourBalls.filter(_.endsWith(" blue")).head.trim.split(" ").head.toInt
    CubeSet(red, green, blue)
  }

  /**
   *
   * @param gameLine e.g. Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
   * @return Game object
   */
  def parseLine(gameLine: String): Game = {
    val gameId = gameLine.split(":").head.substring("Game ".length).toInt
    val attemptsList = gameLine.split(":").tail.head.split(";").map(eachCubeset => parseCubset(eachCubeset)).toList
    Game(gameId, attemptsList)
  }

  def getGames(gamesData: List[String]): List[Game] = {
    gamesData.map(eachLine => parseLine(eachLine))
  }

}

case class CubeSet(red: Int, green: Int, blue: Int)
case class Game(id: Int, attempts: List[CubeSet]) {
  def maxRed: Int = attempts.map(_.red).max
  def maxGreen: Int = attempts.map(_.green).max
  def maxBlue: Int = attempts.map(_.blue).max
}