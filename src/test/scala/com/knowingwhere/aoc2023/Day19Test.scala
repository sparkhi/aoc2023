package com.knowingwhere.aoc2023

import org.scalatest.Matchers._
import org.scalatest.{BeforeAndAfterEach, WordSpec}

class Day19Test extends WordSpec with BeforeAndAfterEach {
  "Day 19 Workflow " should {
    "create a workflow " in {
      val workflow = Day19.createWorkflow("px{a<2006:qkq,m>2090:A,rfg}").name shouldBe "px"
    }
  }
}
