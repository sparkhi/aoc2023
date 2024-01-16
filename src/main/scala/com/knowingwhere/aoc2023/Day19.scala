package com.knowingwhere.aoc2023

import com.knowingwhere.aoc2023.util.{LinesSplitter, StringUtil}

import scala.annotation.tailrec
import scala.io.Source
import scala.util.chaining.scalaUtilChainingOps

object Day19 extends App {
  val textBlocks = new LinesSplitter().splitAtBlankLines(Source.fromResource("day19-input.txt").getLines().toList)
  val workFlows = textBlocks.head.map(createWorkflow)
  val machineParts = textBlocks.tail.head.map(createMachinePart)

  machineParts.map(part => acceptPart(part, workFlows)).filter(_.isLeft).map(_.left.get).map(_.getRating).sum.pipe(println)

  def acceptPart(machinePart: MachinePart, workFlows: List[Workflow] ): Either[MachinePart, Boolean] = {
    val startingWorkFlow = workFlows.find(_.name == "in").head
    if (runFlow(machinePart, startingWorkFlow, startingWorkFlow.steps, workFlows)) {
      Left(machinePart)
    } else {
      Right(false)
    }
  }

  @tailrec
  def runFlow(machinePart: MachinePart, currentFlow: Workflow, remainingSteps: List[Rule], workFlows: List[Workflow]) : Boolean = {
    val currentRule = remainingSteps.head
    currentRule.getRuleName match {
      case "ACCEPT" => true
      case "REJECT" => false
      case "CONTINUE" =>
        val nextWorkflow = workFlows.find(_.name == currentRule.getNextWorkflow).head
        val newSteps = nextWorkflow.steps
        runFlow(machinePart, nextWorkflow, newSteps, workFlows)
      case "LESS_THAN" =>
        val isPass = currentRule.applyRule(machinePart)
        if (isPass) {
          val nextWorkFlowName = currentRule.getNextWorkflow
          nextWorkFlowName match {
            case "A" => true
            case "R" => false
            case _ =>
              val nextWorkflow = workFlows.find(_.name == nextWorkFlowName).head
              val newSteps = nextWorkflow.steps
              runFlow(machinePart, nextWorkflow, newSteps, workFlows)
          }
        } else {
          runFlow(machinePart, currentFlow, remainingSteps.tail, workFlows)
        }
      case "GREATER_THAN" =>
        val isPass = currentRule.applyRule(machinePart)
        if (isPass) {
          val nextWorkflowName = currentRule.getNextWorkflow
          nextWorkflowName match {
            case "A" => true
            case "R" => false
            case _ =>
              val nextWorkflow = workFlows.find(_.name == nextWorkflowName).head
              val newSteps = nextWorkflow.steps
              runFlow(machinePart, nextWorkflow, newSteps, workFlows)
          }
        } else {
          runFlow(machinePart, currentFlow, remainingSteps.tail, workFlows)
        }
    }
  }


  def createMachinePart(machinePartString: String) : MachinePart= {
    val partExpressions = StringUtil.trimTrailing(StringUtil.trimLeading(machinePartString, '{'), '}').split(",").toList
    MachinePart(partExpressions.map(eachExpr => getTuple(eachExpr)).toMap)
  }

  private def getTuple(expressionString: String): (String, Int) = {
    expressionString.split("=").head -> expressionString.split("=").tail.head.toInt
  }

  def createWorkflow(workflowString: String) : Workflow = {
    val workflowName = workflowString.split("\\{").head
    val workflowStepsString = StringUtil.trimTrailing(workflowString.split("\\{").tail.head, '}')
    val workflowSteps = workflowStepsString.split(",").map(createRule).toList
    Workflow(workflowName, workflowSteps)
  }

  def createRule(stepString: String) = {
    if (stepString.contains(":")) {
      val expressionParts = stepString.split(":").toList
      val nextStepName = expressionParts.tail.head
      val expression = expressionParts.head
      if (expression.contains("<")) {
        val operands = expression.split("<")
        new LessThanRule(operands.head, operands.tail.head.toInt, nextStepName)
      } else {
        val operands = expression.split(">")
        new GreaterThanRule(operands.head, operands.tail.head.toInt, nextStepName)
      }
    } else {
      stepString match {
        case "A" => new AcceptRule
        case "R" => new RejectRule
        case _ => new continueNextRule(stepString)
      }
    }
  }
}

// DATA STRUCTURES
case class Workflow (name: String, steps: List[Rule])
case class MachinePart(categories: Map[String, Int]) {
  def getRating: Int = {
    categories.values.sum
  }
}

abstract class Rule (nextStep: String) {
  def getRuleName: String
  def getNextWorkflow: String = { nextStep }
  def applyRule(part: MachinePart): Boolean = { throw new RuntimeException("You implement, you override")}
}

class GreaterThanRule (category: String, compareVal:Int, nextStep: String) extends Rule (nextStep) {
  override def applyRule(part: MachinePart): Boolean = {
    part.categories(category) > compareVal
  }

  override def getRuleName: String = {"GREATER_THAN"}
}

class LessThanRule (category: String, compareVal:Int, nextStep: String) extends Rule (nextStep) {
  override def applyRule(part: MachinePart): Boolean = {
    part.categories(category) < compareVal
  }

  override def getRuleName: String = {"LESS_THAN"}
}

class AcceptRule extends Rule ("ACCEPT") {
  override def applyRule(part: MachinePart): Boolean = {
    true
  }

  override def getRuleName: String = {"ACCEPT"}
}

class RejectRule extends Rule ("REJECT"){
  override def applyRule(part: MachinePart): Boolean = {
    false
  }

  override def getRuleName: String = {"REJECT"}
}

class continueNextRule (nextStep: String) extends Rule (nextStep) {
  override def applyRule(part: MachinePart): Boolean = {
    true
  }

  override def getRuleName: String = {"CONTINUE"}
}
