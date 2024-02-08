package com.knowingwhere.aoc2023

import com.knowingwhere.aoc2023.util.{InclusiveRange, LinesSplitter, StringUtil}

import scala.annotation.tailrec
import scala.io.Source
import scala.util.chaining.scalaUtilChainingOps

object Day19 extends App {
  val textBlocks = new LinesSplitter().splitAtBlankLines(Source.fromResource("day19-input.txt").getLines().toList)
  val workFlows = textBlocks.head.map(createWorkflow)
  val machineParts = textBlocks.tail.head.map(createMachinePart)

  //Part 1
  machineParts.map(part => acceptPart(part, workFlows)).filter(_.isLeft).map(_.left.get).map(_.getRating).sum.pipe(println)

  //Part 2
  getPossibleCombinationsOfAcceptance(workFlows).pipe(println)

  /**
   * Part 1 of the problem - run each part through the workflows
   * @param machinePart The part to be processed
   * @param workFlows list of all workflows
   * @return Left if the machine part is accepted, right (false) if the machine part was rejected
   */
  def acceptPart(machinePart: MachinePart, workFlows: List[Workflow] ): Either[MachinePart, Boolean] = {
    val startingWorkFlow = workFlows.find(_.name == "in").head
    if (runFlow(machinePart, startingWorkFlow.steps, workFlows)) {
      Left(machinePart)
    } else {
      Right(false)
    }
  }

  /**
   * ************ PART 1 **************
   * Recursively run a machine part through the workflow steps.
    * @param machinePart the part under consideration
   * @param remainingSteps workflow steps to be carried out
   * @param workFlows list of all workflows
   * @return true if machine part is accepted, false if machine part is rejected
   */
  @tailrec
  def runFlow(machinePart: MachinePart, remainingSteps: List[Rule], workFlows: List[Workflow]) : Boolean = {
    val currentRule = remainingSteps.head
    currentRule.getRuleName match {
      case "ACCEPT" => true
      case "REJECT" => false
      case "CONTINUE" =>
        val newSteps = workFlows.find(_.name == currentRule.getNextWorkflow).head.steps
        runFlow(machinePart, newSteps, workFlows)
      case "LESS_THAN" =>
        val isPass = currentRule.applyRule(machinePart)
        if (isPass) {
          val nextWorkFlowName = currentRule.getNextWorkflow
          nextWorkFlowName match {
            case "A" => true
            case "R" => false
            case _ =>
              val newSteps = workFlows.find(_.name == nextWorkFlowName).head.steps
              runFlow(machinePart, newSteps, workFlows)
          }
        } else {
          runFlow(machinePart, remainingSteps.tail, workFlows)
        }
      case "GREATER_THAN" =>
        val isPass = currentRule.applyRule(machinePart)
        if (isPass) {
          val nextWorkflowName = currentRule.getNextWorkflow
          nextWorkflowName match {
            case "A" => true
            case "R" => false
            case _ =>
              val newSteps = workFlows.find(_.name == nextWorkflowName).head.steps
              runFlow(machinePart, newSteps, workFlows)
          }
        } else {
          runFlow(machinePart, remainingSteps.tail, workFlows)
        }
    }
  }

  /**
   * Part 2, start with a list containing MachinePartRanges of 1 to 4000 for all categories
   * @param workflows list of all workflows
   * @return sum of all possible combinations
   */
  def getPossibleCombinationsOfAcceptance(workflows: List[Workflow]): BigInt = {
    val startingList = List(MachinePartRanges(Map(
      "x" -> InclusiveRange(1, 4000),
      "m" -> InclusiveRange(1, 4000),
      "a" -> InclusiveRange(1, 4000),
      "s" -> InclusiveRange(1, 4000))) -> workflows.find(_.name == "in").get.steps)
    val acceptedPartRanges = List.empty[MachinePartRanges]
    val acceptedRanges = processRanges(startingList, workflows, acceptedPartRanges)
    acceptedRanges.map(_.getPossibleCombinations).sum
  }

  /**
   * ************ PART 2 **************
   * Process a range as per the rule and split it into 2 ranges, one that meets condition and other that does not meet
   * the condition. Depending on the action needed, the split range is either accepted, rejected or added to the list of
   * ranges for further processing recursively. If a range is accepted, it is added to the accumulation list of accepted
   * ranges. Once all ranges are processed as per rules, we get a list of accepted ranges which are returned to the caller
   * @param rangesToProcess Ranges to process in each iteration
   * @param workFlows List of all workflows
   * @param acceptedPartRanges Accumulation list of accepted ranges
   * @return List of accepted ranges
   */
  @tailrec
  def processRanges(rangesToProcess: List[(MachinePartRanges, List[Rule])], workFlows: List[Workflow], acceptedPartRanges: List[MachinePartRanges]): List[MachinePartRanges] = {
    rangesToProcess.length match {
      case 0 => acceptedPartRanges
      case _ =>
        val rangeToProcess = rangesToProcess.head
        val rules = rangeToProcess._2
        if (rules.isEmpty) {
          throw new IllegalStateException("how did we get here")
        }
        val currentRule = rules.head
        currentRule.getRuleName match {
          case "ACCEPT" =>
            processRanges(rangesToProcess.tail, workFlows, acceptedPartRanges :+ rangeToProcess._1)
          case "REJECT" =>
            processRanges(rangesToProcess.tail, workFlows, acceptedPartRanges)
          case "LESS_THAN" =>
            val accepted = currentRule.getSuccessRange(rangeToProcess._1)
            val rejected = currentRule.getFailureRange(rangeToProcess._1)
            currentRule.getNextWorkflow match {
              case "A" =>
                val nextRejectedSteps = rangeToProcess._2.tail
                processRanges(rangesToProcess.tail :+ (rejected -> nextRejectedSteps), workFlows, acceptedPartRanges :+ accepted)
              case "R" =>
                val nextWorkflow = workFlows.find(_.name == currentRule.getNextWorkflow)
                if (nextWorkflow.nonEmpty) {
                  val nextAcceptedSteps = workFlows.find(_.name == currentRule.getNextWorkflow).get.steps
                  processRanges(rangesToProcess.tail :+ (rejected -> nextAcceptedSteps), workFlows, acceptedPartRanges)
                } else {
                  val nextRejectedSteps = rangeToProcess._2.tail
                  processRanges(rangesToProcess.tail :+ (rejected -> nextRejectedSteps), workFlows, acceptedPartRanges)
                }
              case _ =>
                val nextAcceptedSteps = workFlows.find(_.name == currentRule.getNextWorkflow).get.steps
                val nextRejectedSteps = rangeToProcess._2.tail
                processRanges(rangesToProcess.tail :+ (accepted -> nextAcceptedSteps) :+ (rejected -> nextRejectedSteps), workFlows, acceptedPartRanges)
            }
          case "GREATER_THAN" =>
            val accepted = currentRule.getSuccessRange(rangeToProcess._1)
            val rejected = currentRule.getFailureRange(rangeToProcess._1)
            currentRule.getNextWorkflow match {
              case "A" =>
                val nextRejectedSteps = rangeToProcess._2.tail
                processRanges(rangesToProcess.tail :+ (rejected -> nextRejectedSteps), workFlows, acceptedPartRanges :+ accepted)
              case "R" =>
                val nextWorkflow = workFlows.find(_.name == currentRule.getNextWorkflow)
                if (nextWorkflow.nonEmpty) {
                  val nextAcceptedSteps = workFlows.find(_.name == currentRule.getNextWorkflow).get.steps
                  processRanges(rangesToProcess.tail :+ (rejected -> nextAcceptedSteps), workFlows, acceptedPartRanges)
                } else {
                  val nextRejectedSteps = rangeToProcess._2.tail
                  processRanges(rangesToProcess.tail :+ (rejected -> nextRejectedSteps), workFlows, acceptedPartRanges)
                }
              case _ =>
                val nextAcceptedSteps = workFlows.find(_.name == currentRule.getNextWorkflow).get.steps
                val nextRejectedSteps = rangeToProcess._2.tail
                processRanges(rangesToProcess.tail :+ (accepted -> nextAcceptedSteps) :+ (rejected -> nextRejectedSteps), workFlows, acceptedPartRanges)
            }
          case "CONTINUE" =>
            val nextSteps = workFlows.find(_.name == currentRule.getNextWorkflow).head.steps
            processRanges(rangesToProcess.tail :+ (rangeToProcess._1 -> nextSteps) , workFlows, acceptedPartRanges)
        }
    }
  }

  /**
   * Create a machine part from an input string
   * @param machinePartString input string
   * @return Machine Part object
   */
  def createMachinePart(machinePartString: String) : MachinePart= {
    val partExpressions = StringUtil.trimTrailing(StringUtil.trimLeading(machinePartString, '{'), '}').split(",").toList
    MachinePart(partExpressions.map(eachExpr => getTuple(eachExpr)).toMap)
  }

  private def getTuple(expressionString: String): (String, Int) = {
    expressionString.split("=").head -> expressionString.split("=").tail.head.toInt
  }

  /**
   * Workflow parsing to create a workflow.
   * @param workflowString string representation of workflow from input
   * @return Workflow
   */
  def createWorkflow(workflowString: String) : Workflow = {
    val workflowName = workflowString.split("\\{").head
    val workflowStepsString = StringUtil.trimTrailing(workflowString.split("\\{").tail.head, '}')
    val workflowSteps = workflowStepsString.split(",").map(createRule).toList
    Workflow(workflowName, workflowSteps)
  }

  /**
   * Rule parsing to create a Rule object from the string
   * @param stepString string representation of the rule in input
   * @return Subclass of the Rule object
   */
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

case class MachinePartRanges(categories: Map[String, util.InclusiveRange]) {
  def getPossibleCombinations: BigInt = {
    val prd = categories.map(eachItem => BigInt(eachItem._2.max - eachItem._2.min + 1)).product
    prd
  }

  override def toString: String = {
    categories.map(makeString).mkString
  }

  def makeString(category: (String, InclusiveRange)): String = {
    category._1 + " -> (" + category._2.min + ", " + category._2.max + ") "
  }
}

/**
 * Represents a base class for a "Rule", A rule is any action taken on the machine part. As a result, every rule has
 * following components
 *    NextStep -> represents what needs to happen when the condition of the rule is met
 *    Name -> Name of the rule, this is useful for pattern matching when processing rules
 *
 * @param nextStep next step for the flow
 */
abstract class Rule (nextStep: String) {
  def getRuleName: String
  def getNextWorkflow: String = { nextStep }
  def applyRule(part: MachinePart): Boolean = { throw new RuntimeException("You implement, you override")}

  /**
   * Return a MachinePartRanges object which contains the range that meets the criteria
   * @param currentPartRange current ranges
   * @return New MachinePartRanges which meets the condition
   */
  def getSuccessRange(currentPartRange: MachinePartRanges): MachinePartRanges = { throw new RuntimeException("You implement, you override")}

  /**
   * Return a MachinePartRanges object which contains the range after removing the range which meets the criteria
   * @param currentPartRange current ranges
   * @return New MachinePartRanges which does not meet the condition
   */
  def getFailureRange(currentPartRange: MachinePartRanges): MachinePartRanges = { throw new RuntimeException("You implement, you override")}

  /**
   * Generate a new MachinePArt with a specific range modified. Useful to split the ranges on condition
   * @param part Existing part
   * @param categoryToModify name of the category whose range needs to be changed
   * @param min minimum value of new range
   * @param max maximum value of new range
   * @return New MachinePartRanges
   */
  def getRangeModifiedPart(part: MachinePartRanges, categoryToModify: String, min: Int, max: Int): MachinePartRanges = {
    val newInclusiveRange = InclusiveRange(min, max)
    categoryToModify match {
      case "x" =>
        MachinePartRanges(Map(
          "x" -> newInclusiveRange,
          "m" -> part.categories.find(_._1 == "m").get._2,
          "a" -> part.categories.find(_._1 == "a").get._2,
          "s" -> part.categories.find(_._1 == "s").get._2))
      case "m" =>
        MachinePartRanges(Map(
          "x" -> part.categories.find(_._1 == "x").get._2,
          "m" -> newInclusiveRange,
          "a" -> part.categories.find(_._1 == "a").get._2,
          "s" -> part.categories.find(_._1 == "s").get._2))
      case "a" =>
        MachinePartRanges(Map(
          "x" -> part.categories.find(_._1 == "x").get._2,
          "m" -> part.categories.find(_._1 == "m").get._2,
          "a" -> newInclusiveRange,
          "s" -> part.categories.find(_._1 == "s").get._2))
      case "s" =>
        MachinePartRanges(Map(
          "x" -> part.categories.find(_._1 == "x").get._2,
          "m" -> part.categories.find(_._1 == "m").get._2,
          "a" -> part.categories.find(_._1 == "a").get._2,
          "s" -> newInclusiveRange))
    }
  }

}

/**
 *  A rule representing a condition of "greater than"
 * @param category category on which the rule is applied
 * @param compareVal comparison constant
 * @param nextStep nextStep if the rule is met
 */
class GreaterThanRule (category: String, compareVal:Int, nextStep: String) extends Rule (nextStep) {
  override def applyRule(part: MachinePart): Boolean = {
    part.categories(category) > compareVal
  }

  override def getRuleName: String = {"GREATER_THAN"}

  override def getSuccessRange(currentPartRange: MachinePartRanges): MachinePartRanges = {
    getRangeModifiedPart(currentPartRange, this.category, compareVal + 1, currentPartRange.categories.find(_._1 == this.category).get._2.max)
  }

  override def getFailureRange(currentPartRange: MachinePartRanges): MachinePartRanges = {
    getRangeModifiedPart(currentPartRange, this.category, currentPartRange.categories.find(_._1 == this.category).get._2.min, compareVal)
  }

  override def toString: String = "(" + category + " > " + compareVal + ")" + nextStep

}

/**
 *  A rule representing a condition of "less than"
 * @param category category on which the rule is applied
 * @param compareVal comparison constant
 * @param nextStep nextStep if the rule is met
 */
class LessThanRule (category: String, compareVal:Int, nextStep: String) extends Rule (nextStep) {
  override def applyRule(part: MachinePart): Boolean = {
    part.categories(category) < compareVal
  }

  override def getRuleName: String = {"LESS_THAN"}

  override def getSuccessRange(currentPartRange: MachinePartRanges) : MachinePartRanges = {
    getRangeModifiedPart(currentPartRange, this.category, currentPartRange.categories.find(_._1 == this.category).get._2.min, compareVal - 1)
  }

  override def getFailureRange(currentPartRange: MachinePartRanges): MachinePartRanges = {
    getRangeModifiedPart(currentPartRange, this.category, compareVal, currentPartRange.categories.find(_._1 == this.category).get._2.max)
  }

  override def toString: String = "(" + category + " < " + compareVal + "): " + nextStep

}

/**
 * A rule representing acceptance. This rule simply means the machine part on which it operates is accepted
 */
class AcceptRule extends Rule ("ACCEPT") {
  override def applyRule(part: MachinePart): Boolean = {
    true
  }

  override def getRuleName: String = {"ACCEPT"}

  override def toString: String = " (ACCEPT) "

}

/**
 * A rule representing rejection. This rule simply means the machine part on which it operates is rejected
 */
class RejectRule extends Rule ("REJECT"){
  override def applyRule(part: MachinePart): Boolean = {
    false
  }

  override def getRuleName: String = {"REJECT"}
  override def toString: String = " (REJECT) "
}

/**
 * A rule representing continue, typically when the next step is a workflow to which processiinng needs to continue
 * @param nextStep nextStep of the rule, when condition is met
 */
class continueNextRule (nextStep: String) extends Rule (nextStep) {
  override def applyRule(part: MachinePart): Boolean = {
    true
  }

  override def getRuleName: String = {"CONTINUE"}
  override def toString: String = " (CONTINUE) " + nextStep

}
