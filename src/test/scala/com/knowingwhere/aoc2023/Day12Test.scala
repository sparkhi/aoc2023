package com.knowingwhere.aoc2023

import org.scalatest.Matchers._
import org.scalatest._

class Day12Test extends WordSpec with BeforeAndAfterEach {
  "Day 12" should {
    "Pattern where sum of the spring pattern adds up to the number sequence and gaps" in {
      //1 1 3 => indicates there has to be 5 damaged spring with each group separated by a dot
      //hence total count = 1 + 1 + 3 + 2(dots) = 7 = length of "???.###"
      Day12.ruleOfExactCount("???.###", List(1,1,3)).get shouldBe 1
    }
    "Pattern where the separation by dot results in just one variation" in {
      Day12.ruleOfSplitOnDotToMatchExactPattern("????.#...#...", List(4,1,1)).get shouldBe 1
    }

    //NOT IMPLEMENTED YET

//    "Trial and error for patterns " in {
//      Day12.ruleOfTryingOutAllPossibilitiesRecursively("???.?", List(2,1)).get shouldBe 2
//      Day12.ruleOfTryingOutAllPossibilitiesRecursively(".??..??...?##.", List(1,1,3)).get shouldBe 4
//      Day12.ruleOfTryingOutAllPossibilitiesRecursively("....??..??...?##.", List(1,1,3)).get shouldBe 4
//    }
    "calling all rules on the same patterns as previous individual tests" in {
      Day12.getArrangementVariationsCount("???.### 1,1,3") shouldBe 1
      Day12.getArrangementVariationsCount("????.#...#... 4,1,1") shouldBe 1
    }
  }
}
