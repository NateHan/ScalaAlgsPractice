package NStepsProblem

object Runner {

  val randGen = scala.util.Random
  var comboStore: Array[BigInt] = _

  def main(args: Array[String]) = {
    val start = 2
    val rnd = new scala.util.Random
    val numOfStairs = start + rnd.nextInt( (100 - start) + 1 )
    println(s"We are in a building of $numOfStairs steps!")

    // recursively can be painful for a slow computer, be warned
    println(s"\n \n Calculated recursively, there are ${calcStepsRecursive(numOfStairs)}" +
    s" number of combinations to climb $numOfStairs stairs. It took $totalCallsRecursive method calls.")

    // initialize array
    comboStore = new Array[BigInt](numOfStairs + 1)
    for (i <- 2 to numOfStairs) {
      comboStore(i) = 0
    }

    println(s"\n \n Calculated recursively with memoization, there are ${calcStepsMemoized(numOfStairs)}" +
      s" number of combinations to climb $numOfStairs stairs. It took $totalCallsMemoized method calls.")
  }

  var totalCallsRecursive = 0

  def calcStepsRecursive(numSteps: BigInt): BigInt = {
    totalCallsRecursive += 1
    if (numSteps == 0 || numSteps == 1) {
      1
    } else {
      calcStepsRecursive(numSteps - 1) + calcStepsRecursive(numSteps - 2)
    }
  }

  var totalCallsMemoized = 0

  def calcStepsMemoized(numSteps: Int): BigInt = {
    totalCallsMemoized += 1
    if (numSteps < 2) {
      return 1
    } else {
      if (comboStore(numSteps) == 0) {
        comboStore(numSteps) = calcStepsMemoized(numSteps - 1) + calcStepsMemoized(numSteps - 2)
      }
      return comboStore(numSteps)
    }
  }

}
