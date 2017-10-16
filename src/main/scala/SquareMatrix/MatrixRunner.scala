package SquareMatrix

object MatrixRunner {

  val randomGenerator = new scala.util.Random
  val sizeLimit = 5
  val matrixSize = 5 // randomGenerator.nextInt(sizeLimit)
  val matrix = Array.ofDim[Int](matrixSize,matrixSize)

  def main (args: Array[String]): Unit = {
    populateMatrix()
    val(x1, y1) = (1, 1)
    val(x2, y2) = (2, 2)
    val square:Square = makeSquare(x1, y1, x2, y2)
    for (row <- matrix) {
      for (elem <- row ) {
        print(elem + "   ")
      }
      println("")
    }
    println("Finding sum of this square within original matrix: ")
    square.prettyPrint()
    println(s"The sum of the square is: ${getSumOfSquarePointsIterative(square)}")

    val sumMatrix: Array[Array[Int]] = generateSumMatrix(matrix)
    println("Sum matrix:")
    for (row <- sumMatrix) {
      for (elem <- row ) {
        print(elem + "   ")
      }
      println("")
    }


    println(s"Done using the sum matrix: ${getSumOfSquareUsingSumMatrix(sumMatrix, square)}")

  }

  /**
    * Get the sum of the values within the square from the sum matrix created from the original matrix
    * @param sumMatrix the sum matrix where each (x,y) is the sum of the old matrix from (0,0) to (x,y) value
    * @param square the sub-section of the original matrix we are calculating
    * @return the sum of the square we passed
    */
  private def getSumOfSquareUsingSumMatrix(sumMatrix:Array[Array[Int]], square:Square): Int = {
    val(bl, br,tl,tr) = (sumMatrix(square.lowerRow)(square.leftCol-1),
      sumMatrix(square.lowerRow)(square.rightCol),
      sumMatrix(square.upperRow-1)(square.leftCol-1),
      sumMatrix(square.upperRow-1)(square.rightCol))
      // return the bottom right square(the whole area)
      // minus the bottom left square and the top right square
      // re-add the top left because you've subtracted it twice
      println(s"$br - $bl - $tr + $tl")
      br - bl - tr + tl
  }

  /**
    * method which takes the original square and adds up all the points between its dimensions
    * @param square the square within the matrix we are searching
    * @return the sum of the points within the matrix
    */
  private def getSumOfSquarePointsIterative(square:Square): Int = {
    val allValues = for {
      i <- square.leftCol to square.rightCol
      j <- square.upperRow to square.lowerRow
    } yield matrix(i)(j)
    allValues.sum
  }

  private def generateSumMatrix(origMatrix:Array[Array[Int]]): Array[Array[Int]] = {
    val size = origMatrix.length
    val sumMatrix = Array.ofDim[Int](size, size)

    //copy first row
    for ( i <- 0 until size) sumMatrix(0)(i) = origMatrix(0)(i)

    // go through and grab each orig value plus prev row of SumMatrix value
    for (i <- 1 until size) {
      for ( j <- 0 until size) {
        sumMatrix(i)(j) = origMatrix(i)(j) + sumMatrix(i-1)(j)
      }
    }

    // go through and add previous sum matrix's row to the current value
    for (i <- 0 until size) {
      for ( j <- 1 until size) {
        sumMatrix(i)(j) += sumMatrix(i)(j-1)
      }
    }
    sumMatrix
  }


  /**
    * Analyzes the two sets of coordinates and determines which one is which for the square
    * @param x1
    * @param y1
    * @param x2
    * @param y2
    * @return a square container object with its dimensions outlined.
    */
  private def makeSquare(x1:Int, y1:Int, x2:Int, y2:Int): Square = {
    var(leftCol, rightCol) = if(x1 < x2) (x1,x2) else (x2, x1)
    var (upperRow, lowerRow) = if (y1 < y2) (y1, y2) else (y2, y1)
    Square(upperRow, lowerRow, leftCol, rightCol)
  }

  /**
    * Fills out array with values
    */
  private def populateMatrix()= {
    for {
         i <- 0 until matrixSize
         j <- 0 until matrixSize
       } {
      matrix(i)(j) = randomGenerator.nextInt(10)
    }
  }

}

case class Square(upperRow:Int, lowerRow:Int, leftCol:Int, rightCol:Int) {

  def prettyPrint() = { println(s"Upper Row:$upperRow, Lower Row:$lowerRow, Left Row:$leftCol, Right Row:$rightCol.")}
}


