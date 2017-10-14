package BalloonDrop

object DropperRunner {

  val buildingHeight = 100000
  val balloonStrength = scala.util.Random.nextInt(50)

  def main(args: Array[String]) = {

    println(s"Iteratively calculated the max height the balloon could be dropped from to be: " +
      s"${iterCalcMaxBalloonDrop(buildingHeight)} and it should be:  $balloonStrength. " +
      s"This took $iterativeMethodCalls calls")

    println(s"LogN Calculated the max height the balloon could be dropped from to be: " +
      s"${logNCalcBalloonDrop(buildingHeight, buildingHeight, 0)} and it should be:  $balloonStrength. " +
      s"This took $logNCalls calls")

  }

  var iterativeMethodCalls = 0

  def iterCalcMaxBalloonDrop(dropHeight: Int): Int = {
    iterativeMethodCalls += 1
    if (dropHeight == 0) return dropHeight
    val balloon = BalloonFactory.makeBalloon(balloonStrength)
    balloon.dropFrom(dropHeight)
    if (balloon.popped) iterCalcMaxBalloonDrop(dropHeight - 1) else return dropHeight
  }

  var logNCalls = 0

  /**
    *
    * @param dropHeight the height we are dropping the balloon from
    * @param lbh        Lowest Break Height: the lowest height we know that the balloon will break
    * @param hsh        Highest Safe Height: the highest height we know that the balloon will not break
    * @return The highest height we can drop the balloon from
    */
  def logNCalcBalloonDrop(dropHeight: Int, lbh: Int, hsh: Int): Int = {
    logNCalls += 1
    val balloon = BalloonFactory.makeBalloon(balloonStrength)
    balloon.dropFrom(dropHeight)
    if (!balloon.popped && (lbh - hsh) == 1) {
      return hsh
    }
    else if (balloon.popped) {
        logNCalcBalloonDrop(dropHeight / 2, dropHeight, hsh) // drop from half the height, store values
      } else {
        logNCalcBalloonDrop(lbh-((lbh-dropHeight)/2), lbh, dropHeight) // drop from half the distance to highest, store
      }
  }
}

object BalloonFactory {

  def makeBalloon(maxDropHeight: Int): Balloon = {
    new Balloon(maxDropHeight)
  }
}

case class Balloon(maxDropHeight: Int, var popped: Boolean = false) {

  def dropFrom(fromHeight: Int) {
    if (fromHeight > maxDropHeight) popped = true
  }
}
