package dungen

/**
 * Created by hawk on 12/30/14.
 */
class Generator(
                 val maxTries: Int = 20, // max number of room tries
                 val extraSize: Int = 0   // increase this for larger rooms
                 ) {
  val rand = new java.util.Random

  def generate(x: Int, y: Int): Array[Boolean] = {
    val grid = Array.ofDim[Int](x,y) // start with an array of zeros
    val rooms = rooms(grid, (x,y))
    ???
  }

  private def rooms(grid: Array[Array[Int]], bounds: (Int, Int)): Array[Array[Int]] = {
    for {
      i <- 1 to maxTries
    } {
      val size = (rand.nextInt(2 + extraSize) + 1) * 2 + 1
      val rect = rand.nextInt(1 + size / 2) * 2
      val (height,width) = rand nextInt 2 match {
        case 0 => (size,size) // square room
        case 1 => (size + rect, size)
        case 2 => (size, size + rect)
      }
      val xpos = rand.nextInt(bounds._1 - width) * 2 + 1
      val ypos = rand.nextInt(bounds._2 - height) * 2 + 1
      for {
        y: Int <- height
        x: Int <- width
        pos: Int <- grid(ypos + y)(xpos + x) if pos == 0
      } {
        pos == i
      }
    }
    grid
  }

}
