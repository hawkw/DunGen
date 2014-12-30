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
    var grid = Array.ofDim[Int](x,y) // start with an array of zeros
    val rooms = makeRooms(grid)
    ???
  }

  def makeRooms(grid: Array[Array[Int]]): Array[Array[Int]] = {
    for {
      i <- 1 to maxTries
    } {
      val size = (rand.nextInt(2 + extraSize) + 1) * 2 + 1
      val rect = rand.nextInt(1 + size / 2) * 2
      val (height: Int,width: Int) = rand nextInt 2 match {
        case 0 => (size,size) // square room
        case 1 => (size + rect, size)
        case 2 => (size, size + rect)
      }

      val ypos = rand.nextInt(grid.length - height) //* 2 + 1
      val xpos = rand.nextInt(grid(0).length - width)// * 2 + 1
      for {
        y <- ypos to ypos + height
        x <- xpos to xpos + width
      } {
        if (grid(x)(y) == 0) {
          println(s"Setting ($x,$y)")
          grid(x)(y) = i
        } else {
          println(s"($x,$y) = ${grid(x)(y)}")
        }

      }
    }
    grid
  }
}
