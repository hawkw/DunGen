package dungen

/**
 * Created by hawk on 12/30/14.
 */
class Generator(
                 val maxRooms: Int = 20, // max number of room tries
                 val extraSize: Int = 0   // increase this for larger rooms
                 ) {
  val rand = new java.util.Random

  /**
   * Generate a 2D array of booleans of the specified size. True = empty, false = wall.
   * @param x the max x-dimension for the dungeon grid
   * @param y the max y-dimension for the dungeon grid
   * @return a 2D array of booleans of size (x,y)
   */
  def generate(x: Int, y: Int): Array[Array[Boolean]] = {
    val rooms = makeRooms(Array.ofDim[Int](x,y))
    rooms.map(slice => slice.map(i => if (i > 0) true else false ))
  }

  /**
   * Carve out rooms on a grid. Each room is colored uniquely.
   *
   * Rooms are colored uniquely 'cause I could implement map coloring here but it would make the algorithm much more complex, and just seems unnecessary.
   * @param grid
   * @return
   */
  protected def makeRooms(grid: Array[Array[Int]]): Array[Array[Int]] = {
    for { i <- 1 to maxRooms } {
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
        if grid(x)(y) == 0
      } {
        grid(x)(y) = i
      }
    }
    grid
  }
}
