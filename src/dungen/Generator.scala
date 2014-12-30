package dungen

import dungen._

import scala.annotation.tailrec

/**
 * A quick dungeon-grid generator.
 * Loosely based on the algorithm described in this blog post: [[http://journal.stuffwithstuff.com/2014/12/21/rooms-and-mazes/]]
 *
 * @author Hawk Weisman
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
    var rooms = makeRooms(Array.ofDim[Int](x,y))
    rooms = makeMazes(rooms)
    rooms.map(slice => slice.map(i => if (i != 0) true else false ))
  }

  def makeMazes(grid: Array[Array[Int]]): Array[Array[Int]] = {
    floodfill(0,0)
    @tailrec def floodfill(x: Int, y: Int): Unit = grid(x)(y) match {
      case i if i > 0 || i == -1 => {}//already filled
      case 0 =>
        grid(x)(y) = -1
        rand.nextInt(2) match {
          case 0 => floodfill(if(x + 1 < grid(0).length - 1) x + 1 else x, y)
          case 1 => floodfill(x, if(y + 1 < grid.length - 1) y + 1 else y)
        }
    }
    grid
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
      val (height: Int,width: Int) = rand nextInt 3 match {
        case 0 => (size,size) // square room
        case 1 => (size + rect, size)
        case 2 => (size, size + rect)
      }
      val ypos = rand.nextInt(grid.length - height -1) //* 2 + 1
      val xpos = rand.nextInt(grid(0).length - width - 1)// * 2 + 1
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
object Generator {
  /**
   * Crappy main method for debugging
   * @param argv
   */
  def main(argv: Array[String]): Unit = "\n" + prettyPrint(new Generator().generate(50,50))
}