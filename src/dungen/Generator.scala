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
    var (grid: Array[Array[Int]], rooms) = makeRooms(Array.ofDim[Int](x,y))
    grid = makeMazes(grid)
    grid.map(slice => slice.map(i => if (i != 0) true else false ))
  }

  def makeMazes(grid: Array[Array[Int]]): Array[Array[Int]] = {
    dfs(0,0)
    def neighbors(p: Point): List[Point] = List(
      Point(p.x + 1, p.y),
      Point(p.x - 1, p.y),
      Point(p.x, p.y + 1),
      Point(p.x, p.y - 1)
      )
    @tailrec def dfs(x: Int, y: Int, visited: List[Point]=Nil, stack: List[Point]=Nil): Unit = {
      var vis = Point(x,y) :: visited
      var sta = stack
      vis.length match {
        case i if i == grid.length * grid(0).length => {}
        case _ =>
          // randomly choose a neighbor
          val eligibleNeighbors = neighbors(Point(x,y)).filter(p =>
            p.y < grid.length     &&
            p.x < grid(0).length  &&
            p.y >= 0              &&
            p.x >= 0              &&
            p.y + 1 >= 0              &&
            p.x + 1 >= 0              &&
            !vis.contains(p)
          )
          val current: Point = eligibleNeighbors.length match {
            case 0 => if (stack != Nil) {
              sta = stack.drop(1)
              stack.head
            } else {
              Point(rand.nextInt(grid(0).length), rand.nextInt(grid.length))
            }
            case i if i > 0 =>
              val doing  = eligibleNeighbors(rand.nextInt(i))
              grid(doing.y)(doing.x) = -1
              vis = doing :: vis
              doing
          }
          dfs(current.x, current.y, vis, sta)
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
  protected def makeRooms(grid: Array[Array[Int]]): (Array[Array[Int]], List[Room]) = {
    var rooms: List[Room] = List[Room]()
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
      rooms = Room(xpos,ypos,height,width) :: rooms
      for {
        y <- ypos to ypos + height
        x <- xpos to xpos + width
        if grid(x)(y) == 0
      } {
        grid(x)(y) = i
      }
    }
    (grid,rooms)
  }
}
object Generator {
  /**
   * Crappy main method for debugging
   * @param argv
   */
  def main(argv: Array[String]): Unit = "\n" + prettyPrint(new Generator().generate(50,50))
}