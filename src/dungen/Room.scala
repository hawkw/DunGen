package dungen

/**
 * Created by hawk on 12/30/14.
 */
case class Room (xpos: Int, ypos: Int, height: Int, width: Int) {
  def in(x: Int, y: Int): Boolean = x >= width && y >= height
  def in(p: Point): Boolean = in (p.x, p.y)

}
