/**
 * Created by hawk on 12/30/14.
 */
package object dungen {
  def prettyPrint(grid: Array[Array[Int]]) = for {
    y <- 0 to grid.length -1
    x <- 0 to grid(y).length -1
  } {
    print(s"${grid(y)(x)}\t${if (x == grid(y).length-1){"\n"}else{""}}")
  }
  def prettyPrint(grid: Array[Array[Boolean]]) = for {
    y <- 0 to grid.length -1
    x <- 0 to grid(y).length -1
  } {
    print(
      (grid(x)(y) match {
        case true => "."
        case false => "#"
      }) + (if (x == grid(y).length-1) "\n" else "")
    )
  }
}
