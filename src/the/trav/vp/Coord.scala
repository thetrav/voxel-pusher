package the.trav.vp

case class Coord(x:Int, y:Int) {
  def *(s:Int) = Coord(x*s, y*s)
  def /(s:Int) = *(1/s)

  def +(o:Coord) = Coord(x+o.x, y+o.y)
  def -(o:Coord) = Coord(x-o.x, y-o.y)

  def snap(i:Int) = Coord(snapInt(x,i), snapInt(y,i))
  private def snapInt(i:Int, s:Int) = i - (i%s)
}