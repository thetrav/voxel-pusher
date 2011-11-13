package the.trav.vp



trait Direction {
  def unitVector:Coord
}

case object North extends Direction {
  def unitVector = Coord(0,1)
}

case object South extends Direction {
  def unitVector = Coord(0,-1)
}
case object East extends Direction{
  def unitVector = Coord(1,0)
}
case object West extends Direction{
  def unitVector = Coord(-1,0)
}

case object NorthWest extends Direction{
  def unitVector = Coord(-1,1)
}
case object NorthEast extends Direction{
  def unitVector = Coord(1,1)
}
case object SouthWest extends Direction{
  def unitVector = Coord(-1,-1)
}
case object SouthEast extends Direction{
  def unitVector = Coord(1,-1)
}