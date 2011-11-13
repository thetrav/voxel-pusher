package the.trav.vp

import collection.mutable.Map

object World {
  var pickTerrain = (c:Coord) => if (math.random > 0.3) Dirt else Open //exposed as var for testing only
  var discovered = Map[Coord, Terrain]()

  val tileSize = 30

  val startPoint = Coord(0,0)
  val startRadius = 5

  def worldFromIndex(x:Int, y:Int) = Coord(x*tileSize, y*tileSize)

  def init {
    for( x <- (-startRadius to startRadius);
         y <- (-startRadius to startRadius)
    ) {
      discovered.put(worldFromIndex(x,y), Open)
    }

    growEdges
  }

  def growEdges {
    growAll(edgesToGrow)
  }

  def growAll(edges:Iterable[Coord]) {
    var next = edges
    while(!next.isEmpty) next = grow(next)
  }

  def grow(edges:Iterable[Coord]) = {
    val opens:Iterable[Coord] = edges.flatMap((c:Coord) => {
      val t = pickTerrain(c)
      discovered.put(c, t)
      t match {
        case Open => Some(c)
        case _ => None
      }
    })
    opens.flatMap(missingNeighbors(_)).toSet
  }

  def edgesToGrow = {
    discovered.flatMap(
      (t:(Coord,  Terrain)) => if(t._2 == Open) {Some(missingNeighbors(t._1))} else None
    ).flatten.toSet
  }

  def missingNeighbors(c:Coord) = {
    neighborCoords(c).filter (!discovered.contains(_))
  }

  def neighborCoords(c:Coord) = {
    val f = (n:Direction) => {c + n.unitVector*tileSize}
    val ns = Set[Direction](North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest)
    ns.map(f)
  }
}