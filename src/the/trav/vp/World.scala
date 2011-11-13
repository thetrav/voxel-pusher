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

  def cleanEdges {
    discovered.map((t) => {
      val n = orthagonalNeighbors(t._1)
      if(n.filterNot((n) => discovered.contains(n)).isEmpty){
        t._2 match {
          case Open => {
            if (n.filter((c) => discovered(c) == Open).isEmpty) {
              discovered.put(t._1, Dirt)
            }
          }
          case _ => {
            if (n.filter((c) => discovered(c) != Open).isEmpty) {
              discovered.put(t._1,Open)
            }
          }
        }
      }
    })
  }

  def growAll(edges:Iterable[Coord]) {
    var next = edges
    while(!next.isEmpty) next = grow(next)
    cleanEdges
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

  def dirToCoord(c:Coord,  d:Direction) = c + d.unitVector*tileSize

  def neighborCoords(c:Coord) = {
    Set[Direction](North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest).map(dirToCoord(c, _))
  }

  def orthagonalNeighbors(c:Coord) = {
    Set[Direction](North, East, South, West).map(dirToCoord(c, _))
  }
}