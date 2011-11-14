package the.trav.vp



object Polygoniser {
  def shape = {
    val first =World.discovered.first
    println("first:"+first)
    val c = first._1
    val t = first._2

    val neighbors = World.neighborCoords(c).filter(World.discovered.get(_)== Some(t)).toSet


    def fill(edge:Iterable[Coord], existing:Set[Coord]):Iterable[Coord] = {
      if(edge.isEmpty) {
        existing
      } else {
        val extend = edge.flatMap(World.neighborCoords(_)).filter((c) => World.discovered.get(c) == Some(t) && !existing.contains(c)).toSet
        fill(extend, existing ++ extend)
      }
    }

    val all = fill(neighbors, neighbors)

    val edge = all.filter(!World.neighborCoords(_).filter(World.discovered.get(_) != Some(t)).isEmpty)

    edge.toList.sortWith((a, b) => (a.y > b.y) || (a.y == b.y && a.x > b.x))
  }
}