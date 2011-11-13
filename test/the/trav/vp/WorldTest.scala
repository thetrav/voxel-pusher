package the.trav.vp

import org.scalatest.FunSuite


class WorldTest extends FunSuite {

  val c = (x:Int, y:Int) => Coord(x,y)
  val lay = (x:Int, y:Int, t: Terrain) => World.discovered.put(c(x,y), t)
  val open = (x:Int, y:Int) => lay(x, y, Open)
  val dirt = (x:Int, y:Int) => lay(x, y, Dirt)

  def testCoords(set:Iterable[Coord], expected:Iterable[Coord], missing:Iterable[Coord] = Set[Coord]()) {
    try {
      println("size check")
      assert(set.size == expected.size)
      for(e <- expected) {
        println("testing " + e + " exists")
        assert(set.exists(_ == e))
      }
      for(m <- missing) {
        println("testing " + m + " missing")
        assert(!set.exists(_ == m))
      }
    } catch {
      case e => {
        println("error comparing:\n"+set+"\n size " + set.size + " expected size "+expected.size + "\n elements "+expected +"\n explicitly missing" + missing)
        throw e
      }
    }
  }

  test ("correct neighbors are generated") {
    println("testing correct neighbors are generated")
    val neighbors = World.neighborCoords(c(0, 0))
    val expected = Set(c(0, -30), c(30,-30), c(30,0), c(30,30), c(0,30), c(-30,30), c(-30,0), c(-30,-30))
    testCoords(neighbors, expected)
  }

  test("missing neighbors are detected") {
    println("testing missing neighbors are detected")
    World.discovered = World.discovered.empty
    open(0,0)
    testCoords(World.missingNeighbors(c(0,0)), Set(c(0, -30), c(30,-30), c(30,0), c(30,30), c(0,30), c(-30,30), c(-30,0), c(-30,-30)))

    open(30,0)
    testCoords(World.missingNeighbors(c(0,0)), Set(c(0, -30), c(30,-30), c(30,30), c(0,30), c(-30,30), c(-30,0), c(-30,-30)), Set(c(30,0)))
  }

  test("edges to grow are detected") {
    println("testing edges to grow are detected")
    World.discovered = World.discovered.empty
    open(0,0)
    testCoords(World.edgesToGrow, Set(c(0, -30), c(30,-30), c(30,0), c(30,30), c(0,30), c(-30,30), c(-30,0), c(-30,-30)))

    open(30,0)
    testCoords(World.edgesToGrow, Set(c(0, -30), c(30,-30), c(30,30), c(0,30), c(-30,30), c(-30,-30), c(60,30), c(60,0), c(60,-30), Coord(-30,0)))
  }

  test("dirt is ignored") {
    println("testing dirt is ignored")
    World.discovered = World.discovered.empty
    dirt(0,0)
    assert(World.edgesToGrow.isEmpty)
  }

  test ("grow edges") {
    println ("testing edge growing")
    World.discovered = World.discovered.empty
    open(0,0)

    val oldFn = World.pickTerrain
    World.pickTerrain = (p:Coord) => if( p == c(30,0)) Open else Dirt

    World.growEdges

    try {
      def a = (x:Int,  y:Int, t:Terrain) => assert(World.discovered(c(x,y)) == t)
      def o = (x:Int,  y:Int) => a(x, y, Open)
      def d = (x:Int,  y:Int) => a(x, y, Dirt)

      o(0,0)
      o(30,0)

      d(-30,-30)
      d(-30, 0)
      d(-30, 30)
      d(0,-30)
      d(0, 30)
      d(30,-30)
      d(30, 30)
      d(60,-30)
      d(60, 0)
      d(60, 30)

      assert(World.discovered.size == 12)
    } catch {
      case e => {
        println("error with world " + World.discovered)
        throw e
      }
    } finally {
      World.pickTerrain = oldFn
    }
  }

}