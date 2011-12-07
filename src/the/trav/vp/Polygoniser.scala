package the.trav.vp

object Polygoniser {
  
  var tileSize = World.tileSize

  trait Facing {
    def left(c:Coord):Coord
    def right(c:Coord):Coord
    def up(c:Coord) = right(c)
    def turnLeft:Facing
    def turnRight:Facing
  }

  //0 degrees
  case object North extends Facing {
    def left(c:Coord) = c + Coord(-tileSize, tileSize)
    def right(c:Coord) = c + Coord(0, tileSize)
    def turnLeft = West
    def turnRight = East
  }

  //-90 degrees
  case object East extends Facing {
    def left(c:Coord) = c + Coord(tileSize, tileSize)
    def right(c:Coord) = c + Coord(tileSize, 0)
    def turnLeft = North
    def turnRight = South
  }

  //180 degrees
  case object South extends Facing {
    def left(c:Coord) = c + Coord(tileSize, -tileSize)
    def right(c:Coord) = c + Coord(0, -tileSize)
    def turnLeft = East
    def turnRight = West
  }

  //90 degrees
  case object West extends Facing {
    def left(c:Coord) = c + Coord(-tileSize, -tileSize)
    def right(c:Coord) = c + Coord(-tileSize, 0)
    def turnLeft = South
    def turnRight = North
  }

  def pickTurn(left:Terrain, right:Terrain, rotation:Facing) = {
    if(left != Open && right == Open) {
      rotation
    } else if (left != Open && right != Open) {
      rotation.turnRight
    } else { //(left == Open)
      rotation.turnLeft
    }
  }
  
  def shape = {
    def leftIsBlocked(c:Coord) = {
      val l = North.left(c) - Coord(0,tileSize)
//      println("c:"+c+" l:"+l)
      World.discovered.contains(l) && World.discovered(l) != Open
    }

    //find a left edge
    val start = World.discovered.filter(t => (t._2 == Open && leftIsBlocked(t._1))).head._1

    def buildList(accum:List[Coord], rotation:Facing, current:Coord):List[Coord] = {
      if(current == start && accum.size > 1) {
        accum
      } else {
        try {
          val nextPos = rotation.up(current)
          val l = rotation.left(current)
          val r = rotation.right(current)
          val topLeft = World.discovered(l)
          val topRight = World.discovered(r)
          println ("\nat "+current+" rotation="+rotation+ "\n\tl="+l+"\n\tr="+r+"\n\ttl"+topLeft+"\n\ttr" + topRight+ "\n\tnext:"+nextPos+"\n\t\tlist="+accum)
          val nextRotation = pickTurn(topLeft, topRight, rotation)

          buildList(current :: accum, nextRotation, nextPos)
        } catch {
          case _ => println("crash!");current :: accum
        }
      }
    }

    buildList(List(), North, start)
  }

  def main(args:Array[String]) {
    tileSize = 1

    def test(c:Coord, r:Facing, left:Terrain, right:Terrain, er:Facing, ec:Coord) {
      val nr = pickTurn(left,right,r)
      val nc = nr.up(c)
      println("from c:"+c+" r:"+r+" left:"+left+" right:"+right+" got nr:"+nr+" nc:"+nc+" expect er:"+er+" ec:"+ec)
      assert(nr == er)
      assert(nc == ec)
    }

    test(Coord(0,0), North, Dirt, Open, North, Coord(0,1))
    test(Coord(0,0), North, Dirt, Dirt, East, Coord(1,0))
    test(Coord(0,0), North, Open, Open, West, Coord(-1,0))
    test(Coord(0,0), North, Open, Dirt, West, Coord(-1,0))

    test(Coord(0,0), East, Dirt, Open, East, Coord(1,0))
    test(Coord(0,0), East, Dirt, Dirt, South, Coord(0,-1))
    test(Coord(0,0), East, Open, Open, North, Coord(0,1))
    test(Coord(0,0), East, Open, Dirt, North, Coord(0,1))

    test(Coord(0,0), South, Dirt, Open, South, Coord(0,-1))
    test(Coord(0,0), South, Dirt, Dirt, West, Coord(-1,0))
    test(Coord(0,0), South, Open, Open, East, Coord(1,0))
    test(Coord(0,0), South, Open, Dirt, East, Coord(1,0))

    test(Coord(0,0), West, Dirt, Open, West, Coord(-1,0))
    test(Coord(0,0), West, Dirt, Dirt, North, Coord(0,1))
    test(Coord(0,0), West, Open, Open, South, Coord(0,-1))
    test(Coord(0,0), West, Open, Dirt, South, Coord(0,-1))
  }
}