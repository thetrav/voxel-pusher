package the.trav.vp



object Main {
  def main(args:Array[String]) {
    World.init
    println("world:"+World.discovered)

    new Ui().run
  }
}