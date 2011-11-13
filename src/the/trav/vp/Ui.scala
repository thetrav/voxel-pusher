package the.trav.vp

import javax.swing.{JPanel, JFrame}
import java.awt.{Color, Graphics2D, Graphics}


class Ui {

  val width = 1024
  val height = 768

  val frame = new JFrame("VoxelPusher")

  val panel = new JPanel() {
    override def paint(g:Graphics) {
      renderUi(g.asInstanceOf[Graphics2D])
    }
  }

  def run {
    frame.setSize(width, height)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

    frame.getContentPane().add(panel)

    frame.setVisible(true)
  }

  def renderUi(g:Graphics2D) {
    g.setColor(Color.black)
    g.fillRect(0,0,width, height)

    g.translate((width*0.5), (height*0.5))

    val colors = Map[Terrain,  Color]((Open -> Color.lightGray), (Dirt -> Color.darkGray))

    World.discovered.map((p:(Coord, Terrain)) => {
      val c = p._1
      val t = p._2

      g.setColor(colors(t))
      g.fillRect(c.x, c.y, World.tileSize, World.tileSize)
      g.setColor(Color.white)
      g.drawRect(c.x, c.y, World.tileSize, World.tileSize)
    })
  }
}