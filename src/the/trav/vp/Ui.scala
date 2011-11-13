package the.trav.vp

import javax.swing.{JPanel, JFrame}
import java.awt.{Color, Graphics2D, Graphics}
import java.awt.event._
import java.awt.geom.AffineTransform


class Ui {

  val width = 1024
  val height = 768

  var mousePos = Coord(0,0)
  var camPos = Coord(width/2,height/2)

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

    panel.addMouseListener(new MouseAdapter() {
      override def mouseClicked(e:MouseEvent) {
        World.discovered.put(mousePos, Open)
        World.growAll(World.missingNeighbors(mousePos))
        panel.invalidate()
        panel.repaint()
      }
    })

    panel.addMouseMotionListener(new MouseMotionAdapter(){
      override def mouseMoved(e:MouseEvent) {
        mousePos = (Coord(e.getX(), e.getY()) - camPos).snap(World.tileSize)
        panel.invalidate()
        panel.repaint()
      }
    })

    frame.addKeyListener(new KeyAdapter() {
      override def keyPressed(e:KeyEvent) {
        val dir = e.getKeyCode() match {
          case KeyEvent.VK_UP => North
          case KeyEvent.VK_DOWN => South
          case KeyEvent.VK_LEFT => East
          case KeyEvent.VK_RIGHT => West
        }
        camPos += (dir.unitVector*World.tileSize)
        panel.invalidate()
        panel.repaint()
      }
    })

    frame.setVisible(true)
  }

  def renderUi(g:Graphics2D) {
    g.setColor(Color.black)
    g.fillRect(0,0,width, height)

    g.translate(camPos.x, camPos.y)

    val colors = Map[Terrain,  Color]((Open -> Color.lightGray), (Dirt -> Color.darkGray))

    World.discovered.map((p:(Coord, Terrain)) => {
      val c = p._1
      val t = p._2

      g.setColor(colors(t))
      g.fillRect(c.x, c.y, World.tileSize, World.tileSize)
      g.setColor(Color.white)
      g.drawRect(c.x, c.y, World.tileSize, World.tileSize)

      g.setColor(Color.red)
      g.drawRect(mousePos.x, mousePos.y, World.tileSize, World.tileSize)
    })

    g.setTransform(new AffineTransform())
    g.setColor(Color.green)
    g.drawString("camPos:"+camPos, 10,10)
  }
}