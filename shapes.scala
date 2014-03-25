import javax.swing._
import java.awt._
import java.awt.event._
import scala.language.implicitConversions
import java.awt.geom._
import scala.List
import util.Random.nextInt
import scala.math

/*
*	Stores info about shapes
*/
class MyShape {
	var shape:Int = 0
	var x:Int = 0
	var y:Int = 0
	var size:Int = 0
	var color:Color = Color.black
  var up = true
  var left = true
	// setVisible(true)
	def paintComponents(g:Graphics){}
	def hit(xH:Int, yH:Int):Boolean = {
		if((x to x+size contains xH) && (y to y+size contains yH)){
			println("hit!!!")
			return true
		}
		return false
	}

}

/*
*	Keeps track of details of square and how to paint it
*/
class Square(var squareX:Int, var squareY:Int, var length:Int, val sColor:Color) extends MyShape{
	shape = 1
	x = squareX
	y = squareY
	size = length
	color = sColor
	
  override def paintComponents(g:Graphics){
    g.setColor(color)
    g.fillRect(x, y, size, size)
  }
}


/*
*	Keeps track of details of cirle and how to paint it
*/
class Circle(var circleX:Int, var circleY:Int, var diameter:Int, val ccolor:Color) extends MyShape{
	shape = 2
	x = circleX
	y = circleY
	size = diameter
	color = ccolor
  override def paintComponents(g:Graphics){
    g.setColor(color)
    g.fillOval(x, y, size, size)
  }
}


/*
*	The Shapes class keeps an array of all of the shapes drawn on screen and what order they are in. It also stores the various event listeners 
* 	that each shape needs to function. Shapes extends the JComponent class and all the shpaes are drawn in its paintComponent function
*/
class Shapes (numShapes:Int) extends JComponent {

  //list of colors avalible to shapes
  val colors:List[Color] = List(Color.black, Color.blue, Color.cyan, Color.green, Color.magenta, Color.orange, Color.pink, Color.red, Color.yellow)
  //array of shapes
  var shapes = new  Array[MyShape](numShapes)

  // val animatoinTimer:Timer = NULL

  //creates random shapes 
  for(i<-0 until numShapes){
  	val temp = nextInt(2)
  	if(temp == 1) 
  		shapes(i) = new Circle(nextInt(500), nextInt(500), 50, colors(nextInt(colors.size)))
  	else
  		shapes(i) = new Square(nextInt(500), nextInt(500), 50, colors(nextInt(colors.size)))
  	// println("test38f")
  }

  //animates shapes when animation key ('s') is pressed
  val al = new AnimationListener(shapes, this)
  val animatoinTimer = new Timer(30, al)

  //listens for comands to resize shapes
  val kl = new TypingListener(shapes, this, animatoinTimer)
  addKeyListener(kl)
  setFocusable(true)
  requestFocus()

  //listens for clicks to select shapes
  val sl = new SelectListener(shapes, this)
  addMouseListener(sl)

  //listens for drags to move shpaes
  val dl = new DragListener(shapes, this)
  addMouseMotionListener(dl)


  setVisible(true)



  //paints all the shapes and turns antialiasing on
  override def paintComponent(g:Graphics){
  	val g2D = g.asInstanceOf[Graphics2D]
  	g2D.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
    shapes.foreach(_.paintComponents(g))
  }
}

/*
*	When a shape is pressed it is moved to the back of the shape array so that it is drawn on top
*/
class SelectListener(shapes:Array[MyShape], shape:Shapes) extends MouseAdapter {
	override def mousePressed(e:MouseEvent){
  		val hits = shapes.map(x => if(x.hit(e.getX, e.getY)) shapes.indexOf(x) else false).filter(_ != false) //returns list of indexs of hits
  		if(hits.size != 0){
   			val last = hits.last.asInstanceOf[Int] //last position of hit shape in shapes
   			val temp:MyShape = shapes(last)
   			if(shapes.last != temp){
   				for(i <- last until shapes.length-1){
   					shapes(i) = shapes(i+1)
   				}
   				shapes(shapes.length-1) = temp
   			}
   			shape.repaint()
		}
   	}
  	override def mouseReleased(e:MouseEvent){}
  	override def mouseClicked(e:MouseEvent){}
  	override def mouseEntered(e:MouseEvent){}
  	override def mouseExited(e:MouseEvent){}
}

/*
*	Moves the center of the selected shape to the current cursor position while the cursor is over the shape
*/
class DragListener(shapes:Array[MyShape], shape:Shapes) extends MouseMotionListener{
  override def mouseDragged(e:MouseEvent){
    if(shapes.last.hit(e.getX, e.getY)){
    	val pos = shapes.length-1
    	val radius = shapes(pos).size/2
    	// println(shapes(pos).x + "&" +e.getX)

    	val curX = shapes(pos).x + radius
    	if(curX > e.getX && e.getX - radius > 0) {
    		shapes(pos).x -= curX - e.getX
    	}
    	else if(e.getX - radius > 0  && e.getX + radius < shape.getSize().getWidth) {
    		shapes(pos).x += e.getX - curX
    	}

    	val curY = shapes(pos).y + radius
    	if(curY > e.getY && e.getY-radius > 0) {
    		shapes(pos).y -= curY - e.getY
    	}
    	else if(e.getY - radius > 0 && e.getY + radius < shape.getSize().getHeight) {
    		shapes(pos).y += e.getY - curY
    	}

    	shape.repaint()
    }
  }
  override def mouseMoved(e:MouseEvent){}
}

/*
* When animation is running, the shapes will bounce around from wall to wall(side of the screen)
*/
class AnimationListener(shapes:Array[MyShape], shape:Shapes) extends ActionListener{
  override def actionPerformed(e:ActionEvent){
    for(i<-0 until shapes.length){
      val cur = shapes(i)

      //does left and right animation
      if(cur.left){
        val newX = shapes(i).x - 1
        if(newX > 0)
          shapes(i).x = newX
        else
          shapes(i).left = false
      }
      else {
        val newX = shapes(i).x + 1
        if(newX + cur.size < shape.getSize().getWidth)
          shapes(i).x = newX
        else
          shapes(i).left = true
      }

      //does up and down animation
      if(cur.up){
        val newY = shapes(i).y - 1
        if(newY > 0)
          shapes(i).y = newY
        else
          shapes(i).up = false
      }
      else {
        val newY = shapes(i).y + 1
        if(newY + cur.size < shape.getSize().getHeight)
          shapes(i).y = newY
        else
          shapes(i).up = true
      }
    }

    shape.repaint()
  }
}

/*
*	Will increase or decrease the size of the last selected shape based on pressing - and = signs 
* Pressing s will start or stop the animation
*/
class TypingListener(shapes:Array[MyShape], shape:Shapes, animatoinTimer:Timer) extends KeyListener {
  var animate = false
	override def keyTyped(e:KeyEvent){}
	override def keyPressed(e:KeyEvent){
		if(e.getKeyChar == '-'){
			if(shapes.last.size >5)
				shapes.last.size -= 5
			shape.repaint()
		}
		else if(e.getKeyChar == '='){
			shapes.last.size += 5
			shape.repaint()
		}
    else if(e.getKeyChar == 's'){
      println("SSSSS")
      if(animate){
        animatoinTimer.stop
        animate = false
      }
      else{
        animatoinTimer.start
        animate = true
      }
    }
	}
	override def keyReleased(e:KeyEvent){}
}

/*
*	Creates a JFrame, and adds the shapes. 
*/
object MovingShapes {
  implicit def string2Int(s: String): Int = augmentString(s).toInt
  
  def main(args: Array[String]): Unit = {
    val frame = new JFrame
    
    var numShapes = 2
	if(args.length > 0) numShapes = string2Int(args(0))
	
	
	frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
	frame.setSize(600, 600)
	val shapes = new Shapes(numShapes)
    frame.getContentPane.add(shapes)
	
	frame.setVisible(true)
  }
}