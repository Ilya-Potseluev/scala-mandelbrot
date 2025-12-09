import java.awt.Dimension
import java.awt.event.{KeyAdapter, KeyEvent, MouseAdapter, MouseEvent}
import java.awt.image.BufferedImage
import javax.swing.{ImageIcon, JFrame, JLabel, SwingUtilities, WindowConstants}
import scala.concurrent.{ExecutionContext, Future}

val size = 1000
val title = "Множество Мандельброта"
val threads = 10
var center = Complex(-0.75, 0.0)
var viewWidth = 3.5

def showImage(label: JLabel, image: BufferedImage): Unit = {
  label.setIcon(new ImageIcon(image))
  label.repaint()
}

inline def clamp01(v: Double): Double =
  if v < 0 then 0
  else if v > 1 then 1
  else v

case class Color(r: Double, g: Double, b: Double) {
  def toInt: Int = {
    val rr = (clamp01(r) * 255).toInt
    val gg = (clamp01(g) * 255).toInt
    val bb = (clamp01(b) * 255).toInt
    (rr << 16) | (gg << 8) | bb
  }
}
case class Point(x: Double, y: Double)

case class Complex(re: Double, im: Double) {
  inline def +(other: Complex): Complex =
    Complex(re + other.re, im + other.im)
  inline def *(other: Complex): Complex =
    Complex(re * other.re - im * other.im, re * other.im + im * other.re)
  inline def abs2: Double = re * re + im * im
}

def generateImageAsync(
    shader: Point => Color
)(using ec: ExecutionContext): Future[BufferedImage] = {
  val image = new BufferedImage(size, size, BufferedImage.TYPE_INT_RGB)
  val rasterData = Array.ofDim[Int](size * size)

  val rowsPerTask = (size.toDouble / threads).ceil.toInt

  val tasks: Seq[Future[Unit]] = (0 until threads).map { t =>
    val yStart = t * rowsPerTask
    val yEnd = Math.min(size, yStart + rowsPerTask)
    Future {
      (0 until size).map(x =>
        (yStart until yEnd).foreach(y =>
          val idx = y * size + x
          rasterData(idx) = shader(Point(x, y)).toInt
        )
      )
    }
  }

  Future.sequence(tasks).map { _ =>
    image.setRGB(0, 0, size, size, rasterData, 0, size)
    image
  }
}

@main
def main(): Unit = {

  given ExecutionContext = ExecutionContext.global

  SwingUtilities.invokeLater { () =>
    val frame = new JFrame(title)
    val label = new JLabel()
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    frame.setContentPane(label)
    label.setPreferredSize(new Dimension(size, size))
    frame.pack()
    frame.setResizable(false)
    frame.setLocationRelativeTo(null)

    def render(): Future[Unit] =
      generateImageAsync(mandelbrotShader(center, viewWidth)).map { img =>
        SwingUtilities.invokeLater(() => showImage(label, img))
      }

    def pixelToComplex(px: Int, py: Int): Complex = {
      val xMin = center.re - viewWidth / 2
      val yMin = center.im - viewWidth / 2
      Complex(
        xMin + (px.toDouble / (size - 1)) * viewWidth,
        yMin + (py.toDouble / (size - 1)) * viewWidth
      )
    }

    label.addMouseListener(new MouseAdapter {
      override def mouseClicked(e: MouseEvent): Unit = {
        center = pixelToComplex(e.getX, e.getY)
        render()
        ()
      }
    })

    frame.addKeyListener(new KeyAdapter {
      override def keyPressed(e: KeyEvent): Unit =
        if e.getKeyChar == '+' then
          viewWidth *= 0.8
          render()
    })

    frame.setVisible(true)
    frame.requestFocusInWindow()

    render()
    ()
  }
}

def mandelbrotShader(
    center: Complex,
    width: Double,
    maxIter: Int = 100
): Point => Color = { p =>
  val xMin = center.re - width / 2
  val yMin = center.im - width / 2

  val c = Complex(
    xMin + (p.x / (size - 1)) * width,
    yMin + (p.y / (size - 1)) * width
  )

  def palette(t: Double): Color = {
    val points = Seq(
      0.0 -> Color(0.0 / 255, 7.0 / 255, 100.0 / 255),
      0.16 -> Color(32.0 / 255, 107.0 / 255, 203.0 / 255),
      0.42 -> Color(237.0 / 255, 255.0 / 255, 255.0 / 255),
      0.6425 -> Color(255.0 / 255, 170.0 / 255, 0.0 / 255),
      0.8575 -> Color(0.0 / 255, 2.0 / 255, 0.0 / 255)
    )

    val clampedT = clamp01(t)
    val (p1, p2) = points
      .sliding(2)
      .find { case Seq((pA, _), (pB, _)) => clampedT >= pA && clampedT <= pB }
      .map { case Seq(a, b) => (a, b) }
      .getOrElse((points.head, points.last))

    val (pos1, c1) = p1
    val (pos2, c2) = p2
    val w = if pos2 - pos1 == 0 then 0.0 else (clampedT - pos1) / (pos2 - pos1)

    def lerp(a: Double, b: Double, t: Double) = a + (b - a) * t

    Color(
      lerp(c1.r, c2.r, w),
      lerp(c1.g, c2.g, w),
      lerp(c1.b, c2.b, w)
    )
  }

  @annotation.tailrec
  def iterate(z: Complex, iter: Int): Int =
    if iter >= maxIter then iter
    else if z.abs2 > 4.0 then iter
    else iterate(z * z + c, iter + 1)

  val iters = iterate(Complex(0, 0), 0)
  if iters >= maxIter then Color(0, 0, 0)
  else {
    val t = iters.toDouble / maxIter
    palette(t)
  }
}
