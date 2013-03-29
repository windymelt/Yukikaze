package controllers

import play.api._
import play.api.mvc._
import java.awt.image.BufferedImage
import java.awt.Image

/**
 * Created with IntelliJ IDEA.
 * User: qwilas
 * Date: 13/03/16
 * Time: 18:23
 * To change this template use File | Settings | File Templates.
 */
object ImageViewerController extends Controller {
  def view(pathstr: String) = Action {
    import scala.util.control.Exception._
    import scalax.file.Path

    val path: Option[Path] = allCatch opt Path.fromString(pathstr)
    val rootPath = Path.fromString(Play.current.configuration.getString("images.root").get)

    if (path == None) BadRequest("Invalid path.")

    val filePath = rootPath / path.get
    val ext = filePath.toString().substring(filePath.toString().lastIndexOf("."))

    ext match {
      /*case ".png" => Ok(filePath.byteArray).as("image/png")
      case ".jpg" => Ok(filePath.byteArray).as("image/jpeg")
      case ".bmp" => Ok(filePath.byteArray).as("image/bmp")*/
      case _ => Ok(filePath.byteArray).as(BINARY)
    }
  }
  def viewhtml(pathstr: String) = Action {
    Ok("<img src=\"/files/%s\" alt=\"image\">" format(pathstr)).as(HTML)
  }
  def view2(pathStr: String) = Action {
    import javax.imageio._
    import scala.util.control.Exception._
    import scalax.file.Path

    val path = allCatch opt Path.fromString(pathStr)
    val rootPath = Path.fromString(Play.current.configuration.getString("images.root").get)
    val filePath = rootPath / path.get
    val image = ImageIO.read(new java.io.File(filePath.path))
    val image2: BufferedImage = new BufferedImage(160,160,BufferedImage.TYPE_INT_RGB)

    image2.getGraphics.drawImage(image.getScaledInstance(160,160,Image.SCALE_SMOOTH), 0,0,null)

     /* for (x <- 0 until image.getWidth; y <- 0 until image.getHeight) {
      val c:Int = image.getRGB(x, y)
      val r: Int = ImageUtility.r(c)
      val g: Int = ImageUtility.g(c)
      val b: Int = ImageUtility.b(c)
      val q: Int = pow((r * g * b), (1.0/3.0)).toInt
      //val q: Int = ((r+g+b)/3).toInt
      image.setRGB(x, y, rgb(r, g, b))
    }*/
    val out = new java.io.ByteArrayOutputStream()
    ImageIO.write(image2, "png", out)
    val arr: Array[Byte] = out.toByteArray
    Ok(arr)
  }


}

object ImageUtility{
  import scala.collection.mutable.Map
  val Ymemo: Map[Int, Double] = Map()
  def Y(c: Int): Int = {
    /*Ymemo.isDefinedAt(c) match {
      case true => Ymemo(c)
      case false =>
        val v = (0.299*r(c) + 0.587* g(c) + 0.114*b(c))
        v > 255 match {
          case true => Ymemo.put(c, 255); 255.0
          case false => Ymemo.put(c, v); v
        }*/
    (r(c)+g(c)+b(c))/3

  }
  def a(c: Int): Short ={
    return (c>>>24).toShort
  }
  def r(c: Int): Short ={
    return (c>>16&0xff).toShort
  }
  def g(c: Int): Short ={
    return (c>>8&0xff).toShort
  }
  def b(c: Int): Short ={
    return (c&0xff).toShort
  }
  def rgbtuple(c: Int): (Short, Short, Short) = {
    (r(c), g(c), b(c))
  }
  def rgbList(c: Int): List[Int] = {
    List(r(c), g(c), b(c))
  }
  def rgb
  (r: Int,g: Int,b: Int): Int ={
    return 0xff000000 | r <<16 | g <<8 | b
  }
  def argb
  (a: Int,r: Int,g: Int,b: Int): Int ={
    return a<<24 | r <<16 | g <<8 | b
  }

}
