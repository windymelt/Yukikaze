package controllers

import play.api._
import play.api.mvc._

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
      case ".png" => Ok(filePath.byteArray).as("image/png")
      case ".jpg" => Ok(filePath.byteArray).as("image/jpeg")
      case ".bmp" => Ok(filePath.byteArray).as("image/bmp")
      case _ => Ok(filePath.byteArray)
    }
  }
}
