package controllers

import play.api._
import play.api.mvc._

/**
 * Created with IntelliJ IDEA.
 * User: qwilas
 * Date: 13/03/17
 * Time: 1:27
 * To change this template use File | Settings | File Templates.
 */
object ImageKillerController extends Controller {
  def kill(pathStr: String) = Action {
    import scalax.file.Path

    println("Killing:" + pathStr)
    val rootPath = Path.fromString(Play.current.configuration.getString("images.root").get)

    try {
      val path = rootPath / Path.fromString(pathStr)
      val result = path.deleteIfExists()
      result match {
        case true => Ok(pathStr)
        case false => InternalServerError("Failed to remove file \"" + pathStr + "\": the file does not exists.")
      }

    } catch {
      case _ => InternalServerError("Failed to remove file \"" + pathStr + "\".")
    }
  }
}
