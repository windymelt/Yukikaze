package controllers

import play.api._
import play.api.mvc._

object Application extends Controller {
  
  def index = Action {
    Ok(views.html.index("Yukikaze is ready."))
  }

  def showFiles = Action {
    import scalax.file.Path
    import scalax.file.PathSet

    val root:PathSet[Path] = Path.fromString(Play.current.configuration.getString("images.root").get).children().filter{file => !file.isHidden}
    Ok(root.mkString("\n"))
  }
  
}