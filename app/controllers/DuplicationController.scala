package controllers


import play.api._
import play.api.mvc._

/**
 * Created with IntelliJ IDEA.
 * User: qwilas
 * Date: 13/03/13
 * Time: 21:50
 * To change this template use File | Settings | File Templates.
 */
object DuplicationController extends Controller {
  def index = Action {
    import scalax.file.Path
    import scalax.file.PathSet
    import java.security.MessageDigest
    val hexArray: Array[String] = Array(
      "00","01","02","03","04","05","06","07","08","09","0A","0B","0C","0D","0E","0F",
      "10","11","12","13","14","15","16","17","18","19","1A","1B","1C","1D","1E","1F",
      "20","21","22","23","24","25","26","27","28","29","2A","2B","2C","2D","2E","2F",
      "30","31","32","33","34","35","36","37","38","39","3A","3B","3C","3D","3E","3F",
      "40","41","42","43","44","45","46","47","48","49","4A","4B","4C","4D","4E","4F",
      "50","51","52","53","54","55","56","57","58","59","5A","5B","5C","5D","5E","5F",
      "60","61","62","63","64","65","66","67","68","69","6A","6B","6C","6D","6E","6F",
      "70","71","72","73","74","75","76","77","78","79","7A","7B","7C","7D","7E","7F",
      "80","81","82","83","84","85","86","87","88","89","8A","8B","8C","8D","8E","8F",
      "90","91","92","93","94","95","96","97","98","99","9A","9B","9C","9D","9E","9F",
      "A0","A1","A2","A3","A4","A5","A6","A7","A8","A9","AA","AB","AC","AD","AE","AF",
      "B0","B1","B2","B3","B4","B5","B6","B7","B8","B9","BA","BB","BC","BD","BE","BF",
      "C0","C1","C2","C3","C4","C5","C6","C7","C8","C9","CA","CB","CC","CD","CE","CF",
      "D0","D1","D2","D3","D4","D5","D6","D7","D8","D9","DA","DB","DC","DD","DE","DF",
      "E0","E1","E2","E3","E4","E5","E6","E7","E8","E9","EA","EB","EC","ED","EE","EF",
      "F0","F1","F2","F3","F4","F5","F6","F7","F8","F9","FA","FB","FC","FD","FE","FF")

    val md = MessageDigest.getInstance("MD5")

    def md5(bA: Array[Byte]) = {
      md.digest(bA)
    }

    // def duplication[A](lis: List[A]): List[A] = {lis diff (lis distinct)}
    //def duplication(xs:List[Array[Byte]]): List[Array[Byte]] = xs.map(_.deep.asInstanceOf[Array[Byte]]) diff xs.map(_.deep.asInstanceOf[Array[Byte]]).distinct
    //def duplication(xs:List[Array[Byte]]): Vector[Array[Byte]] = xs.filter((p: Array[Byte]) => xs.count(_.deep == p.deep) > 1).toVector.distinct
    def duplication(xs:Vector[Vector[Byte]]): Vector[Vector[Byte]] = xs.filter((p: Vector[Byte]) => xs.count(_ == p) > 1).distinct

    val fileCrawler: Path => PathSet[Path] = (p: Path) => p.***.filter((p: Path) => p.isFile)
    val rootPath = Path.fromString(Play.current.configuration.getString("images.root").get)
    var md5hashes: Vector[(Path, Vector[Byte])] = Vector()
    fileCrawler(rootPath).foreach {(p: Path) => md5hashes = md5hashes :+ (Tuple2(p, md5(p.byteArray).toVector))}

    val onlyHash: Vector[Vector[Byte]] = md5hashes.map(_._2)

    val duplicatedmd5s: Vector[Vector[Byte]] = duplication(onlyHash)
    //Ok(duplicatedmd5s.map(_.map((b: Byte) => hexArray(b.asInstanceOf[Int] & 0xff)).mkString(" ")).mkString("\n"))

    val dup = md5hashes.filter((t: Tuple2[Path, Vector[Byte]]) => t match {case (p: Path, bA: Vector[Byte]) => duplicatedmd5s.contains(bA)})

    if (!dup.isEmpty) {
        Ok(dup.map((p) => p._2.map((b: Byte) => hexArray(b.asInstanceOf[Int] & 0xff)).mkString(" ") ++ " " ++ p._1.toString()).mkString("\n"))
    } else {
      Ok("No Dups")
    }
    }
}
