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

    // Vector[Byte]をhex-stringに変換するためのテーブル。
    val hexArray: Array[String] = Array(
      "00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "0A", "0B", "0C", "0D", "0E", "0F",
      "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "1A", "1B", "1C", "1D", "1E", "1F",
      "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "2A", "2B", "2C", "2D", "2E", "2F",
      "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "3A", "3B", "3C", "3D", "3E", "3F",
      "40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "4A", "4B", "4C", "4D", "4E", "4F",
      "50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "5A", "5B", "5C", "5D", "5E", "5F",
      "60", "61", "62", "63", "64", "65", "66", "67", "68", "69", "6A", "6B", "6C", "6D", "6E", "6F",
      "70", "71", "72", "73", "74", "75", "76", "77", "78", "79", "7A", "7B", "7C", "7D", "7E", "7F",
      "80", "81", "82", "83", "84", "85", "86", "87", "88", "89", "8A", "8B", "8C", "8D", "8E", "8F",
      "90", "91", "92", "93", "94", "95", "96", "97", "98", "99", "9A", "9B", "9C", "9D", "9E", "9F",
      "A0", "A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "AA", "AB", "AC", "AD", "AE", "AF",
      "B0", "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "BA", "BB", "BC", "BD", "BE", "BF",
      "C0", "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "CA", "CB", "CC", "CD", "CE", "CF",
      "D0", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "DA", "DB", "DC", "DD", "DE", "DF",
      "E0", "E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "EA", "EB", "EC", "ED", "EE", "EF",
      "F0", "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "FA", "FB", "FC", "FD", "FE", "FF")
    def hexStr(vB: Vector[Byte]): String = vB.map {
      (b: Byte) => hexArray(b.asInstanceOf[Int] & 0xff)
    }.mkString(" ")

    val md = MessageDigest.getInstance("MD5")

    def md5(bA: Array[Byte]) = md.digest(bA)

    // 重複するベクターを抽出する関数。この関数が返すベクターに重複は無い。
    def duplication(xs: Vector[Vector[Byte]]): Vector[Vector[Byte]] = xs.filter((p: Vector[Byte]) => xs.count(_ == p) > 1).distinct

    // あるパス以下のファイルを全て列挙する。フォルダは除外される。
    val fileCrawler: Path => PathSet[Path] = (p: Path) => p.***.filter((p: Path) => p.isFile)

    // コンフィグにある、画像フォルダのルート。
    val rootPath = Path.fromString(Play.current.configuration.getString("images.root").get)

    // md5hashesに次々(Path, Vector[Byte)を格納してゆく。
    var md5hashes: Vector[(Path, Vector[Byte])] = Vector()
    fileCrawler(rootPath).foreach {
      (p: Path) => md5hashes = md5hashes :+ (Tuple2(p, md5(p.byteArray).toVector))
    }


    // md5が重複しているファイルのmd5のみを抽出する。
    val duplicatedmd5s: Vector[Vector[Byte]] = duplication(md5hashes.map(_._2))

    // (Path, Vector)の大小を比較する関数。
    val pathVectorBytelt: ((Path, Vector[Byte]), (Path, Vector[Byte])) => Boolean =
      (vBx: (Path, Vector[Byte]), vBy: (Path, Vector[Byte])) => vBx._2.toString() < vBy._2.toString()

    // 抽出されたmd5をキーとしてmd5hashesを抽出。
    val dup = md5hashes.filter {
      (t: (Path, Vector[Byte])) => duplicatedmd5s.contains(t._2)
    } sortWith {
      pathVectorBytelt
    }
    val dupToShow: Map[String, Vector[(String, String)]] = dup.map {
      (t: Tuple2[Path, Vector[Byte]]) => (t._1.relativize(rootPath).path, hexStr(t._2))
    }.groupBy(_._2)
    /*val dup = md5hashes.filter{(t: (Path, Vector[Byte])) =>
      t match {
        case (p: Path, bA: Vector[Byte]) =>
          duplicatedmd5s.contains(bA)}}
      .sortWith{pathVectorBytelt}
      */
    Ok(views.html.duplication(dupToShow))
    /*if (!dup.isEmpty) {
      Ok(views.html.duplication(dupToShow))
      //Ok(dup.map((p) => p._2.map((b: Byte) => hexArray(b.asInstanceOf[Int] & 0xff)).mkString(" ") ++ " " ++ p._1.toString()).mkString("\n"))
    } else {
      Ok(views.html.duplication(Map[String, Vector[Tuple2[String, String]]]()))
    }*/

  }
}
