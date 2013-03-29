package controllers

import play.api._
import play.api.mvc._
import scalax.file.{PathSet, Path}
import javax.imageio._
import java.io.File
import java.awt.image.BufferedImage
import java.awt.Image
import scala.collection.immutable.TreeMap
import scala.collection.parallel.immutable.ParSeq
/**
 * Created with IntelliJ IDEA.
 * User: qwilas
 * Date: 13/03/21
 * Time: 0:06
 * To change this template use File | Settings | File Templates.
 */
object ImageSimilarityController extends Controller{

 def RGBtoHSV(red: Int, green: Int, blue: Int): Tuple3[Int, Short, Short] = {
    var h,s,v:Double = 0

    val max: Double = List(red, green, blue).max
    val min: Double = List(red, green, blue).min

    // h
    if(max == min){
      h = 0;
    }
    else if(max == red){
      h = (60 * (green - blue) / (max - min) + 360) % 360
    }
    else if(max == green){
      h = (60 * (blue - red) / (max - min)) + 120
    }
    else if(max == blue){
      h = (60 * (red - green) / (max - min)) + 240
    }

    // s
    if(max == 0){
      s = 0
    } else {
      s = (255.0 * ((max - min) / max))
    }

    // v
    v = max

    (h.toInt, s.toShort, v.toShort)
  }
  def rgb2hsvt(t: Tuple3[Short, Short, Short]) = RGBtoHSV(t._1, t._2, t._3)

  def minmaxloc[A <% ParSeq[Int]](xs: A, length: Double) = {
    val max = xs.max
    val min = xs.min
    xs.map {(d: Int) => d.toDouble*length/(max-min) - length/(max-min)*min}
  }

  def medianCut[A <% ParSeq[Double]](xs: A) = {
    val median = xs.seq.sorted.apply((xs.size-1)/2)
    xs.map {(d: Double) => d.<=(median)}
  }
  def calculateSimilarity (from:String,to:String)= Action {

        val rootPath = Path.fromString(Play.current.configuration.getString("images.root").get)
        val fromPath: Path = rootPath / Path.fromString(from)
        val toPath: Path = rootPath / Path.fromString(to)

        fromPath.exists && toPath.exists match {
          case true => {

            val difference: Double = calculateSimilarityInt(fromPath, toPath)

            Ok(difference.toString)
        }
          case false => BadRequest("Specified images do not exist.")
        }

      }

  def calculateSimilarityInt (from: Path, to: Path): Double = {
    val size = 160
    val fromImage = ImageIO.read(new File(from.path))
    val fromImageSmall = new BufferedImage(size, size,BufferedImage.TYPE_INT_RGB)
    fromImageSmall.getGraphics.drawImage(fromImage.getScaledInstance(size, size, Image.SCALE_SMOOTH), 0,0,null)

    println("Images has been read: %s %s" format(from.path, to.path))

    calculateSimilarityImage(fromImageSmall, to)
  }
  def calculateSimilarityImage (from: BufferedImage, to: Path): Double = {
    val size = 160
    val smallsize = 16
    val toImage = ImageIO.read(new File(to.path))
    val toImageSmall = new BufferedImage(size, size,BufferedImage.TYPE_INT_RGB)
    toImageSmall.getGraphics.drawImage(toImage.getScaledInstance(size, size, Image.SCALE_SMOOTH), 0,0,null)

    val fromRGB = grayEqualize(minmaxloc({for (y <- 0 until size; x <- 0 until size) yield ImageUtility.Y(from.getRGB(x, y))}.toSeq.par, 255).map {_.toInt})
    val toRGB = grayEqualize(minmaxloc({for (y <- 0 until size; x <- 0 until size) yield ImageUtility.Y(toImageSmall.getRGB(x, y))}.toSeq.par, 255).map {_.toInt})
    val fromRGBinInt: Array[Int] = fromRGB.map {(d: Int) => ImageUtility.rgb(d, d, d)} toArray
    val toRGBinInt: Array[Int] = toRGB.map {(d: Int) => ImageUtility.rgb(d, d, d)} toArray

    from.setRGB(0,0,size,size,fromRGBinInt,0, size)
    toImageSmall.setRGB(0,0,size,size,toRGBinInt,0, size)

    /*val fromAVG = fromRGB.par.map{(fromRGB: Int) => (ImageUtility.r(fromRGB) + ImageUtility.g(fromRGB) + ImageUtility.b(fromRGB))/3}
    val toAVG = toRGB.par.map{(toRGB: Int) => (ImageUtility.r(toRGB) + ImageUtility.g(toRGB) + ImageUtility.b(toRGB))/3}

    math.sqrt({fromAVG zip (toAVG)}.par.map(t => math.pow(t._1.toDouble - t._2.toDouble, 2)).sum/10000)*/
    /*
    val Dif_R: Double = fromRGB.zip(toRGB).par.map((t: Tuple2[Int, Int]) => math.pow(ImageUtility.r(t._1) - ImageUtility.r(t._2), 3)) sum;
    val Dif_G: Double = fromRGB.zip(toRGB).par.map((t: Tuple2[Int, Int]) => math.pow(ImageUtility.g(t._1) - ImageUtility.g(t._2), 3)) sum;
    val Dif_B: Double = fromRGB.zip(toRGB).par.map((t: Tuple2[Int, Int]) => math.pow(ImageUtility.b(t._1) - ImageUtility.b(t._2), 3)) sum;
    math.sqrt((Dif_R + Dif_G + Dif_B)/3)
    */
    val fromImageVerySmall = new BufferedImage(smallsize, smallsize,BufferedImage.TYPE_INT_RGB)
    fromImageVerySmall.getGraphics.drawImage(from.getScaledInstance(smallsize,smallsize, Image.SCALE_SMOOTH), 0, 0, null)
    val toImageVerySmall = new BufferedImage(smallsize, smallsize,BufferedImage.TYPE_INT_RGB)
    toImageVerySmall.getGraphics.drawImage(toImageSmall.getScaledInstance(smallsize,smallsize, Image.SCALE_SMOOTH), 0, 0, null)
    val fromRGBsmall = medianCut({for (x <- 0 until smallsize; y <- 0 until smallsize) yield ImageUtility.r(fromImageVerySmall.getRGB(x, y)) toDouble} par)
    val toRGBsmall = medianCut({for (x <- 0 until smallsize; y <- 0 until smallsize) yield ImageUtility.r(toImageVerySmall.getRGB(x, y)) toDouble} par)
    val xored = fromRGBsmall.zip(toRGBsmall).map{(t: Tuple2[Boolean, Boolean]) => t._1 ^ t._2}

    xored.count((b: Boolean) => b).toDouble / (smallsize.toDouble*smallsize.toDouble)
  }
  def calculateAllSimurality(from: String) = Action {
    val asize = 160

    println("allsim")
    // あるパス以下のファイルを全て列挙する。フォルダは除外される。
    val fileCrawler: Path => PathSet[Path] = (p: Path) => p.***.filter((p: Path) => p.isFile)
    // コンフィグにある、画像フォルダのルート。
    val rootPath = Path.fromString(Play.current.configuration.getString("images.root").get)
    val fromPath = rootPath / Path.fromString(java.net.URLDecoder.decode(from, "UTF-8"))
    fromPath.exists match {
      case true => {
        val imageList = fileCrawler(rootPath).filter((p: Path) => p.name.endsWith(".jpg") || p.name.endsWith(".png") || p.name.endsWith(".gif") || p.name.endsWith(".bmp"))
        println("imagelist size: %d" format(imageList.size))
        val fromImage = ImageIO.read(new File(fromPath.path))
        val fromImageSmall = new BufferedImage(asize, asize,BufferedImage.TYPE_INT_RGB)
        fromImageSmall.getGraphics.drawImage(fromImage.getScaledInstance(asize, asize,Image.SCALE_SMOOTH), 0,0,null)

        var simList: scala.collection.mutable.ArrayBuffer[(Path, Double)] = new scala.collection.mutable.ArrayBuffer[(Path, Double)] with scala.collection.mutable.SynchronizedBuffer[(Path, Double)]

        var count: Long = 1
        val size: Int = imageList.size
        imageList.par.foreach{
              (p: Path) =>
                println("[%d/%d]" format(count, size))
                try {simList += ((p, calculateSimilarityImage(fromImageSmall, p)))} catch {case e: Exception => println("err: %s\n%s" format(p.path, e.getMessage))}
                count += 1
        }

        println("simList size: %d" format(simList.size))
        val SortedSimList:Seq[(String, Double)] = simList.seq.sortBy(_._2).map((t: Tuple2[Path, Double]) => (t._1.relativize(rootPath).path, t._2)).toSeq/*.take(30)*/
        Ok(views.html.similarity(SortedSimList))
      }
      case false => BadRequest("ファイルが存在しません: %s" format(from))
    }

  }

  def makeHistogram(xs: ParSeq[Int]): TreeMap[Int, Int] = {
    print("MK.HIST ")
    val min = xs.min
    val max = xs.max
    TreeMap(( (min to max) zip { (min to max) map {(x: Int) => xs.count((p: Int) => p == x)} } sortBy((t: Tuple2[Int, Int]) => t._1)).toArray :_*)
  }
  def makeCDF(hist: TreeMap[Int, Int]): TreeMap[Int, Int] = { // 累積分布行列を作る
    print("CDF ")
    val v = hist.values.par.scan(0)((z: Int, a: Int) => z + a)
    TreeMap(hist.keySet.zip(v.tail) toArray :_*)
  }
  def histEQ(v: Int, CDF: TreeMap[Int, Int], size: Int): Int = {
    val min = CDF.values.min.toDouble
    math.round( { (CDF(v).toDouble-min)/(size-min) } * 255 ).toInt
  }

  def grayEqualize(c: ParSeq[Int]): ParSeq[Int] = {
    print("EQ ")
    val cdf = makeCDF(makeHistogram(c))
    val size = c.size
    val eqHistgram = TreeMap(c.toSet.map((i: Int) => (i, histEQ(i, cdf, size))).toArray :_*)
    c.map {eqHistgram(_)}
  }

}
