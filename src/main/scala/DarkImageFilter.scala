import java.io.File

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.color.RGBColor
import com.sksamuel.scrimage.nio.{JpegWriter, PngWriter}
import com.typesafe.config.ConfigFactory

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Random, Try}


case class Config(inputDirectory: String, outputDirectory: String, threshold: Int)


object DarkImageFilter extends App {

  def pixelDarkness(color: RGBColor): Double = {
    255 - (color.blue*0.0722 + color.green*0.7152 + color.red*0.2126)
  }

  def imageDarkness(image: ImmutableImage): Int = {
    val colors = image.colors
    val sizeOfSampling = (colors.length*0.01).toInt

    val darknessOfSampling = (0 to sizeOfSampling).foldLeft(0.0)( (result, _) => result + pixelDarkness(colors(Random.nextInt(colors.length))) ) /sizeOfSampling
    (darknessOfSampling * 100 / 255).toInt
  }

  def processImage(nameWithExtension: String, image: ImmutableImage, threshold: Int, outputDirectory: String): Future[Unit] = Future {
    val (name, extension) = nameWithExtension.split('.') match {
      case Array(n, e) => (n, e)
    }

    val imageDarknessScore = imageDarkness(image)

    val imageNameWithScore = {
      if (imageDarknessScore < threshold)
        s"/${name}_bright_$imageDarknessScore"
      else
        s"/${name}_dark_$imageDarknessScore"
    }

    extension match {
      case "jpg" => image.output(JpegWriter.Default, outputDirectory + s"/$imageNameWithScore.jpg")
      case "jpeg" => image.output(JpegWriter.Default, outputDirectory + s"/$imageNameWithScore.jpeg")
      case "png" => image.output(PngWriter.NoCompression, outputDirectory + s"/$imageNameWithScore.png")
    }
  }

  val config = {
    for {
      configFile        <-    Try(ConfigFactory.load("application.conf"))
      inputDirectory    <-    Try(configFile.getString("dark-image-filter.input-directory")) if new File(inputDirectory).exists
      outputDirectory   <-    Try(configFile.getString("dark-image-filter.output-directory")) if new File(outputDirectory).exists
      threshold         <-    Try(configFile.getInt("dark-image-filter.threshold"))
    } yield Config(inputDirectory, outputDirectory, threshold)
  }.recover {
    case t: Throwable => sys.error(t.getMessage)
  }.get

  new File(config.inputDirectory).listFiles().foreach { file =>
    processImage(file.getName, ImmutableImage.loader().fromFile(file), config.threshold, config.outputDirectory)
  }
}

