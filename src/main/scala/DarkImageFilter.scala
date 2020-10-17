import java.io.File

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.nio.{JpegWriter, PngWriter}
import com.typesafe.config.ConfigFactory

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global


case class Config(inputDirectory: String, outputDirectory: String, threshold: Int)


object DarkImageFilter extends App {

  def imageDarkness(image: ImmutableImage): Int = {
    val colors = image.colors
    val brightness = colors.foldLeft(0.0)((sum, color) => sum + color.blue*0.0722 + color.green*0.7152 + color.red*0.2126) / colors.size
    100 - (brightness * 100 / 255).toInt
  }

  def processImage(nameWithExtension: String, image: ImmutableImage): Future[Unit] = Future {
    val (name, extension) = nameWithExtension.split('.') match {
      case Array(n, e) => (n, e)
    }

    val imageDarknessScore = imageDarkness(image)

    val imageNameWithScore = {
      if (imageDarknessScore < config.threshold)
        s"/${name}_bright_$imageDarknessScore"
      else
        s"/${name}_dark_$imageDarknessScore"
    }

    extension match {
      case "jpg" => image.output(JpegWriter.Default, config.outputDirectory + s"/$imageNameWithScore.jpg")
      case "jpeg" => image.output(JpegWriter.Default, config.outputDirectory + s"/$imageNameWithScore.jpeg")
      case "png" => image.output(PngWriter.NoCompression, config.outputDirectory + s"/$imageNameWithScore.png")
    }
  }

  val configFile = ConfigFactory.load("application.conf")

  val config = Config(
    configFile.getString("dark-image-filter.input-directory"),
    configFile.getString("dark-image-filter.output-directory"),
    configFile.getInt("dark-image-filter.threshold")
  )

  new File(config.inputDirectory).listFiles().foreach { file =>
    processImage(file.getName, ImmutableImage.loader().fromFile(file))
  }
}

