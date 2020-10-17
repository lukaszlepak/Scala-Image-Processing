import java.io.File

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.nio.{JpegWriter, PngWriter}
import com.typesafe.config.ConfigFactory


case class Config(inputDirectory: String, outputDirectory: String)


object DarkImageFilter extends App {

  val configFile = ConfigFactory.load("application.conf")

  val config = Config(
    configFile.getString("dark-image-filter.input-directory"),
    configFile.getString("dark-image-filter.output-directory")
  )

  new File(config.inputDirectory).listFiles().foreach { file =>
    val (name, extension) = file.getName.split('.') match {
      case Array(n, e) => (n, e)
    }

    val image = ImmutableImage.loader().fromFile(file)

    extension match {
      case "jpg" => image.output(JpegWriter.Default, config.outputDirectory + s"/${name}_bright_0.jpg")
      case "jpeg" => image.output(JpegWriter.Default, config.outputDirectory + s"/${name}_bright_0.jpeg")
      case "png" => image.output(PngWriter.NoCompression, config.outputDirectory + s"/${name}_bright_0.png")
    }
  }
}

