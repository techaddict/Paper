package io.pilo.util.helpers

object FileHelper {
  def loadResourceFile(filename: String): String = {
    import scala.io.Source
    try {
      return Source.fromFile("src/main/resources/io/pilo/" + filename, "UTF-8").mkString
    } catch {
      case e: Throwable =>
        println("error")
        return ""
    }
  }
}