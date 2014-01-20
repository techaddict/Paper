package io.pilo.util.helpers

object FileHelper {
  def loadResourceFile(filename: String): Array[String] = {
    import scala.io.Source
    try {
      return Source.fromFile("src/main/resources/io/pilo/" + filename, "UTF-8").mkString.split('\n')
    } catch {
      case e: Throwable =>
        println("error")
        return Array("")
    }
  }
}