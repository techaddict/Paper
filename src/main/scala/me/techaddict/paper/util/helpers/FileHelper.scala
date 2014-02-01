package me.techaddict.paper.util.helpers

object FileHelper {
  def loadResourceFile(filename: String): String = {
    import scala.io.Source
    try {
      return Source.fromFile("src/main/resources/me/techaddict/paper/" + filename, "UTF-8").mkString
    } catch {
      case e: Throwable =>
        println("error")
        return ""
    }
  }
}
