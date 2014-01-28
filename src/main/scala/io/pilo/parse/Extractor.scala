package io.pilo.parse

import io.pilo.Article
import org.jsoup.nodes.{ Element, TextNode, Node, Document }
import org.jsoup.select.Evaluator.Tag
import org.jsoup.select.{ Collector, Elements }

class Extractor extends io.pilo.Configuration {
  def getAuthors(article: Article) {

    def contains_digits(d: String): Boolean =
      return """\d""".r.findFirstIn(d) != None

    def parseByLine(searchStr: String): Array[String] = {
      // Remove HTML
      """<[^<]+?>""".r.replaceAllIn(searchStr, "")
      // Remove Original
      """[bB][yY][\:\s]|[fF]rom[\:\s]""".r.replaceAllIn("", searchStr)

      searchStr.replaceAll("""(?m)\s+$""", "")
      val nameTokens = """[^\w\'\-]""".r.split(searchStr).map(_.replaceAll("""(?m)\s+$""", ""))
      var authors = Array[String]()
      var curname = Array[String]()
      val delim = Array("and", "")

      nameTokens foreach { token =>
        if (delim.contains(token) && curname.length == 2) {
          authors = authors :+ curname.mkString(" ")
          curname = Array[String]()
        }
        else if (!contains_digits(token))
          curname = curname :+ token
      }
      if(curname.length >= 2)
        authors = authors :+ curname.mkString(" ")
      return authors
    }
    val attrs = Array("name", "rel", "itemprop", "class", "id")
    val vals = Array("author", "byline")
    val matches = Array[String]()
    val authors = Array[String]()
    val _authors = Array[String]()
    val doc = article.doc
    val html = article.html
    attrs foreach { attr =>
      vals foreach { value =>
        val found = doc.getAttribute(attr)
      }
    }

    def getTitle(article: Article): String = {
      var title = ""
      val doc = article.doc
      val title = doc.getElementsByTag("title")
      if (title == null || title.isEmpty)
        return ""
      var titleText = title.first.text
      if (titleText == null || titleText == "")
        return ""
      var usedDelimeter = false
      if (titleText.contains("|")) {
        //titleText
      }
      return ""
    }
  }
}
