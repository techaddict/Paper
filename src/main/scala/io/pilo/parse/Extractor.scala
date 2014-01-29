package io.pilo.parse

import io.pilo.Article
import org.jsoup.nodes.{ Element, TextNode, Node, Document }
import org.jsoup.select.Evaluator.Tag
import org.jsoup.select.{ Collector, Elements }
import scala.util.matching.Regex

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
        //val found = doc.getAttribute(attr)
      }
    }

    val pipeSplitter = """\\|""".r
    val dashSplitter = """ - """.r
    val arrowSplitter = """»""".r
    val colonSplitter = """:""".r

    def getTitle(article: Article): String = {
      val doc = article.doc
      val titleElem = doc.getElementsByTag("title")
      if (titleElem == null || titleElem.isEmpty)
        return ""
      var titleText = titleElem.first.text
      if (titleText == null || titleText == "")
        return ""
      var usedDelimeter = false
      if (titleText.contains("|")) {
        titleText = doTitleSplits(titleText, pipeSplitter)
        usedDelimeter = true
      }
      if (!usedDelimeter && titleText.contains("|")) {
        titleText = doTitleSplits(titleText, dashSplitter)
        usedDelimeter = true
      }
      if (!usedDelimeter && titleText.contains("»")) {
        titleText = doTitleSplits(titleText, arrowSplitter)
        usedDelimeter = true
      }
      if (!usedDelimeter && titleText.contains(":")) {
        titleText = doTitleSplits(titleText, colonSplitter)
      }
      return """(&#65533;)""".r.replaceAllIn(titleText, "")
    }

    def doTitleSplits(title: String, splitter: Regex): String = {
      var largestTextLen = 0
      var largeTextIndex = 0
      val titlePieces = splitter.split(title)
      var i =0
      while (i < titlePieces.length) {
        val current = titlePieces(i)
        if (current.length > largestTextLen) {
          largestTextLen = current.length
          largeTextIndex = i
        }
        i += 1
      }
      return """»|(&raquo;)""".r.replaceAllIn(titlePieces(largeTextIndex), "")
    }
  }
}
