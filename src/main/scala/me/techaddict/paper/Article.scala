package me.techaddict.paper

import util.url.Url._
import util.url.Parse._
import concurrent.{ Future, Promise }
import org.jsoup.nodes.{ Element, TextNode, Node, Document }
import org.jsoup.Jsoup

class Article(url1: String, title: String = "", sourceUrl1: String = "") extends Configuration {
  import scala.concurrent.ExecutionContext.Implicits.global

  var sourceUrl = sourceUrl1
  var url = url1
  var finalUrl = ""

  if(sourceUrl == "")
    sourceUrl = getScheme(url) + "://" + getDomain(url)

  url = prepareUrl(url, sourceUrl)

  var topImg = ""
  var imgs: Array[String] = Array()
  var movies: Array[String] = Array()

  var text = ""

  var keywords: Array[String] = Array()
  var metaKeywords: Array[String] = Array()
  var tags: Set[String] = Set()

  var authors: Array[String] = Array()
  var publishedDate = ""
  var summary = ""
  var html: Future[String] = _
  var articleHtml = ""
  var cleanedArticleText = ""

  var isParsed = false
  var isDownloaded = false

  var metaDescription = ""
  var metaFavicon = ""
  var canonicalLink = ""
  var topNode: Element = null
  var doc: Document = null
  var rawDoc: Document = null

  
  def build {
    download
    parse
    nlp
  }

  def download {
    import scala.concurrent.ExecutionContext.Implicits.global
    html = network.AsyncWebClient.get(url)
    //AsyncWebClient.shutdown()
    html onSuccess {
      case content =>
        doc = Jsoup.parse(content)
        isDownloaded = true
        network.AsyncWebClient.shutdown()
    }
    html onFailure {
      case e =>
        println("Error html not found" + e)
        network.AsyncWebClient.shutdown()
    }
  }

  def isValidUrl = validUrl(url)

  def isValidBody = ???
  def isMediaNews = ???
  def parse = {
    if(!isDownloaded){
      println("you must download an article before parsing it")
    }
    rawDoc = doc
    if (doc == None){
      println("Article Parse Error" + url)
    }
    //val documentCleaner = DocumentCleaner
    //OutputFormatter(doc)
    import me.techaddict.paper.parse.Extractor._
    topNode = calculateBestNode(doc)
  }

  def nlp {
    if(!isDownloaded || !isParsed) {
      println(" You must Download and Parse the article before using this")
    }
    else {
      import me.techaddict.paper.text.Nlp
      val nlp = new Nlp()
      val textKeywords = nlp.getKeywords(text)._1.keys
      val titleKeywords = nlp.getKeywords(title)._1.keys
      keywords = (titleKeywords ++ textKeywords).toArray
      val summarySentences = nlp.summarize("", title, text)
      summary = summarySentences.drop(summarySentences.length - maxSummary).mkString("\r\n")
    }
  }
}
