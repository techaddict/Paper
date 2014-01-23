package io.pilo

import util.url.Url._
import util.url.Parse._
import concurrent.{ Future, Promise }

class Article(url1: String, title: String = "", sourceUrl1: String = "") extends Configuration {
  var sourceUrl = sourceUrl1
  var url = url1

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

  var isParsed = false
  var isDownloaded = false

  var metaDescription = ""
  var metaFavicon = ""
  var canonicalLink = ""
  var topNode = None
  var doc = None
  var rawDoc = None

  def build {
    download
    //parse
    //nlp
  }

  def download {
    import scala.concurrent.ExecutionContext.Implicits.global
    html = AsyncWebClient.get(url)
    //AsyncWebClient.shutdown()
    isDownloaded = true
  }

  def isValidUrl = validUrl(url)

  def isValidBody = ???
  def isMediaNews = ???
  def parse = ???

  def nlp {
    if(!isDownloaded || !isParsed) {
      println(" You must Download and Parse the article before using this")
    }
    else {
      import io.pilo.text.Nlp
      val nlp = new Nlp()
      val textKeywords = nlp.getKeywords(text)._1.keys
      val titleKeywords = nlp.getKeywords(title)._1.keys
      val keywords = (titleKeywords ++ textKeywords).toList
      val summarySentences = nlp.summarize("", title, text)
      summary = summarySentences.drop(summarySentences.length - maxSummary).mkString("\r\n")
    }
  }
}