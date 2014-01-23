package io.pilo

import util.url.Url._
import util.url.Parse._

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
  var html = ""

  var metaDescription = ""
  var metaFavicon = ""
  var canonicalLink = ""
  var topNode = None
  var doc = None
  var rawDoc = None
}