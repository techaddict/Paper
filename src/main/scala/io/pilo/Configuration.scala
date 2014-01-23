package io.pilo

trait Configuration {
  var minWordCount = 300
  var minSentCount = 7
  var maxTitle = 200
  var maxText = 100000
  var maxKeywords = 35
  var maxAuthors = 10
  var maxSummary = 5000

  var maxFileMemo = 20000
  //val parserClass = 'jsoup'
  val memoizeArticles = true
  val fetch_images = false
  val keepArticleHtml = false
  val stopwordsClass = text.StopWords.stopWords
  val browserUserAgent = "pilo.io/0.1"
  val requestTimeout = 7000
  val numberThreads = 10
  val verbose = false
}