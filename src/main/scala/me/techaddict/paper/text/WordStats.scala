package me.techaddict.paper.text

object WordStats {
  var EMPTY: WordStats = new WordStats
}

class WordStats {
  import WordStats._
  var stopWordCount: Int = 0
  var wordCount: Int = 0
  var stopWords: List[String] = List[String]()

  def getStopWords: List[String] = {
    stopWords
  }

  def setStopWords(words: List[String]) {
    stopWords = words
  }

  def getStopWordCount: Int = {
    stopWordCount
  }

  def setStopWordCount(wordcount: Int) {
    stopWordCount = wordcount
  }

  def getWordCount: Int = {
    wordCount
  }

  def setWordCount(cnt: Int) {
    wordCount = cnt
  }
}