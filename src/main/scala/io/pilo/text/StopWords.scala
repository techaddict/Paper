package io.pilo.text

class StopWords {
  import io.pilo.text.{StopWords => ws}
  val PUNCTUATION = """\p{punct}""".r

  def removePunctuation(content: String): String = content.replaceAll(PUNCTUATION.toString, "")
  def getcandidateWords(strippedInput: String): Array[String] = strippedInput.split(' ')
  def getStopWordsCount(content: String) {
    val strippedInput = removePunctuation(content)
    val candidateWords = getcandidateWords(strippedInput)
    var overlappingStopWords: Array[String] = Array()
    var count = 0
    for (word <- candidateWords) {
      count += 1
      if (ws.stopWords.contains(word.toLowerCase))
        overlappingStopWords ++= Array(word.toLowerCase)
    }
    ws.wordCount = count
    ws.stopWordCount = overlappingStopWords.length
    //ws.stopWords = overlappingStopWords
  }
}

object StopWords {
  import io.pilo.util.helpers.FileHelper

  private var _stopWordCount, _wordCount = 0

  def stopWordCount = _stopWordCount
  def stopWordCount_= (value: Int):Unit = _stopWordCount = value

  def wordCount = _wordCount
  def wordCount_= (value: Int): Unit = _wordCount = value

  //def stopWords = _stopWords
  //def stopWords_= (value: Array[String]) = _stopWords = value

  val fileName: String = "stopWords.txt"
  lazy val stopWords: Set[String] = FileHelper.loadResourceFile(fileName).split('\n').toSet
}
