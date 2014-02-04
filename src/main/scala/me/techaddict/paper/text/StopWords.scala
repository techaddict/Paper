package me.techaddict.paper.text

object StopWords {
  import me.techaddict.paper.util.helpers.FileHelper

  private var _stopWordCount, _wordCount = 0

  def stopWordCount = _stopWordCount
  def stopWordCount_= (value: Int):Unit = _stopWordCount = value

  def wordCount = _wordCount
  def wordCount_= (value: Int): Unit = _wordCount = value

  //def stopWords = _stopWords
  //def stopWords_= (value: Array[String]) = _stopWords = value

  val fileName: String = "stopWords.txt"
  val stopWords: Set[String] = FileHelper.loadResourceFile(fileName).split('\n').toSet

  val PUNCTUATION = """[^a-zA-Z0-9-|_\\s]"""

  def removePunctuation(content: String): String =
    content.replaceAll(PUNCTUATION, " ")
  def getcandidateWords(strippedInput: String): Array[String] =
    strippedInput.split(" ")
  def getStopWordCount(content: String): WordStats = {
    if (content == "") return WordStats.EMPTY
    val ws: WordStats = new WordStats
    val strippedInput = removePunctuation(content)
    val candidateWords = getcandidateWords(strippedInput)
    var overlappingStopWords = List[String]()
    candidateWords foreach(word => {
      if (stopWords.contains(word.toLowerCase))
        overlappingStopWords ++= List(word.toLowerCase)
    })
    ws.setWordCount(candidateWords.length)
    ws.setStopWordCount(overlappingStopWords.size)
    ws.setStopWords(overlappingStopWords)
    return ws
  }
}
