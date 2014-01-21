package io.pilo.text

class Nlp {
  val ideal = 20.0
  def summarize(url: String, title: String, text: String): Array[String] = {
    if (text == "" || title == "") return Array()
    var summaries: Array[String] = Array()
    val sentences = splitSentences(text)
    val keys = keyWords(text)
    val titleWords = splitWords(title)
    val ranks = score(sentences, titleWords, keys)
    ranks.keys.copyToArray(summaries)
    return summaries
  }

  def score(sentences: Array[String], titleWords: Array[String], keyWords: Map[String, Int]): Map[String, Double] = {
    val senSize = sentences.length
    var ranks: Map[String, Double] = Map()
    var i = 0
    for (i <- 0 to senSize) {
      val sentence = splitWords(sentences(i))
      val titleFeature = titleScore(titleWords, sentence)
      val sentenceLen = lengthScore(sentence.length)
      val sentencePos = sentencePosition(i+1, senSize)
      val sbsFeature = sbs(sentence, keyWords)
      val dbsFeature = dbs(sentence, keyWords)
      val frequency = (sbsFeature + dbsFeature) / 2.0 * 10.0

      val totalScore = (titleFeature * 1.5 + frequency * 2.0 + sentenceLen * 1.0 + sentencePos * 1.0) / 4.0
      ranks += sentence(i) -> totalScore
    }
    return ranks
  }

  def sbs(words: Array[String], keyWords: Map[String, Int]): Double = {
    var score = 0.0
    if (words.length == 0)
      return 0
    for (word <- words)
      if (keyWords.contains(word))
        score += keyWords(word)
    return (1.0 / scala.math.abs(words.length) * score) / 10.0
  }

  def dbs(words: Array[String], keyWords: Map[String, Int]): Double = {
    if (words.length == 0) return 0
    var summ: Double = 0
    var first: Array[Int] = Array()
    var second: Array[Int] = Array()
    val i = 0
    for (i <- 0 to words.length) {
      if (keyWords.contains(words(i))) {
        val score = keyWords(words(i))
        if (first == Array())
          first = Array(i, score)
        else {
          second = first
          first = Array(i, score)
          val diff = first(0) - second(0)
          summ += ((first(1)*second(1)) / scala.math.pow(diff, 2.0))
        }
      }
    }
    val k = (keyWords.keySet & words.toSet).size + 1 // & - set intersection
    return (1 / (k * (k + 1.0)) * summ)
  }

  def splitSentences(text: String): Array[String] = {
    import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
    var doc = new CoreNLPProcessor(internStrings = true).mkDocument(text)
    var ret: Array[String] = Array()
    for (sentence <- doc.sentences; if sentence.getSentenceText().length > 10) {
      ret = ret :+ sentence.getSentenceText().replace("\n", "")
    }
    return ret
  }

  def lengthScore(sentenceLen: Int): Double = {
    return 1 - scala.math.abs(ideal - sentenceLen) / ideal
  }

  def titleScore(title1: Array[String], sentence: Array[String]): Double = {
    import io.pilo.text.{StopWords => ws}
    val title = title1.filterNot(x => ws.stopWords.contains(x))
    var count = 0.0
    for (word <- sentence)
      if (!ws.stopWords.contains(word) && title.contains(word))
        count += 1.0
    return count / scala.math.max(title.length, 1)
  }

  def keyWords(text1: String): Map[String, Int] = {
    import io.pilo.text.{StopWords => ws}
    var text = splitWords(text1)
    val numWords = text.length
    text = text.filterNot(x => ws.stopWords.contains(x))
    var freq: Map[String, Int] = Map()
    for (word <- text) {
      if (freq.contains(word)) {
        val count = freq(word) + 1
        freq -= word
        freq += word -> count
      }
      else freq += word -> 1
    }
    val minSize = scala.math.min(10, freq.size)
    val freqlist: List[(String, Int)] = freq.toList.sortBy{_._2}.takeRight(minSize)
    return freqlist.toMap
  }

  def splitWords(text1: String): Array[String] = {
    val REGEX = """[^\w ]""".r // Stripping Special Chars
    val text = REGEX.replaceFirstIn(text1, "")
    var ret: Array[String] = Array()
    for( x <- text.split("\\s+")) ret ++= Array(x.stripPrefix(".").stripSuffix("."))
    return ret
  }

  def sentencePosition(i: Int, size: Int): Double = {
    val normalized = i*1.0 / size
    if(normalized > 1.0) return 0
    else if (normalized > 0.9) return 0.15
    else if (normalized > 0.8) return 0.04
    else if (normalized > 0.7) return 0.04
    else if (normalized > 0.6) return 0.06
    else if (normalized > 0.5) return 0.04
    else if (normalized > 0.4) return 0.05
    else if (normalized > 0.3) return 0.08
    else if (normalized > 0.2) return 0.14
    else if (normalized > 0.1) return 0.23
    else if (normalized > 0) return 0.17
    else return 0
  }
}