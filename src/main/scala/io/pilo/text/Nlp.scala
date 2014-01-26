package io.pilo.text

import scala.collection.immutable.ListMap

case class Sentence(sentence: String, score: Double, order: Int)

class Nlp {
  val ideal = 20.0
  val summaryLimit = 5
  val keywordsSize = 10

  def summarize(url: String, title: String, text: String): Array[Sentence] = {
    if (text == "" || title == "") return Array()
    val sentences = splitSentences(text)
    def titleWords = splitWords(title)
    val keywords = getKeywords(text)._1
    val ranks = score(sentences, titleWords, keywords)
    return ranks.sortBy(-_.score).take(ranks.size.min(summaryLimit)).sortBy(_.order).toArray.reverse
  }

  def score(sentences: Array[String], titleWords: Array[String], keyWords: Map[String, Int]): List[Sentence] = {
    val senSize = sentences.length
    var ranks: List[Sentence] = List()
    var i = 0
    for (i <- 0 to senSize - 1) {
      val sentence = splitWords(sentences(i))
      val titleFeature = titleScore(titleWords, sentence)
      val sentenceLen = lengthScore(sentence.length)
      val sentencePos = sentencePosition(i+1, senSize)
      val sbsFeature = sbs(sentence, keyWords)
      val dbsFeature = dbs(sentence, keyWords)
      val frequency = (sbsFeature + dbsFeature) / 2.0 * 10.0

      val totalScore = (titleFeature * 1.5 + frequency * 2.0 + sentenceLen * 0.5 + sentencePos * 1.0) / 4.0
      ranks = ranks :+ Sentence(sentences(i), totalScore, i)
    }
    return ranks
  }

  def sbs(words: Array[String], keyWords: Map[String, Int]): Double = {
    if (words.size == 0)
      return 0
    val score = words.map { word =>
      keyWords.contains(word) match {
        case true => keyWords(word)
        case false => 0
      }
    }.sum
    return (1.0 / scala.math.abs(words.length) * score) // / 10.0
  }

  def dbs(words: Array[String], keyWords: Map[String, Int]): Double = {
    if (words.length == 0)
      return 0
    var summ: Double = 0
    var first: (Int, Double) = (0, 0)
    var second: (Int, Double) = (0, 0)
    val i = 0
    for (i <- 0 to words.length - 1) {
      if (keyWords.contains(words(i))) {
        val score = keyWords(words(i))
        if (first == (0, 0))
          first = (i, score)
        else {
          second = first
          first = (i, score)
          val diff = first._1 - second._1
          summ += ((first._2*second._2) / math.pow(diff, 2.0))
        }
      }
    }
    val k = (keyWords.keySet & words.toSet).size + 1 // & - set intersection
    return (1 / (k * (k + 1.0)) * summ)
  }

  def splitSentences(text: String): Array[String] = {
    import opennlp.tools.sentdetect.{ SentenceDetectorME, SentenceModel }
    val model = new SentenceModel(getClass.getResource("en-sent.bin"))
    val sen = new SentenceDetectorME(model)
    return sen.sentDetect(text)
  }

  def lengthScore(sentenceLen: Int): Double = 1.0 - scala.math.abs(ideal - sentenceLen) / ideal

  def titleScore(title: Array[String], sentence: Array[String]): Double = {
    import io.pilo.text.{ StopWords => ws }
    val count = sentence.count(x => !ws.stopWords.contains(x) && title.contains(x))
    return count / math.max(title.size, 1).toDouble
  }

  def getKeywords(text: String): (Map[String, Int], Int) = {
    import io.pilo.text.{StopWords => ws}
    var keyWords = splitWords(text)
    val numWords = keyWords.length
    var freq: List[(String, Int)] =
      keyWords.filterNot(x => ws.stopWords.contains(x)).groupBy(x => x).map(x => (x._1, x._2.size)).toList.sortBy{_._2}
    return (freq.takeRight(keywordsSize.min(freq.size)).toMap, numWords)
  }

  def splitWords(text: String): Array[String] =
    """[^\w]""".r.split(text).map(_.trim).filter(_.nonEmpty)

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
    return 0d
  }
}
