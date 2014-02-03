package me.techaddict.paper.parse

import me.techaddict.paper.Article
import org.jsoup.nodes.{ Element, TextNode, Node, Document }
import org.jsoup.select.Evaluator.Tag
import org.jsoup.select.{ Selector, Collector, Elements }
import scala.util.matching.Regex
import me.techaddict.paper.util.url.Parse._
import scala.collection.JavaConversions._
import me.techaddict.paper.text.StopWords

object Extractor extends me.techaddict.paper.Configuration {

  val pipeSplitter = """\\|""".r
  val dashSplitter = """ - """.r
  val arrowSplitter = """»""".r
  val colonSplitter = """:""".r

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
  }

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

  def getMetaContent(doc: Document, metaName: String): String = {
    val meta: Elements = doc.select(metaName)
    var content = ""
    if (meta.size > 0)
      content = meta.first.attr("content")
    return (if (content == "") "" else content)//trim
  }

  def getMetaDescription(article: Article) =
    getMetaContent(article.doc, "meta[name=description]")

  def getMetaKeywords(article: Article) =
    getMetaContent(article.doc, "meta[name=keywords]")

  def getCanonicalLink(article: Article): String = {
    val meta = article.doc.select("link[rel=canonical]")
    if (meta.size() > 0) {
      val href = Option(meta.first().attr("href")).getOrElse("")
      if (href.nonEmpty) href else article.finalUrl
    }
    else
      article.finalUrl
  }

  def extractTags(doc: Document): Set[String] = {
    if(doc.children.size == 0) return Set.empty[String]
    val elements = Selector.select("a[rel=tag], a[href*=/tag/]", doc)
    if (elements.size == 0) return Set.empty[String]
    var tags = Set[String]()
    elements.filter(x => x.text != "") foreach { el =>
      tags += el.text
    }
    return tags
  }

  def calculateBestNode(doc: Document): Element = {
    var topNode: Element = null
    val nodesToCheck = List("p", "pre", "td").foldLeft(List[Element]())((x, y) => x ++ Collector.collect(new Tag(y), doc))
    var startingBoost = 1.0
    var cnt = 0
    var i = 0
    var parentNodes = Array[Element]()
    var nodesWithText = Array[Element]()
    val nodeNumber = nodesWithText.length
    nodesToCheck foreach { node =>
      val nodeText = node.text
      var count = new StopWords().getStopWordsCount(nodeText)
      if (count > 2 && !isHighLinkDensity(node))
        nodesWithText = nodesWithText :+ node
    }
    var negativeScoring = 0
    val bottomNegativeScoreNodes = nodeNumber * 0.25
    nodesWithText foreach { node =>
      var boostScore = 0.0
      if (isBoostable(node) && cnt >= 0) {
        boostScore = 50.0 / startingBoost.toDouble
        startingBoost += 1.0
      }
      if (nodeNumber > 15 && (nodeNumber - i) <= bottomNegativeScoreNodes) {
        val booster = bottomNegativeScoreNodes - nodeNumber + i
        boostScore = - booster * booster
        val negScore = - boostScore + negativeScoring
        if (negScore > 40)
          boostScore = 5.0
      }
      var textNode = node.text
      val count = new StopWords().getStopWordsCount(textNode)
      val upScore = count + boostScore.toInt

      updateScore(node.parent, upScore)
      updateScore(node.parent.parent, upScore / 2)
      updateNodeCount(node.parent, 1)
      updateNodeCount(node.parent.parent, 1)

      if (!parentNodes.contains(node.parent))
        parentNodes = parentNodes :+ node.parent
      if (!parentNodes.contains(node.parent.parent))
        parentNodes = parentNodes :+ node.parent.parent
      cnt += 1
      i += 1
    }
    var topNodeScore = 0
    parentNodes foreach { node =>
      var score = getScore(node)
      if (score > topNodeScore) {
        topNode = node
        topNodeScore = score
      }
      if (topNode == null)
        topNode = node
    }
    return topNode
  }

  def isHighLinkDensity(node: Element): Boolean = {
    val links = node.getElementsByTag("a")
    if (links == null || links.length == 0)
      return false
    val text = node.text
    val words = " ".split(text)
    val sb: StringBuilder = new StringBuilder
    links map { x => sb.append(x.text)}
    val linkText = sb.toString
    val linkWords = " ".split(linkText)
    val linkDivisor = linkWords.length / words.length.toDouble
    val score = linkDivisor * links.length.toDouble
    if (score > 1)
      return true
    else
      return false
  }

  def isBoostable(node: Element): Boolean = {
    var nodes = walkSiblings(node)
    var stepsAway = 0
    val maxStepsAwayFromNode = 3
    val minimumStopWordCount = 5
    nodes foreach { currentNode =>
      if (currentNode.tagName == "p") {
        if (stepsAway >= maxStepsAwayFromNode)
          return false
        val count = new StopWords().getStopWordsCount(currentNode.text)
        if (count > minimumStopWordCount)
          return true
        stepsAway += 1
      }
    }
    return false
  }

  def walkSiblings(node: Element): Array[Element] = {
    var currentSibling = node.previousElementSibling
    var ret = Array[Element]()
    while (currentSibling != null) {
      ret = ret :+ currentSibling
      currentSibling = currentSibling.previousElementSibling
    }
    return ret
  }

  def updateScore(node: Element, addToScore: Int) {
    var currentScore = 0
    val scoreString = node.attr("gravityScore")
    if (scoreString != "")
      currentScore = scoreString.toInt
    val newScore = currentScore + addToScore
    node.attr("gravityScore", newScore.toString)
  }

  def updateNodeCount(node: Element, addToScore: Int) {
    var currentScore = 0
    val scoreString = node.attr("gravityNodes")
    if (scoreString != "")
      currentScore = scoreString.toInt
    val newScore = currentScore + addToScore
    node.attr("gravityNodes", newScore.toString)
  }

  def getScore(node: Element): Int = {
    val score = node.attr("gravityScore")
    if (score == "") return 0
    else return score.toInt
  }

  def getSiblingScore(topNode: Element): Int = {
    var paraNum, paraScore = 0
    val nodesToCheck = topNode.getElementsByTag("p")
    nodesToCheck foreach { node =>
      val count = new StopWords().getStopWordsCount(node.text)
      if (count > 2 && !isHighLinkDensity(node)) {
        paraNum += 1
        paraScore += count
      }
    }
    return if (paraNum > 0) paraScore / paraNum else 100000
  }

  def addSiblings(topNode: Element): Element = {
    val baselineScoreSiblingPara = getSiblingScore(topNode)
    var results = walkSiblings(topNode).foldLeft("")((x, y) => x ++ siblingsContent(y, baselineScoreSiblingPara))
    topNode.child(0).before(results.mkString)
    return topNode
  }

  def siblingsContent(node: Element, baselineScoreSiblingPara: Int): String = {
    if (node.tagName == "p" && node.text.length > 0)
      return node.outerHtml
    else {
      val potentialParagraphs = node.getElementsByTag("p")
      if (potentialParagraphs.first == null)
        return ""
      else {
        var ps = ""
        potentialParagraphs foreach { paragraph =>
          var text = paragraph.text
          if (text.length > 0) {
            val paraScore = new StopWords().getStopWordsCount(text)
            val score = baselineScoreSiblingPara * 0.3
            if (score < paraScore && !isHighLinkDensity(paragraph)) {
              ps += "<p>" + paragraph.text + "</p>"
            }
          }
        }
        return ps
      }
    }
  }

  def postCleanup(targetNode: Element): Element = {
    val node = addSiblings(targetNode)
    node.children.filter(x => x.tagName != "p")
    .filter( e => isHighLinkDensity(e) || isTableAndNoParaExist(e) || !isNodeScoreThreshholdMet(node, e))
    .foreach { e =>
      e.remove()
    }
    return node
  }

  def isTableAndNoParaExist(node: Element): Boolean = {
    node.getElementsByTag("p") filter(p => p.text.length < 25) foreach { p =>
      p.remove()
    }
    val subPara = node.getElementsByTag("p")
    if (subPara.length == 0 && (node.tagName != "td"))
      return true
    return false
  }

  def isNodeScoreThreshholdMet(node: Element, e: Element): Boolean = {
    val topNodeScore = getScore(node)
    val currentNodeScore = getScore(e)
    val thresholdScore = topNodeScore * 0.08
    if (currentNodeScore < thresholdScore && e.tagName != "td")
      return false
    return true
  }
}
