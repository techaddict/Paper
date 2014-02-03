package me.techaddict.paper.parse

import scala.xml.Utility
import org.jsoup.nodes.{ Element, TextNode, Node, Document }
import org.jsoup.select.Elements
import org.jsoup.Jsoup
import me.techaddict.paper.Article
import me.techaddict.paper.text.StopWords
import scala.collection.JavaConversions._

object OutputFormatter extends me.techaddict.paper.Configuration{
  var topNode = None

  var tagReplace = "<[^>]+>".r

  def convertToText(node: Element): String = node match {
    case null => ""
    case node => {
      (node.children.map(e => {
        Utility.unescape(e.text, new StringBuilder("")).toString.trim
        })).mkString("\n\n")
    }
  }

  def getFormattedText(node: Element): String = {
    removeNodesWithNegativeScores(node)
    convertLinksToText(node)
    replaceTagsWithText(node)
    removeParagraphsWithFewWords(node)
    convertToText(node)
  }

  def removeNodesWithNegativeScores(node: Element) {
    val gravityItems = node.select("*[gravityScore]")
    gravityItems foreach { item =>
      val score = item.attr("gravityScore").toInt
      if (score < 1)
        item.remove()
    }
  }

  def convertLinksToText(node: Element) {
    if (node != null) {
      val baseUri = node.baseUri()
      val links = node.getElementsByTag("a")
      links foreach { item =>
        if (item.getElementsByTag("img").isEmpty) {
          val tn = new TextNode(item.text, baseUri)
          item.replaceWith(tn)
        }
      }
    }
  }

  def replaceTagsWithText(node: Element) {
    if (topNode != null) {
      val baseUri = node.baseUri()
      val bolds = node.getElementsByTag("b")
      bolds foreach { item =>
        val tn = new TextNode(getTagCleanedText(item), baseUri)
      }
      val strongs = node.getElementsByTag("strong")
      strongs foreach { item =>
        val tn = new TextNode(getTagCleanedText(item), baseUri)
      }
      val italics = node.getElementsByTag("i")
      italics foreach { item =>
        val tn = new TextNode(getTagCleanedText(item), baseUri)
      }
    }
  }

  def getTagCleanedText(item: Node): String = {
    val sb = new StringBuilder()
    item.childNodes() foreach {
      case childText: TextNode =>
        sb.append(childText.getWholeText)
      case childElement: Element =>
        sb.append(childElement.outerHtml)
      case _ =>
    }
    val text = tagReplace replaceAllIn(sb.toString, "")
    return text
  }

  def removeParagraphsWithFewWords(node: Element) {
    if (node != null) {
      val allNodes = node.getAllElements
      allNodes foreach { el =>
        val count = new StopWords().getStopWordsCount(el.text)
        if (count < 3 && el.getElementsByTag("object").size == 0 && el.getElementsByTag("embed").size == 0)
          el .remove()
      }
      Option(node.getElementsByTag("p").first()) foreach {
        case firstModdedNode: Element =>
          val trimmed = firstModdedNode.text.trim
          if (trimmed.startsWith("(") && trimmed.endsWith(")"))
            firstModdedNode.remove()
      }
    }
  }
}