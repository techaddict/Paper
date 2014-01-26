package io.pilo.parse

import io.pilo.Article
import org.jsoup.nodes.{ Element, TextNode, Node, Document }
import org.jsoup.select.Evaluator.Tag
import org.jsoup.select.{ Collector, Elements }
import scala.util.matching.Regex
import java.util.regex.{Matcher, Pattern}
import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

object DocumentCleaner extends io.pilo.Configuration{

  val removeNodes = "^side$|combx|retweet|mediaarticlerelated|menucontainer|navbar|comment|PopularQuestions|contact|foot|footer|Footer|footnote|cnn_strycaptiontxt|links|meta$|scroll|shoutbox|sponsor" +
  "|tags|socialnetworking|socialNetworking|cnnStryHghLght|cnn_stryspcvbx|^inset$|pagetools|post-attributes|welcome_form|contentTools2|the_answers|remember-tool-tip" +
  "|communitypromo|runaroundLeft|subscribe|vcard|articleheadings|date|^print$|popup|author-dropdown|tools|socialtools|byline|konafilter|KonaFilter|breadcrumbs|^fn$|wp-caption-text"
  val naughtyIDs = "[id~=(" + removeNodes + ")]"
  val naughtyClasses = "[class~=(" + removeNodes + ")]"
  val naughtyNames = "[name~=(" + removeNodes + ")]"
  val tabsAndNewLinesReplacements = "".r
  val divToPElementsPattern = Pattern.compile("<(a|blockquote|dl|div|img|ol|p|pre|table|ul)")
  val blockElementTags = Array(new Tag("a"), new Tag("blockquote"), new Tag("dl"), new Tag("div"), new Tag("img"), new Tag("ol"), new Tag("p"), new Tag("pre"), new Tag("table"), new Tag("ul"))
  val articleRootTags = Array(new Tag("div"), new Tag("span"), new Tag("article"))

  val captionPattern = "^caption$"
  val googlePattern = " google "
  val entriesPattern = "^[^entry-]more.*$"
  val facebookPattern = "[^-]facebook"
  val twitterPattern = "[^-]twitter"

  def clean(article: Document): Document = {
    var docToClean = article
    //docToClean = cleanArticleTags(docToClean)
    docToClean = cleanEmTags(docToClean)
    docToClean = removeDropCaps(docToClean)
    docToClean = removeScriptsStylesComments(docToClean)
    docToClean = cleanBadTags(docToClean)
    docToClean = RemoveNodeREGEX(docToClean, captionPattern)
    docToClean = RemoveNodeREGEX(docToClean, googlePattern)
    docToClean = RemoveNodeREGEX(docToClean, entriesPattern)
    docToClean = RemoveNodeREGEX(docToClean, facebookPattern)
    docToClean = RemoveNodeREGEX(docToClean, twitterPattern)
    docToClean = cleanUpSpanTagsInParagraphs(docToClean)
    articleRootTags foreach { articleRootTag =>
      docToClean = convertWantedTagsToParagraphs(docToClean, articleRootTag)
    }
    docToClean
  }

  def cleanArticleTags(doc: Document): Document = {
    doc.getElementsByTag("article").foreach { article =>
      Array("id", "name", "class").foreach { attr =>
        article.removeAttr(attr)
      }
    }
    return doc
  }

  def cleanEmTags(doc: Document): Document = {
    doc.getElementsByTag("em") foreach { elem =>
      val images = doc.getElementsByTag("img")
      if (images.size == 0)
        elem.remove()
    }
    return doc
  }

  def removeDropCaps(doc: Document): Document = {
    doc.select("span[class~=(dropcap|drop_cap)]") foreach { elem =>
      doc.replaceWith(new TextNode(elem.text, doc.baseUri))
    }
    return doc
  }

  def cleanUpSpanTagsInParagraphs(doc: Document): Document = {
    doc.getElementsByTag("span") foreach { item =>
      if (item.parent().nodeName == "p") {
        item.replaceWith(new TextNode(item.text, doc.baseUri))
      }
    }
    return doc
  }

  def removeScriptsStylesComments(doc: Document): Document = {
    doc.getElementsByTag("script").foreach(_.remove())
    doc.getElementsByTag("noscript").foreach(_.remove())
    doc.getElementsByTag("style").foreach(_.remove())

    def removeComments(node: Node): Unit = {
      var i = 0
      while (i < node.childNodes().size()) {
          val child = node.childNode(i);
          if (child.nodeName().equals("#comment"))
              child.remove();
          else {
              removeComments(child);
              i += 1
          }
      }
    }
    removeComments(doc)
    return doc
  }

  def cleanBadTags(doc: Document): Document = {
    val children = doc.body.children
    children.select(naughtyIDs).foreach(_.remove())
    children.select(naughtyClasses).foreach(_.remove())
    children.select(naughtyNames).foreach(_.remove())
    return doc
  }

  def RemoveNodeREGEX(doc: Document, pattern: String): Document = {
    doc.getElementsByAttributeValueMatching("id", pattern).foreach(_.remove())
    doc.getElementsByAttributeValueMatching("class", pattern).foreach(_.remove())
    return doc
  }

  def replaceElementsWithPara(doc: Document, div: Element) {
    try {
      val newDoc = new Document(doc.baseUri)
      val newNode = newDoc.createElement("p")
      newNode.append(div.html)
      div.replaceWith(newNode)
    } catch {
      case _: Exception =>
    }
  }

  def convertWantedTagsToParagraphs(doc: Document, wantedTags: Tag): Document = {
    Collector.collect(wantedTags, doc) foreach { elem =>
      blockElementTags foreach { blockEle =>
        if(Collector.collect(blockEle, elem).isEmpty){
          //println("in")
          if (doc != null && elem != null)
            replaceElementsWithPara(doc, elem)
          //println("out\n\n\n")
        }
        else {
          val replacements = getReplacementNodes(doc, elem)
          elem.children() foreach(_.remove())
          replacements foreach{n =>
            try {
              elem.appendChild(n)
            } catch {
              case _: Exception => println("Failed")
            }
          }
        }
      }
    }
    return doc
  }

  def convertDivsToParagraphs(doc: Document, domType: String): Document = {
    var badDivs = 0
    var convertedTextNodes = 0
    val divs: Elements = doc.getElementsByTag(domType)
    var divIndex = 0
    divs foreach { div =>
      val divToPElementsMatcher = divToPElementsPattern.matcher(div.html.toLowerCase)
      if (divToPElementsMatcher.find == false) {
        replaceElementsWithPara(doc, div)
        badDivs += 1
      }
      else {
        val replaceNodes = getReplacementNodes(doc, div)
        div.children.foreach(_.remove())
        replaceNodes.foreach { node =>
          div.appendChild(node)
        }
      }
      divIndex += 1
    }
    return doc
  }

  def getFlushedBuffer(replacementText: StringBuilder, doc: Document): Node = {
    val newDoc = new Document(doc.baseUri)
    val newPara = newDoc.createElement("p")
    newPara.html(replacementText.toString)
    newPara
  }

  def getReplacementNodes(doc: Document, div: Element) = {
    val replacementText = new StringBuilder
    val nodesToReturn = new ListBuffer[Node]()
    val nodesToRemove = new ListBuffer[Node]()
    div.childNodes foreach { kid =>
      if (kid.nodeName == "p" && replacementText.size > 0) {
        val newNode = getFlushedBuffer(replacementText, doc)
        nodesToRemove += newNode
        replacementText.clear()
        if(kid.isInstanceOf[Element])
          nodesToReturn += kid.asInstanceOf[Element]
      }
      else if (kid.nodeName == "#text") {
        val kidTextNode = kid.asInstanceOf[TextNode]
        val kidText = kidTextNode.attr("text")
        val replaceText = tabsAndNewLinesReplacements.replaceAllIn(kidText, "")
        if (replaceText.trim().length > 1) {
          var prevSibNode = kidTextNode.previousSibling
          while (prevSibNode != null && prevSibNode.nodeName == "a" && prevSibNode.attr("grv-usedalready") != "yes") {
            replacementText.append(" " + prevSibNode.outerHtml + " ")
            nodesToRemove += prevSibNode
            prevSibNode.attr("grv-usedalready", "yes")
            prevSibNode = if (prevSibNode.previousSibling == null) null else prevSibNode.previousSibling
          }
          replacementText.append(replaceText)
          var nextSibNode = kidTextNode.nextSibling()
          while (nextSibNode != null && nextSibNode.nodeName == "a" && nextSibNode.attr("grv-usedalready") != "yes") {
            replacementText.append(" " + nextSibNode.outerHtml + " ")
            nodesToRemove += nextSibNode
            nextSibNode.attr("grv-usedalready", "yes")
            nextSibNode = if (nextSibNode.nextSibling == null) null else nextSibNode.nextSibling
          }
        }
        nodesToRemove += kid
      }
      else
        nodesToReturn += kid
    }
    if (replacementText.size > 0) {
      nodesToReturn += getFlushedBuffer(replacementText, doc)
      replacementText.clear()
    }
    nodesToRemove.foreach(_.remove())
    nodesToReturn
  }
}