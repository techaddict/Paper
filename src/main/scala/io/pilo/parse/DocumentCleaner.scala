package io.pilo.parse

import io.pilo.Article
import org.jsoup.nodes.{ Element, TextNode, Node, Document }
import org.jsoup.select.Evaluator.Tag
import org.jsoup.select.Collector
import scala.util.matching.Regex
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
  val divToPElementsPattern = "<(a|blockquote|dl|div|img|ol|p|pre|table|ul)"
  val blockElemementTags = List[Tag]("a", "blockquote", "dl", "div", "img", "ol", "p", "pre", "table", "ul")
  val articleRootTags = List[Tag]("div", "span", "article")

  def clean(article: Article): Unit = {
    var docToClean = article.doc
    docToClean = cleanArticleTags(docToClean)
    docToClean = cleanEmTags(docToClean)
    docToClean = removeDropCaps(docToClean)
  }

  def cleanArticleTags(doc: Document): Document = {
    val it = doc.getElementsByTag("article").iterator()
    while (it.hasNext()) {
      val article = it.next
      Array("id", "name", "class").foreach { attr =>
        article.removeAttr(attr)
      }
    }
    return doc
  }

  def cleanEmTags(doc: Document): Document = {
    val it = doc.getElementsByTag("em").iterator()
    while (it.hasNext()) {
      val images = doc.getElementsByTag("img")
      if (images.size == 0)
        it.next.remove()
    }
    return doc
  }

  def removeDropCaps(doc: Document): Document = {
    var it = doc.select("span[class~=(dropcap|drop_cap)]").iterator()
    while (it.hasNext()) {
      val tn = new TextNode(it.next.text, doc.baseUri)
      doc.replaceWith(tn)
    }
    return doc
  }

  def removeScriptsStylesComments(doc: Document): Document = {
    var it = doc.getElementsByTag("script").iterator()
    while (it.hasNext())
      it.next.remove()

    it = doc.getElementsByTag("style").iterator()
    while (it.hasNext())
      it.next.remove()

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
    var it = children.select(naughtyIDs).iterator()
    while (it.hasNext)
      it.next.remove()

    it = children.select(naughtyClasses).iterator()
    while (it.hasNext)
      it.next.remove()

    it = children.select(naughtyNames).iterator()
    while (it.hasNext)
      it.next.remove()

    return doc
  }

  def RemoveNodeREGEX(doc: Document, pattern: String): Document = {
    var it = doc.getElementsByAttributeValueMatching("id", pattern).iterator()
    while (it.hasNext)
      it.next.remove()

    it = doc.getElementsByAttributeValueMatching("class", pattern).iterator()
    while (it.hasNext)
      it.next.remove()

    return doc
  }

  def replaceElementsWithPara(doc: Document, div: Element) {
    val newDoc = new Document(doc.baseUri)
    val newNode = newDoc.createElement("p")
    newNode.append(div.html)
    div.replaceWith(newNode)
  }

  def convertWantedTagsToParagraphs(doc: Document, wantedTags: Tag): Document = {
    val it = Collector.collect(wantedTags, doc).iterator()
    while (it.hasNext) {
      var elem = it.next
      if(Collector.collect(blockElemementTags, elem).isEmpty)
        replaceElementsWithPara(doc, elem)
      else {
        val replacements = getReplacementNodes(doc, elem)
        elem.children() foreach(_.remove())
        replacements foreach(n => elem.appendChild(n))
      }
    }
    return doc
  }

  def getFlushedBuffer(replacementText: String, doc: Document): Node = {
    val newDoc = new Document(doc.baseUri)
    val newPara = newDoc.createElement("p")
    newPara.html(replacementText)
    newPara
  }

  def getReplacementNodes(doc: Document, div: Element) = {
    val replacementText = new StringBuilder
    val nodesToReturn = new ListBuffer[Node]()
    val nodesToRemove = new ListBuffer[Node]()
    div.childNodes.foreach{ kid =>
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
        val replaceText = tabsAndNewLinesReplacements.replaceAll(kidText)
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
      nodesToReturn += getFlushedBuffer(replacementText.toString, doc)
      replacementText.clear()
    }
    nodesToRemove.foreach(_.remove())
    nodesToReturn
  }
}