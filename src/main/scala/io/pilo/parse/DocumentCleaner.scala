package io.pilo.parse

import io.pilo.Article
import org.jsoup.nodes.{ TextNode, Node, Document }
import org.jsoup.select.Evaluator.Tag
import scala.util.matching.Regex

object DocumentCleaner extends io.pilo.Configuration{

  val removeNodes = "^side$|combx|retweet|mediaarticlerelated|menucontainer|navbar|comment|PopularQuestions|contact|foot|footer|Footer|footnote|cnn_strycaptiontxt|links|meta$|scroll|shoutbox|sponsor" +
  "|tags|socialnetworking|socialNetworking|cnnStryHghLght|cnn_stryspcvbx|^inset$|pagetools|post-attributes|welcome_form|contentTools2|the_answers|remember-tool-tip" +
  "|communitypromo|runaroundLeft|subscribe|vcard|articleheadings|date|^print$|popup|author-dropdown|tools|socialtools|byline|konafilter|KonaFilter|breadcrumbs|^fn$|wp-caption-text"
  val naughtyIDs = "[id~=(" + removeNodes + ")]"
  val naughtyClasses = "[class~=(" + removeNodes + ")]"
  val naughtyNames = "[name~=(" + removeNodes + ")]"

  def clean(article: Article): Unit = {
    var docToClean = article.doc
    docToClean = cleanArticleTags(docToClean)
    docToClean = cleanEmTags(docToClean)
    docToClean = removeDropCaps(docToClean)
  }

  def cleanArticleTags(doc: Document): Document = {
    val it = doc.getElementsByTag("article").iterator()
    while(it.hasNext()) {
      val article = it.next
      Array("id", "name", "class").foreach { attr =>
        article.removeAttr(attr)
      }
    }
    return doc
  }

  def cleanEmTags(doc: Document): Document = {
    val it = doc.getElementsByTag("em").iterator()
    while(it.hasNext()) {
      val images = doc.getElementsByTag("img")
      if (images.size == 0)
        it.next.remove()
    }
    return doc
  }

  def removeDropCaps(doc: Document): Document = {
    var it = doc.select("span[class~=(dropcap|drop_cap)]").iterator()
    while(it.hasNext()) {
      val tn = new TextNode(it.next.text, doc.baseUri)
      doc.replaceWith(tn)
    }
    return doc
  }

  def removeScriptsStylesComments(doc: Document): Document = {
    var it = doc.getElementsByTag("script").iterator()
    while(it.hasNext())
      it.next.remove()

    it = doc.getElementsByTag("style").iterator()
    while(it.hasNext())
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
    while(it.hasNext)
      it.next.remove()

    it = children.select(naughtyClasses).iterator()
    while(it.hasNext)
      it.next.remove()

    it = children.select(naughtyNames).iterator()
    while(it.hasNext)
      it.next.remove()

    return doc
  }

  def RemoveNodeREGEX(doc: Document, pattern: String): Document = {
    var it = doc.getElementsByAttributeValueMatching("id", pattern).iterator()
    while(it.hasNext)
      it.next.remove()

    it = doc.getElementsByAttributeValueMatching("class", pattern).iterator()
    while(it.hasNext)
      it.next.remove()

    return doc
  }
}