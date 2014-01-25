package io.pilo.parse

import io.pilo.Article
import org.jsoup.nodes.{ Node, Document }

object DocumentCleaner extends io.pilo.Configuration{

  def clean(article: Article): Unit = {
    var docToClean = article.doc
    docToClean = cleanArticleTags(docToClean)
    docToClean = cleanEmTags(docToClean)
    docToClean = removeDropCaps(docToClean)
  }

  def cleanArticleTags(doc1: Document): Document = {
    var doc = doc1
    val it = doc.getElementsByTag("article").iterator()
    while(it.hasNext()) {
      val article = it.next
      Array("id", "name", "class").foreach { attr =>
        article.removeAttr(attr)
      }
    }
    return doc
  }

  def cleanEmTags(doc1: Document): Document = {
    var doc = doc1
    val it = doc.getElementsByTag("em").iterator()
    while(it.hasNext()) {
      val images = doc.getElementsByTag("img")
      if (images.size == 0)
        it.next.remove()
    }
    return doc
  }

  def removeDropCaps(doc1: Document): Document = {
    doc1
  }

  def removeScriptsStyles(doc1: Document): Document = {
    var doc = doc1
    var it = doc.getElementsByTag("script").iterator()
    while(it.hasNext()) {
      it.next.remove()
    }

    it = doc.getElementsByTag("style").iterator()
    while(it.hasNext()) {
      it.next.remove()
    }

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
}