package io.pilo.parse

import io.pilo.Article
import org.jsoup.nodes.Document

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
      val node = it.next
      val images = doc.getElementsByTag("img")
      if (images.size == 0)
        node.remove()
    }
    return doc
  }

  def removeDropCaps(doc1: Document): Document = {
    doc1
  }
}