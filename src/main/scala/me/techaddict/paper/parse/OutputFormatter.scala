package me.techaddict.paper.parse

import scala.xml.Utility
import org.jsoup.nodes.Document
import org.jsoup.Jsoup
import me.techaddict.paper.Article

object OutputFormatter extends me.techaddict.paper.Configuration{
  var topNode = None

  def convertToText(doc: Document): Array[String] = {
    var txts = Array[String]()
    topNode foreach { node =>
      println("node" + node)
      var txt = doc.body.text().toString
      txt = Utility.unescape(txt, new StringBuilder("")).toString
      var txt_lis = txt.filter(_ >= ' ').split("\n")
      txts = txts ++ txt_lis
    }
    return txts
  }
}