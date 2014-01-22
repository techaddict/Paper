import io.pilo.util.url

class Parse {
  import io.pilo.util.url._

  val goodPaths = Array("story", "article", "feature", "featured", "slides", "slideshow", "gallery", "news", "video", "media", "v", "radio", "press")
  val badChunks = Array("careers", "contact", "about", "faq", "terms", "privacy", "adverd", "preferences", "feedback", "info", "browse", "howto", "account", "subscribe", "donate", "shop", "admin")
  val badDomains = Array("amazon", "doubleclick", "twitter")

  def removeArgs(url: String): String = {
    var tuple = Url.urlSplit(url)
    return Url.urlUnSplit((tuple._1, tuple._2, tuple._3, "", ""))
  }

  def redirectBack(url: String, sourceDomain: String): String = {
    var parsedData = Url.urlParse(url)
    var domain = parsedData._2
    var query = parsedData._5

    if(domain.contains(sourceDomain) || sourceDomain.contains(domain)){
      return url
    }

    var queryItem = Url.parseQs(query)
    if(queryItem.contains("url")) {
      return queryItem("url")(0)
    }
    return url
  }

  def prepareUrl(url: String, sourceUrl: String = ""): String = {
    var properUrl = ""
    try {
      if(sourceUrl != "") {
        val sourceDomain = Url.urlParse(sourceUrl)._2
        properUrl = Url.urlJoin(sourceUrl, url)
        properUrl = redirectBack(properUrl, sourceDomain)
        properUrl = removeArgs(properUrl)
      }
      else {
        properUrl = removeArgs(url)
      }
    } catch {
      case e: Throwable => println("url failed on"+ e)
    }
    return properUrl
  }

  def validUrl(url: String) = {
    import org.apache.commons.validator.routines.UrlValidator
    new UrlValidator().isValid(url)
  }

  def getDomain(absUrl: String): String = {
    if(absUrl == "") return ""
    else return Url.urlParse(absUrl)._2
  }

  def getScheme(absUrl: String): String = {
    if(absUrl == "") return ""
    else return Url.urlParse(absUrl)._1
  }

  def getPath(absUrl: String): String = {
    if(absUrl == "") return ""
    else return Url.urlParse(absUrl)._3
  }
}