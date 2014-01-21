package io.pilo.util.url

object Url{
  val protocols = Array("http", "https")
  def parseQs(qs: String): Map[String, Array[String]] = {
    var r: Map[String, Array[String]] = Map()
    var pairs = qs.split(Array('&', ';')).filter(x => x != "")
    for (pair <- pairs) {
      var nv = pair.split("=", 2)
      if (nv.length == 2 && nv(1).length > 0) {
        // Add (%20 etc.)iso-latin to utf-8
        var name = nv(0).replaceAll("+", " ")
        var value = nv(1).replaceAll("+", " ")
        if(r.contains(name)) {
          val tempVal = r(name) ++ Array(value)
          r -= name
          r += name -> tempVal
        }
        else r += name -> Array(value)
      }
    }
    return r
  }

  def urlUnParse(in: (String, String, String, String, String, String)): String = {
    var (scheme, netloc, url, params, query, fragment) = in
    if (params != "") url = s"$url;$params"
    return urlUnSplit((scheme, netloc, url, query, fragment))
  }

  def urlJoin(base: String, url: String): String = {

    import scala.util.control.Breaks._

    if (base == "") return url
    if (url == "") return base
    var (bscheme, bnetloc, bpath, bparams, bquery, bfragment) = urlParse(base)
    var (scheme, netloc, path, params, query, fragment) = urlParse(url)
    if (scheme != bscheme || !protocols.contains(scheme)) return url
    if (protocols.contains(scheme)) {
      if (netloc != "") return urlUnParse((scheme, netloc, path, params, query, fragment))
      netloc = bnetloc
    }
    if (path.substring(0, 1) == "/") return urlUnParse((scheme, netloc, path, params, query, fragment))
    if (path == "") {
      if (params == "") {
        params = bparams
        if (query == "") {
          query = bquery
        }
      }
      return urlUnParse((scheme, netloc, path, params, query, fragment))
    }
    var segments = bpath.split('/').dropRight(1) ++ path.split('/')
    if (segments.last == ".") {
      segments.dropRight(1)
      segments ++= Array("")
    }
    segments = segments.filter(x => x != ".")

    breakable {
      for (i <- 1 to segments.length - 1; if (segments(i) == ".." && !Array("", "..").contains(segments(i - 1)))) {
        segments = segments.take(i - 1) ++ segments.drop(i + 1)
        break
      }
    }
    if (segments == Array("", "..")) {
      segments.dropRight(1)
      segments ++= Array("")
    }
    else if (segments.length > 2 && segments.last == "..") {
      segments.dropRight(2)
      segments ++= Array("")
    }
    return urlUnParse((scheme, netloc, segments.mkString("/"), params, query, fragment))
  }
  def urlUnSplit(in: (String, String, String, String, String)): String = {
    var (scheme, netloc, url, query, fragment) = in
    if (netloc != "" || (scheme != "" && protocols.contains(scheme) && url.substring(0, 2) != "//")) {
      if (url != "" && url.substring(0, 1) != "/") url = "/" + url
      if (netloc != "") url = "//" + netloc + url
      else url = "//" + url
    }
    if (scheme != "") url = scheme + ":" + url
    if (query != "") url = url + "?" + query
    if (fragment != "") url = url + "#" + fragment
    return url
  }

  def urlParse(url: String): (String, String, String, String, String, String) = {
    var params = ""
    var (scheme, netloc, url1, query, fragment) = urlSplit(url)
    var i = url1.indexOf(";")
    if (protocols.contains(scheme) && (i > 0) ) {
      val tuple = splitParams(url)
      url1 = tuple._1
      params = tuple._2
    }
    return (scheme, netloc, url1, params, query, fragment)
  }

  def splitParams(url: String): (String, String) = {
    var i = url.indexOf("/")
    if (i > 0) {
      i = url.indexOf(";", url.lastIndexOf("/"))
      if (i < 0) {
        return (url, "")
      }
    }
    else {
      i = url.indexOf(";")
    }
    return (url.substring(0, i), url.substring(i+1))
  }

  def urlSplit(url1: String): (String, String, String, String, String) = {
    var url: String = url1
    var netloc, query, fragment, scheme = ""
    var j: Int = 0
    var temp: Array[String] = null.asInstanceOf[Array[String]]
    var i: Int = url.indexOf(":")
    if (i > 0) {
      scheme = url.substring(0, i).toLowerCase()
      if (protocols.contains(scheme)) {
        url = url.substring(i+1)
        if (url.substring(0, 2) == "//") {
          i = url.indexOf('/', 2)
          if (i < 0) {
            i = url.indexOf('#')
            if (i < 0){
              i = url.length()
            }
          }
          netloc = url.substring(2, i)
          url = url.substring(i)
        }
        j = url.indexOf('#')
        if (j > 0) {
          temp = url.split("#")
          url = temp(0)
          fragment = temp(1)
        }
        j = url.indexOf('?')
        if (j > 0) {
          temp = url.split("\\?")
          url = temp(0)
          query = temp(1)
        }
        return (scheme, netloc, url, query, fragment)
        }
      }
      return (scheme, netloc, url, query, fragment)
    }
}