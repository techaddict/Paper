package io.pilo.network

import concurrent.{ Future, Promise }
import com.ning.http.client.{ AsyncHttpClient, AsyncHttpClientConfig }
import java.util.concurrent.Executor

trait WebClient {
  def get(url: String)(implicit exec: Executor): Future[String]
}

case class BadStatus(status: Int) extends RuntimeException

object AsyncWebClient extends WebClient with io.pilo.Configuration {
  val config = new AsyncHttpClientConfig.Builder()
    config.setUserAgent(browserUserAgent)
      .setRequestTimeoutInMs(requestTimeout)
      .setFollowRedirects(true)
  private val client = new AsyncHttpClient(config.build())

  def get(url: String)(implicit exec: Executor): Future[String] = {
    val f = client.prepareGet(url).execute();
    val p = Promise[String]()
    f.addListener(new Runnable {
      def run = {
        val response = f.get
        //println(response.getStatusCode)
        if (response.getStatusCode / 100 < 4)
          p.success(response.getResponseBodyExcerpt(131072))
        else p.failure(BadStatus(response.getStatusCode))
      }
    }, exec)
    p.future
  }

  def shutdown(): Unit = client.close()
}
