package io.pilo

import org.jsoup.Jsoup

object Main extends App {
  //var txt = util.helpers.FileHelper.loadResourceFile("test.txt")
  val txt = "Now that conventional thinking has been turned on its head in a paper by Prof Christof Wetterich at the University of Heidelberg in Germany. He points out that the tell-tale light emitted by atoms is also governed by the masses of their constituent particles, notably their electrons. The way these absorb and emit light would shift towards the blue part of the spectrum if atoms were to grow in mass, and to the red if they lost it.  Because the frequency or ÒpitchÓ of light increases with mass, Prof Wetterich argues that masses could have been lower long ago. If they had been constantly increasing, the colours of old galaxies would look red-shifted Ð and the degree of red shift would depend on how far away they were from Earth. ÒNone of my colleagues has so far found any fault [with this],Ó he says.  Although his research has yet to be published in a peer-reviewed publication, Nature reports that the idea that the universe is not expanding at all Ð or even contracting Ð is being taken seriously by some experts, such as Dr HongSheng Zhao, a cosmologist at the University of St Andrews who has worked on an alternative theory of gravity. ÒI see no fault in [Prof WetterichÕs] mathematical treatment,Ó he says. ÒThere were rudimentary versions of this idea two decades ago, and I think it is fascinating to explore this alternative representation of the cosmic expansion, where the evolution of the universe is like a piano keyboard played out from low to high pitch.Ó  Prof Wetterich takes the detached, even playful, view that his work marks a change in perspective, with two different views of reality: either the distances between galaxies grow, as in the traditional balloon picture, or the size of atoms shrinks, increasing their mass. Or itÕs a complex blend of the two. One benefit of this idea is that he is able to rid physics of the singularity at the start of time, a nasty infinity where the laws of physics break down. Instead, the Big Bang is smeared over the distant past: the first note of the ''cosmic pianoÕÕ was long and low-pitched.  Harry Cliff, a physicist working at CERN who is the Science MuseumÕs fellow of modern science, thinks it striking that a universe where particles are getting heavier could look identical to one where space/time is expanding. ÒFinding two different ways of thinking about the same problem often leads to new insights,Ó he says. ÒString theory, for instance, is full of 'dualitiesÕ like this, which allow theorists to pick whichever view makes their calculations simpler.Ó  If this idea turns out to be right Ð and that is a very big if Ð it could pave the way for new ways to think about our universe. If we are lucky, they might even be as revolutionary as Edwin HubbleÕs, almost a century ago.  Roger Highfield is director of external affairs at the Science Museum"
  var x = new text.Nlp()
  //println(x.splitWords(txt.toString) map {x=>println(x)})
  x.summarize("", "Astronomic news: the universe may not be expanding after all", txt) map {x => println(x.sentence)}

  //Async Tester
  import scala.concurrent.ExecutionContext.Implicits.global
  //  AsyncWebClient get "http://www.google.co.in/?gws_rd=cr&amp;ei=P4qwUoDKJoKOrQeN8YGQBg" map println foreach (_ => AsyncWebClient.shutdown())

  // Article Tester
  val a = new Article("http://www.google.co.in/?gws_rd=cr&amp;ei=P4qwUoDKJoKOrQeN8YGQBg", "google", "")
  a.download
  a.html onSuccess {
    case content =>
      println("sucess")
      network.AsyncWebClient.shutdown()
  }
  a.html onFailure {
    case e =>
      println("failed" + e)
      network.AsyncWebClient.shutdown()
  }

  //Cleaner Tester
  import org.jsoup.Jsoup
  import parse.DocumentCleaner._
  val content = util.helpers.FileHelper.loadResourceFile("testhtml/aol1.txt")
  println(content.length)
  var html = Jsoup.parse(content)
  html = clean(html)
  println(html.toString)
}