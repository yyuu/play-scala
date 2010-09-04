package play.mvc.results

import play.mvc.Http
import play.mvc.Http

class ScalaAction(action: => Any) extends Result {

  action 

  val reversed = ScalaResultHelper._reverse()
  val url = reversed.url
  val delegate = new Redirect(url) 

  def apply(request: Http.Request , response:Http.Response) {
    delegate.apply(request, response)
  }
  
}
