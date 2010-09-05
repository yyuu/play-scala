package play.mvc.results

import play.mvc.Http
import play.mvc.Http
import play.mvc.ControllerDelegate

class ScalaAction(action: => Any) extends Result {

  action 

  val url = ControllerDelegate.reverseForScala().url
  val delegate = new Redirect(url) 

  def apply(request: Http.Request , response:Http.Response) {
    delegate.apply(request, response)
  }
  
}
