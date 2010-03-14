package controllers

import play._
import play.mvc._

object Application extends Controller with Secure {
    
  def index() = renderHtml(<h1>Hello</h1>)
    
}

trait Secure  {
  self: Controller =>
    @Before
    def check {
        request.user match {
            case user: String => renderArgs += ("connected" -> user)
            case _ => unauthorized()
        }
    }
    
}
