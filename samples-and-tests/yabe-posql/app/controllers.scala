package controllers

import play._
import play.mvc._

object Application extends Controller {
    
    def index = {
        
        val o = new Object with scala.util.parsing.combinator.Parsers
        import o._
        
        val all: String ~ Int ~ List[Char] = null
        org.h2.tools.Server.main(null)
        Template
    }

}
