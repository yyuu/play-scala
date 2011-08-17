package controllers

import play._
import play.mvc._

object Application extends Controller {
    
    import views.Application._
    
    def index = {
        val file = Play.getFile("generated/MagicN.scala")
        play.libs.IO.writeContent(txt.packaged(22).toString, file)

        Range(2,23).foreach { i =>
            val f = Play.getFile("generated/Magic"+i+".scala")
            play.libs.IO.writeContent(txt.index(i).toString, f)
        }
    }

}
