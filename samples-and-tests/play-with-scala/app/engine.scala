import scala.collection.mutable._
import play.scalasupport.core.OnTheFly

package object play_with_scala {
    
    def println(v: Any) {
        env.Env.out.get += v.toString
    }
    
    def print(v: Any) {
        println(v)
    }
    
}

package env {
    
    object Env {        
        val out = new ThreadLocal[ListBuffer[String]]
    }
    
}

package controllers {
    

    import play.mvc._
    import play_with_scala._

    object Application extends Controller {
    
      val input = """package play_with_scala
                     class Scrapbook {
                       println("hello world")
                     }"""

        def index {
            env.Env.out set ListBuffer[String]()
            //OnTheFly.eval(input) 
            val c = Class.forName("play_with_scala.Scrapbook")
            c.newInstance()
            "results.html".render("results" -> env.Env.out.get)
        } 

    }
    
}

