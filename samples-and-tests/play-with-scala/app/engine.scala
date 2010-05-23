import scala.collection.mutable._

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

        def index {
            env.Env.out set ListBuffer[String]()
            new Scrapbook()
            "results.html".render("results" -> env.Env.out.get)
        } 

    }
    
}