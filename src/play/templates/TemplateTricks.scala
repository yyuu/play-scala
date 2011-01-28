package play.templates

object CustomGroovy {
    
    def apply() {
        
        import groovy.lang._
        
        new GroovyShell().evaluate("""
            
            java.lang.Object.metaClass.propertyMissing = { name ->
                try {
                    delegate.getClass().getMethod(name)
                } catch(NoSuchMethodException e) {
                    throw new MissingPropertyException(name, delegate.getClass())
                }
                throw new MissingPropertyException("No such property: ${name} for class: ${delegate.getClass()}. Try a method call ${name}() instead", name, delegate.getClass()) 
            }

            java.lang.Object.metaClass.safeNull = { -> delegate }

            scala.Option.metaClass.safeNull = { -> 
                if(delegate.isDefined()) {
                    delegate.get()
                } else {
                    null
                }
            }
            
        """)
    }
    
}

object TemplateScalaExtensions {
    
    import play.templates.JavaExtensions
    import scala.collection._
    
    def pluralize(n: SeqLike[_,_]) = JavaExtensions.pluralize(n.size)
    def pluralize(n: SeqLike[_,_], plural: String) = JavaExtensions.pluralize(n.size, plural)    
    def pluralize(n: SeqLike[_,_], forms: Array[String]) = JavaExtensions.pluralize(n.size, forms)
    
    def o(so: Class[_]) = play.Play.classloader.loadClass(so.getName()+"$").getField("MODULE$").get(null)
    
    def json(it: Any): String = {
        it match {
            case l: List[_] => "[" + l.map( json(_) ).reduceLeft( _ + "," + _ ) + "]"
            case t: Tuple1[_] => "{'_1': " + json(t._1) + "}"
            case t: Tuple2[_,_] => "{'_1': " + json(t._1) + ", '_2': " + json(t._2) + "}"
            case t: Tuple3[_,_,_] => "{'_1': " + json(t._1) + ", '_2': " + json(t._2) + ", '_3': " + json(t._3) + "}"
            case s: String => "'" + s.replace("'", "\\'") + "'"
            case _ => it.toString
        }
    }

}