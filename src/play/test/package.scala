package play {

    package object test {
        
        // Helper to deal with Yaml fixtures in a type safe way
        def Yaml[T](name: String)(implicit m:ClassManifest[T]) = {        
            import scala.collection.JavaConversions._        
            m.erasure.getName match {
                case "scala.collection.immutable.List" => play.test.Fixtures.loadYamlAsList(name).toList.asInstanceOf[T]
                case "scala.collection.immutable.Map"  => play.test.Fixtures.loadYamlAsMap(name).toMap[Any,Any].asInstanceOf[T]
                case _                                 => play.test.Fixtures.loadYaml(name, m.erasure).asInstanceOf[T]
            }
        } 

    }

}
