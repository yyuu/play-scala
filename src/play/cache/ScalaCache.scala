package play.cache

object ScalaCache extends CacheDeleguate {
    
    def get[T](key: String)(implicit m: ClassManifest[T]): Option[T] = {
        _impl.get(key).asInstanceOf[T] match {
            case v: T => if(m.erasure.isAssignableFrom(v.asInstanceOf[AnyRef].getClass)) {
                            Some(v)
                         } else {
                            play.Logger.warn("Found a value in cache for key '%s' of type %s where %s was expected", key, v.asInstanceOf[AnyRef].getClass.getName, m.erasure.getName)
                            None
                         }
            case _ => None
        }
    }
    
    def get[T](key: String, expiration: String)(getter: => T): T = {
        get(key) match {
            case Some(x) => x
            case None => val r = getter
                         set(key, r, expiration)
                         r
        }
    }
    
}
