package play.cache

/**
 * extends the Cache API with two scala specific methods 
 */
object ScalaCache extends CacheDelegate {

    /**
     *  retrieves value from Cache based on the type parameter
     *  @key the name of the cache key
     * @return either the value or None
     */
    def get[T](key: String)(implicit m: ClassManifest[T]): Option[T] = {
        val v = _impl.get(key).asInstanceOf[T] 
        if (v == null) None
        else if (m.erasure.isAssignableFrom(v.asInstanceOf[AnyRef].getClass)) {
          Some(v)
        } else {
          play.Logger.warn("Found a value in cache for key '%s' of type %s where %s was expected", key, v.asInstanceOf[AnyRef].getClass.getName, m.erasure.getName)
          None
        }
    }
  
     /**
     *  retrieves value from Cache based on the type parameter
     *  @key the name of the cache key
     *  @return either the value or None
     *  @expiration expiration period
     */
    def get[T](key: String, expiration: String)(getter: => T): T = {
        get(key) match {
            case Some(x) => x
            case None => val r = getter
                         set(key, r, expiration)
                         r
        }
    }
    
}
