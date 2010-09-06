package play.cache

/**
 * extends the Cache API with two scala specific methods, this is made public via type alias
 */
private[cache] object ScalaCache extends CacheDelegate {

    /**
     *  retrieves value from Cache based on the type parameter
     *  @param key the name of the cache key
     * @param return either the value or None
     */
    def get[T](key: String)(implicit m: ClassManifest[T]): Option[T] = {
      if (key == null) None
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
     *  @param key the name of the cache key
     *  @param return either the value or None
     *  @param expiration expiration period
     */
    def get[T](key: String, expiration: String)(getter: => T): T = {
        import play.libs.Time._
        get(key) match {
            case Some(x) => x
            case None => val r = getter
                         set(key, r, expiration)
                         r
        }
    }
   import play.libs.Time._
   private def prefixed(key:String)="__"+key

   def get[T](key:String,window:String,expiration:String)(getter: => Option[T]):Option[T]={
     val cacheIt= (v:T) =>  {set(prefixed(key), v,parseDuration(expiration) + parseDuration(window) + "s" )
                             set(key, v,expiration)
                             v}
     get(key).orElse(getter.map(cacheIt)).orElse(get(prefixed(key)))
   }

  import scala.actors.Actor._
  import scala.actors._
  private val cacheActor=
                actor{link{ loop{ react{case Exit(from: Actor, exc: Exception) => {play.Logger.warn(exc.toString); from.restart()}}}}
		      loop{
                        react{
                          case (f:Function0[_]) => reply(f.asInstanceOf[Function0[Any]]())
                          case  _ => None}}}

   private def getFromCache[T](key:String)=Option(_impl.get(key).asInstanceOf[T])
   private def getFromCache1(key:String):Option[_]=Option(_impl.get(key))

   def getAsync[T](key:String,expiration:String,window:String,waitForEvaluation:String="10s")(getter: => Option[T]):Option[T]={
    
     def scheduleOrIgnore(key:String,f:Function0[_])={
       val flagForKey="___"+key
       case class Caching() 
       getFromCache1(flagForKey).getOrElse
           {cacheActor.!!( f,{
              case v:Option[_] =>{cacheIt(v.asInstanceOf[Option[T]]); set(flagForKey,None,waitForEvaluation);v}
              case _ => None
            })
          }
   }

     def cacheIt(t: =>Option[T])= get(key,window,expiration)(t)
     getFromCache[T](key).orElse(getFromCache[T](prefixed(key)).map(v=>{scheduleOrIgnore(key,()=>getter);v})
			 .orElse(cacheIt(getter))) 
   }
   def getAsync2[T](key:String,expiration:String,window:String,waitForEvaluation:String="10s")(getter: => Option[T]):Option[T]={
     getAsync1[Option[T]](key,expiration,window,waitForEvaluation)(getter)(o => o.isDefined)
   }
   def getAsync1[A](key:String,expiration:String,window:String,waitForEvaluation:String="10s")(getter: => A)(implicit isDesirable: A => Boolean):A={
       def scheduleOrIgnore(key:String,f:Function0[A])={
         val flagWhileCaching="___"+key
         case class Caching() 
         getFromCache1(flagWhileCaching).getOrElse
           {cacheActor.!!( ()=>{set(flagWhileCaching,Caching(),waitForEvaluation);f()},{
             case None => None 
             case a   =>{cacheIt(a.asInstanceOf[A]); set(flagWhileCaching,None,waitForEvaluation);Some(a)}
            })
          }
       }

   def cacheIt(t: =>A)= get1(key,window,expiration)(t)(isDesirable)
   
   getFromCache[A](key).getOrElse(
       getFromCache[A](prefixed(key)).map(v=>{scheduleOrIgnore(key,()=>getter);v})
			             .getOrElse(cacheIt(getter))) 
     
   }
   def get1[A](key:String,window:String,expiration:String)(getter: => A)(implicit isDesirable: A => Boolean):A={
     val cacheIt= (v:A) =>  {set(prefixed(key), v,parseDuration(expiration) + parseDuration(window) + "s" )
                             set(key, v,expiration)
                             v}
     get(key).getOrElse({val result=getter;
                         if(isDesirable(result)) cacheIt(result)
                         else get(prefixed(key)).getOrElse(result)})
   }
}
