package play.scalasupport.secure

import play.mvc._
import play.classloading.enhancers.ControllersEnhancer.ByPass
import java.lang.reflect.InvocationTargetException

trait SecureWrapper {

    @Before def checkAccess {
        try {
            play.utils.Java.invokeStatic("controllers.Secure", "checkAccess")   
        } catch {
            case e: InvocationTargetException => throw e.getTargetException
        }     
    } 

}

    
