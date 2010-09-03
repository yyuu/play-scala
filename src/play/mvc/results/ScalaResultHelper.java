package play.mvc.results;

import play.mvc.Controller;
import play.mvc.Router.ActionDefinition;
import play.exceptions.UnexpectedException;
import play.mvc.results.RenderTemplate;

import java.io.InputStream;
import java.io.File;

import java.util.Map;
import java.util.HashMap;
import java.util.concurrent.Future;


class ScalaResultHelper extends Controller{
    
    
    public static ActionDefinition _reverse() {
        return Controller.reverse();
    }
    
    public static RenderTemplate _renderTemplate(String template, Map<String,Object> args) {
	try{    
	  if (template == null)
	    Controller.renderTemplate(args);
	  else
            Controller.renderTemplate(template, args);
        } catch(Throwable t) {
            if(t instanceof RenderTemplate) {
                return (RenderTemplate)t;
            }
            if(t instanceof RuntimeException) {
                throw (RuntimeException)t;
            }
            throw new UnexpectedException(t);
        }
        return null;
    }
    
    
    
}
