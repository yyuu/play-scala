package play.mvc;

import play.mvc.Controller;
import play.mvc.Router.ActionDefinition;
import play.exceptions.UnexpectedException;
import play.mvc.results.RenderTemplate;

import java.io.InputStream;
import java.io.File;

import java.util.Map;
import java.util.HashMap;
import java.util.concurrent.Future;


/**
 * 
 * creates a delegate which can be used to take over play.mvc.Controller namespace with a type
 * alias. Extending from this class means that we can avoid circular references which would
 * occur if ScalaController was inhereted directly from @see play.mvc.Controller and we used a type alias
 * to map ScalaController to play.mvc.Controller
 * This class will be removed before 1.1
 *
 */
@Deprecated
public abstract class ControllerDelegate {
    
    /**
     * @Deprecated
     */
    public void renderTemplate(String template, Object... args) {
        Controller.renderTemplate(template, args);
    }

    /**
     * @Deprecated
     */    
    public void renderTemplate(String template, Map<String,Object> args) {
        Controller.renderTemplate(template, args);
    }

    /**
     * @Deprecated
     */    
    public void renderTemplate(Map<String,Object> args) {
        Controller.renderTemplate(args);
    }

    /**
     * @Deprecated
     */
    public void renderText(Object text) {
        Controller.renderText(text);
    }

    /**
     * @Deprecated
     */    
    public void renderHtml(Object text) {
        Controller.renderHtml(text);
    }

    /**
     * @Deprecated
     */
    public void renderText(CharSequence pattern, Object... args) {
        Controller.renderText(pattern, args);
    }

    /**
     * @Deprecated
     */
    public void renderXml(String xml) {
        Controller.renderXml(xml);
    }

    /**
     * @Deprecated
     */    
    public void renderXml(org.w3c.dom.Document xml) {
        Controller.renderXml(xml);
    }

    /**
     * @Deprecated
     */
    public void renderJSON(String json) {
        Controller.renderJSON(json);
    }

    /**
     * @Deprecated
     */
    public void renderJSON(Object anyObject) {
        Controller.renderJSON(anyObject);
    }

    /**
     * @Deprecated
     */
    public void notModified() {
        Controller.notModified();
    }

    /**
     * @Deprecated
     */
    public void todo() {
        Controller.todo();
    }

    /**
     * @Deprecated
     */
    public void redirectToStatic(String file) {
        Controller.redirectToStatic(file);
    }

    /**
     * @Deprecated
     */
    public void redirect(String url) {
        Controller.redirect(url);
    }

    /**
     * @Deprecated
     */
    public void redirect(String url, boolean permanent) {
        Controller.redirect(url, permanent);
    }

    /**
     * @Deprecated
     */
    public void redirect(String action, Object... args) {
       Controller.redirect(action, args);
    }

    /**
     * @Deprecated
     */
    public void redirect(String action, boolean permanent, Object... args) {
        Controller.redirect(action, permanent, args);
    }

    /**
     * @Deprecated
     */
    public void unauthorized(String realm) {
        Controller.unauthorized(realm);
    }

    /**
     * @Deprecated
     */
    public void unauthorized() {
        Controller.unauthorized("");
    }

    /**
     * @Deprecated
     */
    public void notFound(String what) {
        Controller.notFound(what);
    }

    /**
     * @Deprecated
     */
    public void notFound() {
        Controller.notFound("");
    }

    /**
     * @Deprecated
     */
    public void notFoundIfNull(Object o) {
        Controller.notFoundIfNull(o);
    }

    /**
     * @Deprecated
     */
    public void ok() {
        Controller.ok();
    }

    /**
     * @Deprecated
     */    
    public void error(String reason) {
       Controller.error(reason);	 
    }

    /**
     * @Deprecated
     */
    public void error(Exception reason) {
       Controller.error(reason);	 
    }

    /**
     * @Deprecated
     */
    public void error() {
       Controller.error();	 
    }

    /**
     * @Deprecated
     */
    public void error(int status, String reason) {
       Controller.error(status,reason);	 
    }

    /**
     * @Deprecated
     */
    public void renderBinary(InputStream stream) {
        Controller.renderBinary(stream);
    }

    /**
     * @Deprecated
     */
    public void renderBinary(File file) {
        Controller.renderBinary(file);
    }

    /**
     * @Deprecated
     */    
    public void renderBinary(InputStream stream, String name) {
        Controller.renderBinary(stream, name);
    }

    /**
     * @Deprecated
     */
    public void forbidden() {
        Controller.forbidden();
    }

    /**
     * @Deprecated
     */
    public void waitFor(Future task) {
        Controller.waitFor(task);
    }
    
    public ActionDefinition reverse() {
        return Controller.reverse();
    }
    
    // ~~~~ 
    
    public static ActionDefinition _reverse() {
        return Controller.reverse();
    }
    
    public static RenderTemplate _renderTemplate(String template, Map<String,Object> args) {
        try {
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
    
    public static RenderTemplate _renderTemplate(Map<String,Object> args) {
        try {
            Controller.renderTemplate(args);
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
    
    public static RenderTemplate _renderTemplate() {
        return _renderTemplate(new HashMap<String,Object>());
    }
    
    /**
     * @Deprecated
     */
    public void suspend(int millis) {
        Controller.suspend(millis);
    }

    /**
     * @Deprecated
     */
    public void suspend(String timeout) {
        Controller.suspend(timeout);
    }
}
