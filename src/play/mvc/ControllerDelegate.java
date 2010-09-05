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
 * provides java interop
 */
public abstract class ControllerDelegate {

    // ~~~~ 
    
    public static ActionDefinition reverseForScala() {
        return Controller.reverse();
    }

    public ActionDefinition reverse() {
        return Controller.reverse();
    }
    
    public static RenderTemplate renderTemplateForScala(String template, Map<String,Object> args) {
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
    
    /**
     * @Deprecated this method exists only for java interop, return a corresponding Result type (Template, Redirect, Suspend etc.) instead
     */
    @Deprecated
    public void renderTemplate(String template, Object... args) {
        Controller.renderTemplate(template, args);
    }

    /**
     * @Deprecated this method exists only for java interop, return a corresponding Result type (Template, Redirect, Suspend etc.) instead
     */    
    @Deprecated
    public void renderTemplate(String template, Map<String,Object> args) {
        Controller.renderTemplate(template, args);
    }

    /**
     * @Deprecated this method exists only for java interop, return a corresponding Result type (Template, Redirect, Suspend etc.) instead
     */    
    @Deprecated
    public void renderTemplate(Map<String,Object> args) {
        Controller.renderTemplate(args);
    }

    /**
     * @Deprecated this method exists only for java interop, return a corresponding Result type (Template, Redirect, Suspend etc.) instead
     */
    @Deprecated
    public void renderText(Object text) {
        Controller.renderText(text);
    }

    /**
     * @Deprecated this method exists only for java interop, return a corresponding Result type (Template, Redirect, Suspend etc.) instead
     */    
    @Deprecated
    public void renderHtml(Object text) {
        Controller.renderHtml(text);
    }

    /**
     * @Deprecated this method exists only for java interop, return a corresponding Result type (Template, Redirect, Suspend etc.) instead
     */
    @Deprecated
    public void renderText(CharSequence pattern, Object... args) {
        Controller.renderText(pattern, args);
    }

    /**
     * @Deprecated this method exists only for java interop, return a corresponding Result type (Template, Redirect, Suspend etc.) instead
     */
    @Deprecated
    public void renderXml(String xml) {
        Controller.renderXml(xml);
    }

    /**
     * @Deprecated this method exists only for java interop, return a corresponding Result type (Template, Redirect, Suspend etc.) instead
     */    
    @Deprecated
    public void renderXml(org.w3c.dom.Document xml) {
        Controller.renderXml(xml);
    }

    /**
     * @Deprecated this method exists only for java interop, return a corresponding Result type (Template, Redirect, Suspend etc.) instead
     */
    @Deprecated
    public void renderJSON(String json) {
        Controller.renderJSON(json);
    }

    /**
     * @Deprecated this method exists only for java interop, return a corresponding Result type (Template, Redirect, Suspend etc.) instead
     */
    @Deprecated
    public void renderJSON(Object anyObject) {
        Controller.renderJSON(anyObject);
    }

    /**
     * @Deprecated this method exists only for java interop, return a corresponding Result type (Template, Redirect, Suspend etc.) instead
     */
    @Deprecated
    public void notModified() {
        Controller.notModified();
    }

    /**
     * @Deprecated this method exists only for java interop, return a corresponding Result type (Template, Redirect, Suspend etc.) instead
     */
    @Deprecated
    public void todo() {
        Controller.todo();
    }

    /**
     * @Deprecated this method exists only for java interop, return a corresponding Result type (Template, Redirect, Suspend etc.) instead
     */
    @Deprecated
    public void redirectToStatic(String file) {
        Controller.redirectToStatic(file);
    }

    /**
     * @Deprecated this method exists only for java interop, return a corresponding Result type (Template, Redirect, Suspend etc.) instead
     */
    @Deprecated
    public void redirect(String url) {
        Controller.redirect(url);
    }

    /**
     * @Deprecated this method exists only for java interop, return a corresponding Result type (Template, Redirect, Suspend etc.) instead
     */
    @Deprecated
    public void redirect(String url, boolean permanent) {
        Controller.redirect(url, permanent);
    }

    /**
     * @Deprecated this method exists only for java interop, return a corresponding Result type (Template, Redirect, Suspend etc.) instead
     */
    @Deprecated
    public void redirect(String action, Object... args) {
       Controller.redirect(action, args);
    }

    /**
     * @Deprecated this method exists only for java interop, return a corresponding Result type (Template, Redirect, Suspend etc.) instead
     */
    @Deprecated
    public void redirect(String action, boolean permanent, Object... args) {
        Controller.redirect(action, permanent, args);
    }

    /**
     * @Deprecated this method exists only for java interop, return a corresponding Result type (Template, Redirect, Suspend etc.) instead
     */
    @Deprecated
    public void unauthorized(String realm) {
        Controller.unauthorized(realm);
    }

    /**
     * @Deprecated this method exists only for java interop, return a corresponding Result type (Template, Redirect, Suspend etc.) instead
     */
    @Deprecated
    public void unauthorized() {
        Controller.unauthorized("");
    }

    /**
     * @Deprecated this method exists only for java interop, return a corresponding Result type (Template, Redirect, Suspend etc.) instead
     */
    @Deprecated
    public void notFound(String what) {
        Controller.notFound(what);
    }

    /**
     * @Deprecated this method exists only for java interop, return a corresponding Result type (Template, Redirect, Suspend etc.) instead
     */
    @Deprecated
    public void notFound() {
        Controller.notFound("");
    }

    /**
     * @Deprecated this method exists only for java interop, return a corresponding Result type (Template, Redirect, Suspend etc.) instead
     */
    @Deprecated
    public void notFoundIfNull(Object o) {
        Controller.notFoundIfNull(o);
    }

    /**
     * @Deprecated this method exists only for java interop, return a corresponding Result type (Template, Redirect, Suspend etc.) instead
     */
    @Deprecated
    public void ok() {
        Controller.ok();
    }

    /**
     * @Deprecated this method exists only for java interop, return a corresponding Result type (Template, Redirect, Suspend etc.) instead
     */    
    @Deprecated
    public void error(String reason) {
       Controller.error(reason);	 
    }

    /**
     * @Deprecated this method exists only for java interop, return a corresponding Result type (Template, Redirect, Suspend etc.) instead
     */
    @Deprecated
    public void error(Exception reason) {
       Controller.error(reason);	 
    }

    /**
     * @Deprecated this method exists only for java interop, return a corresponding Result type (Template, Redirect, Suspend etc.) instead
     */
    @Deprecated
    public void error() {
       Controller.error();	 
    }

    /**
     * @Deprecated this method exists only for java interop, return a corresponding Result type (Template, Redirect, Suspend etc.) instead
     */
    @Deprecated
    public void error(int status, String reason) {
       Controller.error(status,reason);	 
    }

    /**
     * @Deprecated this method exists only for java interop, return a corresponding Result type (Template, Redirect, Suspend etc.) instead
     */
    @Deprecated
    public void renderBinary(InputStream stream) {
        Controller.renderBinary(stream);
    }

    /**
     * @Deprecated this method exists only for java interop, return a corresponding Result type (Template, Redirect, Suspend etc.) instead
     */
    @Deprecated
    public void renderBinary(File file) {
        Controller.renderBinary(file);
    }

    /**
     * @Deprecated this method exists only for java interop, return a corresponding Result type (Template, Redirect, Suspend etc.) instead
     */    
    @Deprecated
    public void renderBinary(InputStream stream, String name) {
        Controller.renderBinary(stream, name);
    }

    /**
     * @Deprecated this method exists only for java interop, return a corresponding Result type (Template, Redirect, Suspend etc.) instead
     */
    @Deprecated
    public void forbidden() {
        Controller.forbidden();
    }

    /**
     * @Deprecated this method exists only for java interop, return a corresponding Result type (Template, Redirect, Suspend etc.) instead
     */
    @Deprecated
    public void waitFor(Future task) {
        Controller.waitFor(task);
    }
    
    
    /**
     * @Deprecated this method exists only for java interop, return a corresponding Result type (Template, Redirect, Suspend etc.) instead
     */
    @Deprecated
    public void suspend(int millis) {
        Controller.suspend(millis);
    }

    /**
     * @Deprecated this method exists only for java interop, return a corresponding Result type (Template, Redirect, Suspend etc.) instead
     */
    @Deprecated
    public void suspend(String timeout) {
        Controller.suspend(timeout);
    }
}
