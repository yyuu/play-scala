package play.mvc;

import java.io.InputStream;
import play.mvc.Controller;
import java.util.concurrent.Future;

abstract class ControllerDelegate {
    
    public void render(Object... args) {
        Controller.render(args);
    }
    
    public void renderTemplate(String template, Object... args) {
        Controller.renderTemplate(template,args);
    }

    public void renderText(Object text) {
        Controller.renderText(text);
    }

    public void renderText(CharSequence pattern, Object... args) {
        Controller.renderText(pattern, args);
    }

    public void renderXml(String xml) {
        Controller.renderXml(xml);
    }

    public void renderJSON(String json) {
        Controller.renderJSON(json);
    }
    
    public void redirect(String url) {
        Controller.redirect(url);
    }

    public void redirectToStatic(String file) {
        Controller.redirectToStatic(file);
    }

    public void redirect(String url, boolean permanent) {
        Controller.redirect(url, permanent);
    }

    public void redirect(String action, Object... args) {
       Controller.redirect(action, args);
    }

    public void redirect(String action, boolean permanent, Object... args) {
        Controller.redirect(action, permanent, args);
    }

    public void unauthorized(String realm) {
        Controller.unauthorized(realm);
    }

    public void unauthorized() {
        Controller.unauthorized("");
    }

    public void notFound(String what) {
        Controller.notFound(what);
    }

    public void notFound() {
        Controller.notFound("");
    }

    public void notFoundIfNull(Object o) {
        Controller.notFoundIfNull(o);
    }

    public void ok() {
        Controller.ok();
    }

    public void renderBinary(InputStream stream) {
        Controller.renderBinary(stream);
    }
    
    public void renderBinary(InputStream stream, String name) {
        Controller.renderBinary(stream, name);
    }

    public void forbidden() {
        Controller.forbidden();
    }

		public void waitFor(Future task) {
			 	Controller.waitFor(task);
		}

}
