import play.mvc.Scope._
import play.mvc.Http._
import play.mvc.results._

/**
* stuffing a few scala specific helpers into play.mvc namespace
* 
*/
package play {
import mvc.{ScalaController, ScalaMailer, RichSession, RichResponse, RichRenderArgs}

package object mvc {


        // -- HELPERS

        def header(name: String, value: String) = new Header(name, value)
        def header(h: Tuple2[String, String]) = new Header(h._1, h._2)
        
        // -- STATUS CODE

        val OK              = 200
        val CREATED         = 201
        val ACCEPTED        = 202
        val NO_CONTENT      = 204
        val FORBIDDEN       = 403
        val NOT_FOUND       = 404
        val ERROR           = 500

        // -- Responses

        def Ok                                          = new Ok()
        def Created                                     = new Status(201)
        def Accepted                                    = new Status(202)
        def NoContent                                   = new Status(204)
        def NotModified                                 = new NotModified()
        def NotModified(etag: String)                   = new NotModified(etag)
        def Forbidden                                   = new Forbidden("Forbidden")
        def Forbidden(why: String)                      = new Forbidden(why)
        def NotFound                                    = new NotFound("Not found")
        def NotFound(why: String)                       = new NotFound(why)
        def NotFound(method: String, path: String)      = new NotFound(method, path)
        def Error                                       = new Error("Internal server error")
        def Error(why: String)                          = new Error(why)
        def Error(status: Int, why: String)             = new Error(status, why)
        def BadRequest                                  = new BadRequest()
        def Unauthorized                                = new Unauthorized("Secure")
        def Todo                                        = new NotFound("This action has not been implemented Yet")
        def Unauthorized(area: String)                  = new Unauthorized(area)
        def Html(html: Any)                             = new RenderHtml( if(html != null) html.toString else "" )
        def Xml(document: org.w3c.dom.Document)         = new RenderXml(document)
        def Xml(xml: Any)                               = new RenderXml( if(xml != null) xml.toString else "<empty/>" )
        def Json(json: String)                          = new RenderJson(json)
        def Json(o: Any)                                = new RenderJson(new com.google.gson.Gson().toJson(o))
        def Text(content: Any)                          = new RenderText(if(content != null) content.toString else "")
        def Redirect(url: String)                       = new Redirect(url)
        def Redirect(url: String, permanent: Boolean)   = new Redirect(url, permanent)
        def Template                                    = new ScalaRenderTemplate()
        def Template(args: Any*)                        = new ScalaRenderTemplate(args =  ScalaController.argsToParams(args: _*))
        def Action(action: => Any)                      = new ScalaAction(action)
        def Continue                                    = new NoResult()

        // -- Shortcuts
        def @@(action: => Any)                          = Action(action)
        def ^                                           = new ScalaRenderTemplate()
        def ^(args: Any*)                               = new ScalaRenderTemplate(args = ScalaController.argsToParams(args: _*))
        
        // -- TYPES REDEFINITION

        type Controller = ScalaController
        type Mailer = ScalaMailer

    }

}
