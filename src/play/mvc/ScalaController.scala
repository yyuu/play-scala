package play.mvc

import scala.xml.NodeSeq
import scala.io.Source

import java.io.InputStream
import java.util.concurrent.Future

import play.mvc.Http._
import play.mvc.Scope._
import play.data.validation.Validation
import play.classloading.enhancers.LocalvariablesNamesEnhancer.LocalVariablesSupport
import play.classloading.enhancers.ControllersEnhancer.ControllerSupport

/**
* Represents a Scala based Controller
*/
abstract class ScalaController extends ControllerDelegate with LocalVariablesSupport with ControllerSupport {
    def request = Request.current()
    def response = Response.current()
    def session = Session.current()
    def flash = Flash.current()
    def params = Params.current()
    def renderArgs = RenderArgs.current()
    def validation = Validation.current()
    def renderXml(node: NodeSeq) { renderXml(node.toString) }
    def renderHtml(node: NodeSeq) { throw new results.RenderHtml(node.toString, "application/xhtml+xml") }
    def renderHtml(content: String) { throw new results.RenderHtml(content) }
    def render(args: Any*) { ControllerDelegate.render(args.map(_.asInstanceOf[AnyRef]): _*) }
}