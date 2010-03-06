package play.mvc;

import scala.xml.NodeSeq
import scala.io.Source

import java.io.InputStream
import java.util.concurrent.Future

import play.mvc.Http._
import play.mvc.Scope._
import play.data.validation.Validation
import play.classloading.enhancers.LocalvariablesNamesEnhancer.LocalVariablesSupport
import play.classloading.enhancers.ControllersEnhancer.ControllerSupport

abstract class ScalaController extends ControllerDelegate with LocalVariablesSupport with ControllerSupport {

    def request = Request.current()
    def response = Response.current()
    def session = Session.current()
    def flash = Flash.current()
    def params = Params.current()
    def renderArgs = RenderArgs.current()
    def validation = Validation.current()
    def renderXml(node: NodeSeq): Unit = renderXml(node.toString)

}

class RichRenderArgs(val renderArgs: RenderArgs) {

    def +=(variable: Tuple2[String, Any]) {
        renderArgs.put(variable._1, variable._2)
    }

}

class RichSession(val session: Session) {

    def apply(key: String) = session.get(key)

}

class RichResponse(val response: Response) {

    val ContentTypeRE = """[-a-zA-Z]+/[-a-zA-Z]+""".r

    def <<<(x: String) {
        x match {
            case ContentTypeRE() => response.contentType = x
            case _ => response.print(x)
        }
    }

    def <<<(header: Header) {
        response.setHeader(header.name, header.value())
    }

    def <<<(header: Tuple2[String, String]) {
        response.setHeader(header._1, header._2)
    }

    def <<<(status: Int) {
        response.status = status
    }

    def <<<(xml: scala.xml.NodeSeq) {
        response.print(xml)
    }

}

private[play] object ActionProxy {
    
    def deleguate(controller: String, action: String, args: Any*) {
        val m = play.utils.Java.findActionMethod(action, play.Play.classloader.loadClass(controller))
        try {
            m.invoke(null, args.map(_.asInstanceOf[AnyRef]): _*)
        } catch {
            case t: java.lang.reflect.InvocationTargetException => throw t.getTargetException
        }
    }
}


