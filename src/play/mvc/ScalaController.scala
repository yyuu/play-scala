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
 *
 * Represents a Scala based Controller
 */
abstract class ScalaController extends ControllerDelegate with LocalVariablesSupport with ControllerSupport {

  /**
   * @returns a play request object
   */
  def request = Request.current()

  /**
   * @returns a play response object
   */
  def response = Response.current()

  /**
   * @returns a session object
   */
  def session = Session.current()

  /**
   * @returns a flash object
   */
  def flash = Flash.current()

  /**
   * @returns parameters
   */
  def params = Params.current()

  /**
   * @returns render argument object
   */
  def renderArgs = RenderArgs.current()

  /**
   * @returns Validation
   */
  def validation = Validation.current()

  /**
   * renders an xml node as xml
   * @param node xml node to be rendered
   */
  def renderXml(node: NodeSeq) {renderXml(node.toString)}

  /**
   * renders an xml node as XHTML
   * @param xml node to be rendered
   */
  def renderHtml(node: NodeSeq) {throw new results.RenderHtml(node.toString, "application/xhtml+xml")}

  /**
   * renders content in html
   * @param content Html to be rendered
   */
  def renderHtml(content: String) {throw new results.RenderHtml(content)}

  /**
   * renders content using the underlying templating language
   * @param args
   */
  def render(args: Any*) {ControllerDelegate.render(args.map(_.asInstanceOf[AnyRef]): _*)}
}