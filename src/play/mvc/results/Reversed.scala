package play.mvc.results

trait Reversed {
  def reverse(action: => Any): play.mvc.Router.ActionDefinition = {
      val actionDefinition = ScalaResultHelper._reverse()
      action
      actionDefinition
  }
}
