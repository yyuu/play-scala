package play.db.jpa

class RichModel(m:Model) {
  def saveThis() {m.save().asInstanceOf[AnyRef]}
  def editThis(name:String, params:play.mvc.Scope.Params) {m.edit(name,params).asInstanceOf[AnyRef]}
  def deleteThis() {m.delete().asInstanceOf[AnyRef]}
}
