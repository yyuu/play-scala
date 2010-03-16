package play.db.jpa

class RichModel(m:Model) {
  def saveInplace() {m.save().asInstanceOf[AnyRef]}
  def editInplace(name:String, params:play.mvc.Scope.Params) {m.edit(name,params).asInstanceOf[AnyRef]}
  def deleteInplace() {m.delete().asInstanceOf[AnyRef]}
}
