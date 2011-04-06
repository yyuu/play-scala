package play.modules.scala.crud

import play.utils._
import play.mvc._
import play.scalasupport._
import play.classloading.enhancers.ControllersEnhancer.ByPass

/**
 * This trait wraps around the java based CRUD module
 */
trait CRUDWrapper[T] {
    
    @Before def addType = play.utils.Java.invokeStatic("controllers.CRUD", "addType")    
    
    def index = ActionProxy.delegate("controllers.CRUD", "index")    
    def list(page: Int, search: String, searchFields: String, orderBy: String, order: String) = ActionProxy.delegate("controllers.CRUD", "list", page, search, searchFields, orderBy, order)
    def blank = ActionProxy.delegate("controllers.CRUD", "blank")
    def save(id: String) = ActionProxy.delegate("controllers.CRUD", "save", id)
    def create = ActionProxy.delegate("controllers.CRUD", "create")
    def delete(id: String) = ActionProxy.delegate("controllers.CRUD", "delete", id)
    def show(id: String) = ActionProxy.delegate("controllers.CRUD", "show", id)
    def attachment(id: String, field: String) = ActionProxy.delegate("controllers.CRUD", "attachment", id, field)
    
}

