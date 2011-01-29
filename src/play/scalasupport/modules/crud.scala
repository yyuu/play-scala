package play.scalasupport.crud

import play.utils._
import play.mvc._
import play.scalasupport._
import play.classloading.enhancers.ControllersEnhancer.ByPass

/**
 * This trait wraps around the java based CRUD module
 */
trait CRUDWrapper[T] {
    
    @Before def addType = play.utils.Java.invokeStatic("controllers.CRUD", "addType")    
    
    def index = ActionProxy.deleguate("controllers.CRUD", "index")    
    def list(page: Int, search: String, searchFields: String, orderBy: String, order: String) = ActionProxy.deleguate("controllers.CRUD", "list", page, search, searchFields, orderBy, order)
    def blank = ActionProxy.deleguate("controllers.CRUD", "blank")
    def save(id: String) = ActionProxy.deleguate("controllers.CRUD", "save", id)
    def create = ActionProxy.deleguate("controllers.CRUD", "create")
    def delete(id: String) = ActionProxy.deleguate("controllers.CRUD", "delete", id)
    def show(id: String) = ActionProxy.deleguate("controllers.CRUD", "show", id)
    def attachment(id: String, field: String) = ActionProxy.deleguate("controllers.CRUD", "attachment", id, field)
    
}

