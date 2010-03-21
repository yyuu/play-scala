package play.db.jpa

import javax.persistence.GeneratedValue;
import javax.persistence.Id
import javax.persistence.MappedSuperclass
import java.lang.annotation.Annotation
import play.data.validation.Validation
import play.mvc.Scope.Params

@MappedSuperclass
class ScalaModel extends JPABase {
    
  def em() = JPA.em()

  def refresh(): this.type = {
    em().refresh(this) 
    this
  }
  
  def merge(): this.type = {
    em().merge(this)
    this
  }
  
  def save(): this.type = {
    _save() 
    this
  }

  def delete(): this.type = {
    _delete() 
    this
  }
  

  def edit(name: String, params: java.util.Map[String,Array[String]]): this.type = {
     JPASupport.edit(this, name, params, Array[Annotation]())
     this
  } 

  def validateAndSave(): Boolean = {
    if (Validation.current().valid(this).ok) {
        _save()
        true
    } else false
  }

    @Id
    @GeneratedValue
    var id:Long=_

    def getId():Long = id

}
