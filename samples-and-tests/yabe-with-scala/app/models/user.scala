package models
 
import java.util._
 
import play.db.jpa._
import play.db.jpa.Annotations._
import play.data.validation.Annotations._

@Entity
class User(

    @Email
    @Required
    var email: String,
    
    @Required
    var password: String,
    
    var fullname: String

) extends Model {
    
    var isAdmin = false
    
    override def toString() = email
 
}

object Users extends QueryOn[User] {
    
    def connect(email: String, password: String) = {
        find("byEmailAndPassword", email, password).first
    }
    
}
