package controllers

import play._
import play.mvc._
import play.data.validation._

import java.util._

import models._

import play.db.sql._
import play.db.sql.Sql._

object Application extends Controller {
    
    def index = {
        Template("now" -> new Date)    
    }
    
    def list = {
        Template("contacts" -> Contact.find("order by name, firstname ASC"))
    }
       
    def form(id: Long) = {
        Template("contact" -> Contact.findById(id))
    }
    
    def save(id: Long, @Valid contact: Contact) = {
        if(Validation.hasErrors()) {
            "@form".asTemplate("contact" -> Entity(id, contact) )
        } else {
            Contact.update(Entity(id, contact))        
            Action(list)
        }        
    }
    
    def create(@Valid contact: Contact) = {
        if(Validation.hasErrors()) {
            "@form".asTemplate("contact" -> Entity(null,contact) )
        } else {
            val newContact = Contact.create(contact)
            Action(form(newContact.id))
        }
    }
    
    def delete(id: Long) = {
        Contact.delete(id)
        Action(list)
    }
    
}

case class GroovyWrapper(val value: Any) extends groovy.lang.GroovyObjectSupport {
    
    override def getProperty(property: String) = {
        val proxy = new groovy.util.Proxy()
        proxy.wrap(value)
        proxy.getProperty(property)
    }
    
}
