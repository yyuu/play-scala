package controllers

import play._
import play.mvc._

object Application extends Controller {
    
  def index() { 
    var user= new models.User("sdfd","dsfdf","dsfdf")
    user.save()
    renderHtml(<h1>{user.email}</h1>)
}
}

