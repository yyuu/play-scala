package controllers

import play._
import play.mvc._

import models._

import play.db.sql._
import play.db.sql.SqlParser._

object Application extends Controller {
    
    def index = {
        User.create(User(NotAssigned, "bob@gmail.com", "secret", "Bob", false))
        "KIKIXX"
    }

}
