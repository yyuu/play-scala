package controllers

import play._
import play.mvc._

import models._

object Application extends Controller {
    
    def index = {
        val allPosts = Post.allWithAuthorAndComments
        Template("frontPost" -> allPosts.headOption, "olderPosts" -> allPosts.drop(1))
    }

}
