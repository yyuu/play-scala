package controllers

import play._
import play.mvc._
import play.libs._
import play.cache._
import play.data.validation.Annotations._

import models._

trait Defaults {
    self:Controller =>
    
    @Before def setDefaults {
        renderArgs += "blogTitle" -> configuration("blog.title")
        renderArgs += "blogBaseline" -> configuration("blog.baseline") 
    }
    
}

object Application extends Controller with Defaults {
    
    def index = {
        val allPosts = Post.allWithAuthorAndComments
        Template('frontPost -> allPosts.headOption, 'olderPosts -> allPosts.drop(1))
    }
    
    def show(id: Long) = {
        Template('post -> Post.byIdWithAuthorAndComments(id), 'randomID -> Codec.UUID)
    }
    
    def captcha(id: String) = {
        val captcha = Images.captcha
        val code = captcha.getText("#E4EAFD")
        Cache.set(id, code, "10mn")
        captcha
    }
    
    def postComment(
        postId: Long, 
        @Required(message="Author is required") author: String,
        @Required(message="A message is required") content: String,
        @Required(message="Please type the code") code: String,
        randomID: String
    ) = {
        validation.equals(code, Cache.get(randomID).orNull).message("Invalid code. Please type it again")
        if (validation.hasErrors) {
            Template("Application/show.html", 'post -> Post.byIdWithAuthorAndComments(postId), 'randomID -> randomID)
        } else {
            Cache.delete(randomID)
            Comment.create( Comment(postId, author, content) )
            flash.success("Thanks for posting %s", author)
            Action( show(postId) )
        }
    }

}
