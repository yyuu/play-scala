package models

import java.util.{Date}

import play.db.sql._

// User

case class User(id: Pk[Long], email: String, password: String, fullname: String)

object User extends Magic[User] {
    
    def connect(email: String, password: String) = {
        SQL("select * from User where email = {email} and password = {password}")
            .on("email" -> email, "password" -> password)
            .as(User?)
    }
    
}

// Post

case class Post(id: Pk[Long], title: String, content: String, author_id: Long)

object Post extends Magic[Post] {
    
    private val postWithAuthor = Post ~< User
    
    def allWithAuthor = SQL("select * from Post p join User u on p.author_id = u.id").as(postWithAuthor*) 
    
    def byIdWithAuthorAndComments(id: Long) = {
        SQL("select * from Post p join User u on p.author_id = u.id left join Comment c on c.post_id = p.id where p.id = {id}")
            .on("id" -> id)
            .as((postWithAuthor ~< (Comment*))?)
    }
    
}

// Comment

case class Comment(id: Pk[Long], author: String, content: String, post_id: Long)

object Comment extends Magic[Comment]


