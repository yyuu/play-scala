package models

import java.util.{Date}

import play.db.anorm._
import play.db.anorm.SqlParser._

// User

case class User(id: Pk[Long], email: String, password: String, fullname: String, isAdmin: Boolean)

object User extends Magic[User] {
    
    def connect(email: String, password: String) = {
        User.find("email = {email} and password = {password}")
            .on("email" -> email, "password" -> password)
            .first()
    }
    
}

// Post

case class Post(id: Pk[Long], title: String, content: String, postedAt: Date, author_id: Long) {
    
    def previous = {
        Post.find("postedAt < {date} order by postedAt desc").on("date" -> postedAt).first()
    }
    
    def next = {
        Post.find("postedAt > {date} order by postedAt asc").on("date" -> postedAt).first()
    }
    
}

object Post extends Magic[Post] {
    
    private val postWithAuthor = Post ~< User
    private val postWithAuthorAndComments = postWithAuthor ~< Post.spanM( Comment )
    
    def allWithAuthor = 
        SQL(
            """
                select * from Post p 
                join User u on p.author_id = u.id 
                order by p.postedAt desc
            """
        ).as( postWithAuthor ^^ flatten * )
    
    def allWithAuthorAndComments = 
        SQL(
            """
                select * from Post p 
                join User u on p.author_id = u.id 
                left join Comment c on c.post_id = p.id 
                order by p.postedAt desc
            """
        ).as( postWithAuthorAndComments ^^ flatten * )
    
    def byIdWithAuthorAndComments(id: Long) = 
        SQL(
            """
                select * from Post p 
                join User u on p.author_id = u.id 
                left join Comment c on c.post_id = p.id 
                where p.id = {id}
            """
        ).on("id" -> id).as( postWithAuthorAndComments ^^ flatten ? )
    
}

// Comment

case class Comment(id: Pk[Long], author: String, content: String, postedAt: Date, post_id: Long) 

object Comment extends Magic[Comment] {
    
    def apply(post_id: Long, author: String, content: String) = {
        new Comment(NotAssigned, author, content, new Date(), post_id)
    }
    
}


