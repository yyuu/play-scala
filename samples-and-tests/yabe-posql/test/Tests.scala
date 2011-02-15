import play.test._
import play.db.sql._
import play.db.sql.SqlParser._

import models._

import org.scalatest.{FlatSpec,BeforeAndAfterEach}
import org.scalatest.matchers.{ShouldMatchers}

class ModelTests extends UnitFlatSpec with ShouldMatchers with BeforeAndAfterEach {
    
    override def beforeEach() {
        Fixtures.deleteDatabase()
    }
 
    it should "create and retrieve a User" in {
        
       User.create(User(NotAssigned, "bob@gmail.com", "secret", "Bob"))
       
       val bob = User.find("email={email}").on("email" -> "bob@gmail.com").first()
       
       bob should not be (None)
       bob.get.fullname should be ("Bob")
       
    }
    
    it should "connect a User" in {
        
        User.create(User(NotAssigned, "bob@gmail.com", "secret", "Bob"))
        
        User.connect("bob@gmail.com", "secret") should not be (None)
        User.connect("bob@gmail.com", "badpassword") should be (None)
        User.connect("tom@gmail.com", "secret") should be (None)
        
    }
    
    it should "create a Post" in {
        
        User.create(User(Id(1), "bob@gmail.com", "secret", "Bob")).right.get        
        Post.create(Post(NotAssigned, "My first post", "Hello world", 1))
        
        Post.count().single() should be (1)
        
        val posts = Post.find("author_id={id}").on("id" -> 1).as(Post*)
        
        posts.length should be (1)
        
        val firstPost = posts.headOption
        
        firstPost should not be (None)
        firstPost.get.author_id should be (1)
        firstPost.get.title should be ("My first post")
        firstPost.get.content should be ("Hello world")
        
    }
    
    it should "retrieve Posts with author" in {
        
        User.create(User(Id(1), "bob@gmail.com", "secret", "Bob")).right.get        
        Post.create(Post(NotAssigned, "My first post", "Hello world", 1))
        
        val posts = Post.allWithAuthor
        
        posts.length should be (1)
        
        val post~author = posts.head
        
        post.title should be ("My first post")
        author.fullname should be ("Bob")
    }
    
    it should "support Comments" in {
        
        User.create(User(Id(1), "bob@gmail.com", "secret", "Bob")).right.get        
        Post.create(Post(Id(1), "My first post", "Hello world", 1))
        Comment.create(Comment(NotAssigned, "Jeff", "Nice post", 1))
        Comment.create(Comment(NotAssigned, "Tom", "I knew that !", 1))
        
        User.count().single() should be (1)
        Post.count().single() should be (1)
        Comment.count().single() should be (2)
        
        val Some(post~author~comments) = Post.byIdWithAuthorAndComments(1)
        
        post.title should be ("My first post")
        author.fullname should be ("Bob")
        comments.length should be (2)
        comments(0).author should be ("Jeff")
        comments(1).author should be ("Tom")
        
    }
    
    it should "works with cascade delete" in {
        
        User.create(User(Id(1), "bob@gmail.com", "secret", "Bob")).right.get        
        Post.create(Post(Id(1), "My first post", "Hello world", 1))
        Comment.create(Comment(NotAssigned, "Jeff", "Nice post", 1))
        Comment.create(Comment(NotAssigned, "Tom", "I knew that !", 1))
        
        User.count().single() should be (1)
        Post.count().single() should be (1)
        Comment.count().single() should be (2)
        
        Post.delete("id={id}").on("id" -> 1).executeUpdate()
        
        User.count().single() should be (1)
        Post.count().single() should be (0)
        Comment.count().single() should be (0)
        
    }
    
    it should "load a complex graph from Yaml" in {
        
        Yaml[List[Any]]("data.yml").foreach { 
            _ match {
                case u:User => User.create(u)
                case p:Post => Post.create(p)
                case c:Comment => Comment.create(c)
            }
        }
        
        User.count().single() should be (2)
        Post.count().single() should be (3)
        Comment.count().single() should be (3)
        
        User.connect("bob@gmail.com", "secret") should not be (None)
        User.connect("jeff@gmail.com", "secret") should not be (None)
        User.connect("jeff@gmail.com", "badpassword") should be (None)
        User.connect("tom@gmail.com", "secret") should be (None)
        
        val allPostsWithAuthorAndComments = SQL("select * from Post p join User u on p.author_id = u.id left join Comment c on c.post_id = p.id")
            .as( Magic.group(by=User, Magic.group(by=Post, Comment))* )
        
        allPostsWithAuthorAndComments.length should be (2) 
        
        val (bob, bobPosts) = allPostsWithAuthorAndComments(0)
        bob.fullname should be ("Bob")
        bobPosts.length should be (2)
        
        val (firstBobPost, firstBobPostComments) = bobPosts(0)
        
        firstBobPost.title should be("About the model layer")
        firstBobPostComments.length should be (2)
        firstBobPostComments(1).author should be ("Mike")
        
        val (secondBobPost, secondBobPostComments) = bobPosts(1)
        
        secondBobPost.title should be("Just a test of YABE")
        secondBobPostComments.length should be (1)
        
        val (jeff, jeffPosts) = allPostsWithAuthorAndComments(1)
        jeff.fullname should be ("Jeff")
        jeffPosts.length should be (1)
        
        val (firstJeffPost, firstJeffPostComments) = jeffPosts(0)
        
        firstJeffPost.title should be("The MVC application")
        firstJeffPostComments.length should be (0)
        
        User.delete("email={email}").on("email" -> "bob@gmail.com").executeUpdate().isLeft should be (true)
        
        Post.delete("author_id={id}").on("id" -> bob.id()).executeUpdate().isRight should be (true)
        User.delete("email={email}").on("email" -> "bob@gmail.com").executeUpdate().isRight should be (true)

        User.count().single() should be (1)
        Post.count().single() should be (1)
        Comment.count().single() should be (0)

    }
   
}
