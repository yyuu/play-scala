import org.junit._
import java.util._
import play.test._
import models._
 
class BasicTest extends UnitTest {
    
    @Before
    def setup() {
        Fixtures.deleteAll()
    }
 
    @Test
    def createAndRetrieveUser() {
        // Create a new user and save it
        new User("bob@gmail.com", "secret", "Bob").save()
	
        // Retrieve the user with bob username
        var bob = User.find("byEmail", "bob@gmail.com").first

        // Test 
        assertNotNull(bob)
        assertEquals("Bob", bob.fullname)
    }
    
    @Test
    def tryConnectAsUser() {
        // Create a new user and save it
        new User("bob@gmail.com", "secret", "Bob").save()

        // Test 
        assertNotNull(User.connect("bob@gmail.com", "secret"))
        assertNull(User.connect("bob@gmail.com", "badpassword"))
        assertNull(User.connect("tom@gmail.com", "secret"))
    }
    
    @Test
    def createPost() {
        // Create a new user and save it
        var bob = new User("bob@gmail.com", "secret", "Bob").save()

        // Create a new post
        new Post(bob, "My first post", "Hello world").save()

        // Test that the post has been created
        assertEquals(1, Post.count())

        // Retrieve all post created by bob
        var bobPosts = Post.find("byAuthor", bob).fetch

        // Tests
        assertEquals(1, bobPosts.size())
        var firstPost = bobPosts.get(0)
        assertNotNull(firstPost)
        assertEquals(bob, firstPost.author)
        assertEquals("My first post", firstPost.title)
        assertEquals("Hello world", firstPost.content)
        assertNotNull(firstPost.postedAt)
    }
    
    @Test
    def postComments() {
        // Create a new user and save it
        var bob = new User("bob@gmail.com", "secret", "Bob").save()

        // Create a new post
        var bobPost = new Post(bob, "My first post", "Hello world").save()

        // Post a first comment
        new Comment(bobPost, "Jeff", "Nice post").save()
        new Comment(bobPost, "Tom", "I knew that !").save()

        // Retrieve all comments
        var bobPostComments = Comment.find("byPost", bobPost).fetch()

        // Tests
        assertEquals(2, bobPostComments.size())

        var firstComment = bobPostComments.get(0)
        assertNotNull(firstComment)
        assertEquals("Jeff", firstComment.author)
        assertEquals("Nice post", firstComment.content)
        assertNotNull(firstComment.postedAt)

        var secondComment = bobPostComments.get(1)
        assertNotNull(secondComment)
        assertEquals("Tom", secondComment.author)
        assertEquals("I knew that !", secondComment.content)
        assertNotNull(secondComment.postedAt)
    }
    
    @Test
    def useTheCommentsRelation() {
        // Create a new user and save it
        var bob = new User("bob@gmail.com", "secret", "Bob").save()

        // Create a new post
        var bobPost = new Post(bob, "My first post", "Hello world").save()

        // Post a first comment
        bobPost.addComment("Jeff", "Nice post")
        bobPost.addComment("Tom", "I knew that !")

        // Count things
        assertEquals(1, User.count())
        assertEquals(1, Post.count())
        assertEquals(2, Comment.count())

        // Retrieve the bob post
        bobPost = Post.find("byAuthor", bob).first
        assertNotNull(bobPost)

        // Navigate to comments
        assertEquals(2, bobPost.comments.size())
        assertEquals("Jeff", bobPost.comments.get(0).author)

        // Delete the post
        bobPost.delete()

        // Chech the all comments have been deleted
        assertEquals(1, User.count())
        assertEquals(0, Post.count())
        assertEquals(0, Comment.count())
    }
    
    @Test
    def fullTest() {
        Fixtures.load("data.yml")

        // Count things
        assertEquals(2, User.count())
        assertEquals(3, Post.count())
        assertEquals(3, Comment.count())

        // Try to connect as users
        assertNotNull(User.connect("bob@gmail.com", "secret"))
        assertNotNull(User.connect("jeff@gmail.com", "secret"))
        assertNull(User.connect("jeff@gmail.com", "badpassword"))
        assertNull(User.connect("tom@gmail.com", "secret"))

        // Find all bob posts
        var bobPosts = Post.find("author.email", "bob@gmail.com").fetch
        assertEquals(2, bobPosts.size())

        // Find all comments related to bob posts
        var bobComments = Comment.find("post.author.email", "bob@gmail.com").fetch
        assertEquals(3, bobComments.size())

        // Find the most recent post
        var frontPost = Post.find("order by postedAt desc").first
        assertNotNull(frontPost)
        assertEquals("About the model layer", frontPost.title)

        // Check that this post has two comments
        assertEquals(2, frontPost.comments.size())

        // Post a new comment
        frontPost.addComment("Jim", "Hello guys")
        assertEquals(3, frontPost.comments.size())
        assertEquals(4, Comment.count())
    }
    
    @Test
    def testTags() {
        // Create a new user and save it
        var bob = new User("bob@gmail.com", "secret", "Bob").save()

        // Create a new post
        var bobPost = new Post(bob, "My first post", "Hello world").save()
        var anotherBobPost = new Post(bob, "My second post post", "Hello world").save()
        
        // Well
        assertEquals(0, Post.findTaggedWith("Red").size())
        
        // Tag it now
        bobPost.tagItWith("Red").tagItWith("Blue").save()
        anotherBobPost.tagItWith("Red").tagItWith("Green").save()
        
        // Check
        assertEquals(2, Post.findTaggedWith("Red").size())        
        assertEquals(1, Post.findTaggedWith("Blue").size())
        assertEquals(1, Post.findTaggedWith("Green").size())
        
        assertEquals(1, Post.findTaggedWith("Red", "Blue").size())   
        assertEquals(1, Post.findTaggedWith("Red", "Green").size())   
        assertEquals(0, Post.findTaggedWith("Red", "Green", "Blue").size())  
        assertEquals(0, Post.findTaggedWith("Green", "Blue").size())    
        
        var cloud = Tag.getCloud()
        assertEquals("[{tag=Red, pound=2}, {tag=Blue, pound=1}, {tag=Green, pound=1}]", cloud.toString())
        
    }
 
}
