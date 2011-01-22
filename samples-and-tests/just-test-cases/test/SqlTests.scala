import play.test._

import org.junit._
import org.scalatest.junit._
import org.scalatest._
import org.scalatest.matchers._

import play.db.sql._
import SqlRowsParser._ 

// Constructors with unspported type of parameter won't be picked
case class Task(ids:Option[List[Int]],comment:String){
  def this(ids:Option[List[Int]])=this(ids,"no comment.")
  def this(comment:String)=this(None,comment)
}
import Row._
object Task extends Magic[Task]
object TaskE extends MEntity[String,Task] 
case class Student(id:String)
object Student extends Magic[Student]

class SqlTests extends UnitTestCase with ShouldMatchersForJUnit {
  def meta(items:(String,(Boolean,Class[_]))*)=MetaData(items.toList.map(i=>MetaDataItem(i._1,i._2._1,i._2._2.getName)))

  @Test def useTheMagicParser { 
      val metaData=meta("TASK.COMMENT"->(true,classOf[String]),
                        "TASK.NAME"->(false,classOf[String]))
   
      val in= StreamReader(Stream.range(1,100).map(i => MockRow(List("comment no:"+i, "nameb"),metaData)))
      commit((str("COMMENT1")))* (in) should be (Error(ColumnNotFound("COMMENT1").toString,in))

      (str("COMMENT1"))+ (in) should be (Failure(ColumnNotFound("COMMENT1").toString,in))
      (str("COMMENT"))* (in) should be (Success(List(),in))

      str("TASK.COMMENT")+ (in) should be (
        Failure(UnexpectedNullableFound("TASK.COMMENT").toString,in))

      str("COMMENT")+ (in) should be (
        Failure(UnexpectedNullableFound("TASK.COMMENT").toString,in))

          import Magic._
      (Task)* (in) should be(Success(List(),in))
      (Task)+ (in) should be(
        Failure(UnexpectedNullableFound("TASK.COMMENT").toString,in))

      commit((Task)+) (in) should be(
        Error(UnexpectedNullableFound("TASK.COMMENT").toString,in))

      val metaData1=meta("TASK.COMMENT"->(false,classOf[String]),
                        "TASK.NAME"->(false,classOf[String]))
      val in1= StreamReader(Stream.range(1,100).map(i => MockRow(List("comment no:"+i, "nameb"),metaData1)))

      import Row._
     commit((Task) +)(in1).get should be(
        List.range(1,100).map(i=>new Task("comment no:"+i)))
  }
  @Test def testNullables {
    import play.db.sql.Row._
     val metaData=meta("PERSON.ID"->(true,classOf[java.lang.Integer]))
     val in= StreamReader(Stream.range(1,100).map(i=>MockRow(List( if(i % 2 ==0) i else null),metaData)))
     println(commit((get[Option[Int]]("PERSON.ID")) +)(in) )
  } 
  @Test def useSqlParserForGroupBys {
    val metaData=meta("PERSON.ID"->(false,classOf[Int]),
                      "PERSON.NAME"->(false,classOf[String]),
                      "PERSON.AGE"->(true,classOf[Int]),
                      "COMMENT.ID"->(false,classOf[Int]),
                      "COMMENT.TEXT"->(false,classOf[String]))
   
    val in= Stream.range(1,100)
                  .flatMap(i => 
                    Stream.range(1,100)
                           .map(j =>
                             MockRow(List(i, "person"+i, 13, j, "comment"+j),metaData)))
    // manual groupBy
   
    val groupByPerson=group(by=int("PERSON.ID"),(str("COMMENT.TEXT")))* ;
    groupByPerson(StreamReader(in)).get should be (
      List.fill(99)(List.range(1,100).map("comment"+_)))
    
    // "magical" groupBy
    import Magic._
    val parsePeople=group(by=Person,Comment) ^^ 
                      {case (p,cs) => p.copy(comments=cs) } *;

    ( parsePeople (StreamReader(in)) get ) should be (
      List.range(1,100).map(
        i=> Person(i, "person"+i, Seq.range(1,100).map(
              j=> Comment(j,"comment"+j) ))))

  }
  
  @Test def useSomeMagicSql{
    
     play.db.DB.execute("DROP TABLE IF EXISTS Task")
      play.db.DB.execute("DROP TABLE IF EXISTS Student") 
     play.db.DB.execute("""CREATE TABLE Task 
                           (Id char(60) NOT NULL,
                            Comment char(60) NOT NULL) """)
         play.db.DB.execute("""CREATE TABLE Student 
                           (Id char(60) NOT NULL,
                            Task_Id char(60) NOT NULL) """)

    play.db.DB.execute("""insert into Task Values('1','some comment')""")
play.db.DB.execute("""insert into Student Values('1','1')""")
    TaskE.get("1") should be (Some(Entity("1",new Task("some comment"))))
    Task.all() should be (List(new Task("some comment")))
    TaskE.all() should be (List(Entity("1",new Task("some comment"))))
    TaskE.findById("2") should be (None)
    play.db.sql.Sql.sql("select * from Task join Student on Task.id=Student.Task_Id").as(Task ~< Student) should be (SqlRowsParser.~(new Task("some comment"),Student("1")))
  }
  
  @Test def testAlternate(){
    play.db.DB.execute("DROP TABLE IF EXISTS Post")

         play.db.DB.execute("""CREATE TABLE Post 
                           (Id char(60) NOT NULL,
                            Type char(60) NOT NULL,
                            Title char(60) NOT NULL,
                            URL char(200)  DEFAULT 'non' NOT Null,
                            Body char(360) DEFAULT 'non' NOT Null) """)
      
    play.db.DB.execute("""insert into Post Values('1','Link','zengularity','http://www.zengularity.com','non')""")
    play.db.DB.execute("""insert into Post Values('1','Text','Functional Web','non','It rocks!')""")
    play.db.DB.execute("""insert into Post Values('1','Text','Functional Web','non','It rocks!')""")
    import Row._
     play.db.sql.Sql.sql("select * from Post")
              .as( 'TYPE.is("Text") ~> Text |
                   'TYPE.is("Link") ~> Link +) should be (
                     List(Link("1","zengularity","http://www.zengularity.com"),
                          Text("1","Functional Web","It rocks!"),
                          Text("1","Functional Web","It rocks!")))

  }
  @Test def insertAnEntity(){
    play.db.DB.execute("DROP TABLE IF EXISTS User2")
      
    play.db.DB.execute("""CREATE TABLE User2 
                       (Id INTEGER GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY ,
                       Address char(60) NOT NULL,
                       Name char(60) NOT NULL) """)
    User2.create(User2("Paul","Address")) should be (Entity(1,User2("Paul","Address")))
  }

}

case class User2(name:String,address:String)
import Row._
object User2 extends MEntity[Int,User2]


abstract class Post

  case class Link(id:String,title:String,url:String) extends Post
  object Link extends Magic[Link](Some("POST"))
  case class Text(id:String,title:String,body:String) extends Post
  object Text extends Magic[Text](Some("POST"))
case class Person(id: Int,name:String,comments:Seq[Comment]) {
  def this(id:Int,name:String)=this(id,name,List())
}
object Person extends Magic[Person]
case class Comment(id: Int,text:String) 
object Comment extends Magic[Comment] 
