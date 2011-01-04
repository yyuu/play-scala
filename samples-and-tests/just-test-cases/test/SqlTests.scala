import play.test._

import org.junit._
import org.scalatest.junit._
import org.scalatest._
import org.scalatest.matchers._

import play.db.sql._
import SqlRowsParser._ 

// Constructors with unspported type of parameter won't be picked
case class Task(id:String,ids:Option[List[Int]]){
  def this(ids:Option[List[Int]])=this("1",ids)
  def this(id:String)=this(id,None)
}
object Task extends Magic[Task] 
case class Student(id:String)
object Student extends Magic[Student]

class SqlTests extends UnitTestCase with ShouldMatchersForJUnit {
  def meta(items:(String,(Boolean,Class[_]))*)=MetaData(items.toList.map(i=>MetaDataItem(i._1,i._2._1,i._2._2.getName)))

  @Test def useTheMagicParser { 
      val metaData=meta("TASK.ID"->(true,classOf[String]),
                        "TASK.NAME"->(false,classOf[String]))
   
      val in= StreamReader(Stream.range(1,100).map(i => MockRow(List(i.toString, "nameb"),metaData)))
      commit((str("ID1")))* (in) should be (Error(ColumnNotFound("ID1").toString,in))

      (str("ID1"))+ (in) should be (Failure(ColumnNotFound("ID1").toString,in))
      (str("ID"))* (in) should be (Success(List(),in))

      str("TASK.ID")+ (in) should be (
        Failure(UnexpectedNullableFound("TASK.ID").toString,in))

      str("ID")+ (in) should be (
        Failure(UnexpectedNullableFound("TASK.ID").toString,in))

          import Magic._
      (Task)* (in) should be(Success(List(),in))
      (Task)+ (in) should be(
        Failure(UnexpectedNullableFound("TASK.ID").toString,in))

      commit((Task)+) (in) should be(
        Error(UnexpectedNullableFound("TASK.ID").toString,in))

      val metaData1=meta("TASK.ID"->(false,classOf[String]),
                        "TASK.NAME"->(false,classOf[String]))
      val in1= StreamReader(Stream.range(1,100).map(i => MockRow(List(i.toString, "nameb"),metaData1)))

      import Row._
     commit((Task) +)(in1).get should be(
        List.range(1,100).map(i=>new Task(i.toString)))
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
                           (Id char(60) NOT NULL) """)
         play.db.DB.execute("""CREATE TABLE Student 
                           (Id char(60) NOT NULL,
                            Task_Id char(60) NOT NULL) """)

    play.db.DB.execute("""insert into Task Values('1')""")
play.db.DB.execute("""insert into Student Values('1','1')""")
    Task.findById("1") should be (Some(new Task("1")))
    Task.all() should be (List(new Task("1")))
    Task.findById("2") should be (None)
    play.db.sql.Sql.sql("select * from Task join Student on Task.id=Student.Task_Id").as(Task ~< Student) should be (SqlRowsParser.~(new Task("1"),Student("1")))
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

}
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
