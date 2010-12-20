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
object Task extends MagicParser[Task] 

class SqlTests extends UnitTestCase with ShouldMatchersForJUnit {
  def meta(items:(String,(Boolean,Class[_]))*)=MetaData(items.toList.map(i=>MetaDataItem(i._1,i._2._1,i._2._2.getName)))
  @Test def useTheMagicParser { 
      val metaData=meta("Task.Id"->(true,classOf[String]),
                        "Task.Name"->(false,classOf[String]))
   
      val in= StreamReader(Stream.range(1,100).map(i => MockRow(List(i.toString, "nameb"),metaData)))

      commit((str("id")))* (in) should be (Error(ColumnNotFound("id").toString,in))

      (str("id"))+ (in) should be (Failure(ColumnNotFound("id").toString,in))
      (str("id"))* (in) should be (Success(List(),in))

      str("Task.Id")+ (in) should be (
        Failure(UnexpectedNullableFound("Task.Id").toString,in))

          import Magic._
      (Task)* (in) should be(Success(List(),in))

      (Task)+ (in) should be(
        Failure(UnexpectedNullableFound("Task.Id").toString,in))

      commit((Task)+) (in) should be(
        Error(UnexpectedNullableFound("Task.Id").toString,in))

      val metaData1=meta("Task.Id"->(false,classOf[String]),
                        "Task.Name"->(false,classOf[String]))
      val in1= StreamReader(Stream.range(1,100).map(i => MockRow(List(i.toString, "nameb"),metaData1)))

      import Row._
     commit((Task) +)(in1).get should be(
        List.range(1,100).map(i=>new Task(i.toString)))
  }
  @Test def testNullables {
    import play.db.sql.Row._
     val metaData=meta("Person.Id"->(true,classOf[Int]))
     val in= StreamReader(Stream.range(1,100).map(i=>MockRow(List( if(i % 2 ==0) i else null),metaData)))
     println(commit((get[Option[Int]]("Person.Id")) +)(in) )
  } 
  @Test def useSqlParserForGroupBys {
    val metaData=meta("Person.Id"->(false,classOf[Int]),
                      "Person.Name"->(false,classOf[String]),
                      "Person.Age"->(true,classOf[Int]),
                      "Comment.Id"->(false,classOf[Int]),
                      "Comment.Text"->(false,classOf[String]))
   
    val in= Stream.range(1,100)
                  .flatMap(i => 
                    Stream.range(1,100)
                           .map(j =>
                             MockRow(List(i, "person"+i, 13, j, "comment"+j),metaData)))
    // manual groupBy
    val groupByPerson=group(by=int("Person.Id"),(str("Comment.Text")))* ;
    
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
     play.db.DB.execute("""CREATE TABLE Task 
                           (Id char(60) NOT NULL) """)
    

    play.db.DB.execute("""insert into Task Values('1')""")
    Task.find("1") should be (Some(new Task("1")))
    Task.findAll() should be (List(new Task("1")))
    Task.find("2") should be (None)
  }
}
case class Person(id: Int,name:String,comments:Seq[Comment]) {
  def this(id:Int,name:String)=this(id,name,List())
}
object Person extends MagicParser[Person]
case class Comment(id: Int,text:String) 
object Comment extends MagicParser[Comment] 
