import play.test._

import org.junit._
import org.scalatest.junit._
import org.scalatest._
import org.scalatest.matchers._

import play.db.sql._
import SqlRowsParser._


case class Task(id:String)
object Task extends MagicParser[Task]
class SqlTests extends UnitTestCase with ShouldMatchersForJUnit {
  def meta(items:(String,(Boolean,Class[_]))*)=MetaData(items.toList.map(i=>MetaDataItem(i._1,i._2._1,i._2._2.getName)))
  @Test def useTheMagicParser { 
      val metaData=meta("Task.Id"->(true,classOf[String]),
                        "Task.Name"->(false,classOf[String]))

      val in= StreamReader(Stream.range(1,100).map(i => MockRow(List(i.toString, "nameb"),metaData)))

      commit(eatRow(str("id")))* (in) should be (Error(ColumnNotFound("id").toString,in))
      eatRow(str("id"))+ (in) should be (Failure(ColumnNotFound("id").toString,in))
      
      (eatRow(str("id")))* (in) should be (Success(List(),in))

      eatRow(str("Task.Id"))+ (in) should be (Failure(UnexpectedNullableFound("Task.Id").toString,in))

      eatRow(Task())* (in) should be(Success(List(),in))

      eatRow(Task())+ (in) should be(
        Failure(UnexpectedNullableFound("Task.Id").toString,in))

      commit(eatRow(Task())+) (in) should be(
        Error(UnexpectedNullableFound("Task.Id").toString,in))

      val metaData1=meta("Task.Id"->(false,classOf[String]),
                        "Task.Name"->(false,classOf[String]))
      val in1= StreamReader(Stream.range(1,100).map(i => MockRow(List(i.toString, "nameb"),metaData1)))

      commit(eatRow(Task()) +)(in1).get should be(
        List.range(1,100).map(i=>Task(i.toString)))
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

    val groupByPerson=group(by=int("Person.Id"),eatRow(str("Comment.Text")))* ;
    
    groupByPerson(StreamReader(in)).get should be (
      List.fill(99)(List.range(1,100).map("comment"+_)))

   // Task() >> {t=> group(by=Task.Id,Comment!) ^^ t.copy(comments=_)} *
   // Task() ~ group(by=Task,Comment()) ^^ {case (t,cs) => t.copy(comments=cs)} *
   // group(by=Task,Comment) ^^ {case (t,cs) => t.copy(comments=cs)} *

  }
  
}
