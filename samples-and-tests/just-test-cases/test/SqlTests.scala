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
  @Test def useTheMagicParser { 
    val metaData=MetaData(List(MetaDataItem( "id",false,"java.lang.String"),
                            MetaDataItem( "name",false,"java.lang.String")))

    val in= Stream.range(1,100).map(i => MockRow(List(i.toString, "nameb"),metaData))
   
    (eatRow(Task()) *)(StreamReader(in)).get should be  (List.range(1,100).map(i=>Task(i.toString)))
  }
  @Test def useSqlParserForGroupBys {
    val metaData=MetaData(List(MetaDataItem( "Person.Id",false,classOf[Int].getName),
                               MetaDataItem( "Person.Name",false,classOf[String].getName),
                               MetaDataItem( "Person.Age",true,classOf[Int].getName),
                               MetaDataItem( "Comment.Id",false,classOf[Int].getName),
                               MetaDataItem( "Comment.Text",false,classOf[String].getName)))
   
    val in= Stream.range(1,100).flatMap(i => Stream.range(1,100).map(j => MockRow(List(i, "person"+i,13,j,"comment"+j),metaData)))

    val groupByPerson=group(by=int("Person.Id"),eatRow(str("Comment.Text")))* ;
    groupByPerson(StreamReader(in)).get should be (List.fill(99)(List.range(1,100).map("comment"+_)))

  } 

  
}
