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
  @Test def useTheSqlAPI { 
   val metaData=MetaData(List(MetaDataItem( "id",false,"java.lang.String"),
                            MetaDataItem( "name",false,"java.lang.String")))

    val in= Stream.range(1,100).map(i => MockRow(List(i.toString, "nameb"),metaData))
   
    (wholeRow(Task()) *)(StreamReader(in)).get should be  (List.range(1,100).map(i=>Task(i.toString)))
  }

  
}
