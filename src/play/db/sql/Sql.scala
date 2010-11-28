package play.db.sql
/*
 * sample usage:
 *  val sql=Sql("select * from tasks where id={id}") on "id"->1
 *  val tasks:Stream[Task]=sql.result() collect {
 *    case Row(Some(i:Int),Some(name:String))=> Task(i,name)
 *  }
 */
object Row{
 def unapplySeq(row:Row):Option[List[Any]]={
    Some(row.tuples.map(_._2))
  }
}
import play.utils.Scala.MayErr
import play.utils.Scala.MayErr._
abstract class SqlRequestError
case class ColumnNotFound(columnName:String) extends SqlRequestError
case class TypeDoesNotMatch(message:String) extends SqlRequestError

trait ColumnTo[A]{
 def transform(row:Row,columnName:String):MayErr[SqlRequestError,A]
}
object GetSimple {
 
 implicit def rowToSimple[A](implicit m:ClassManifest[A]) :ColumnTo[A]= 
   new ColumnTo[A]{
     def transform(row:Row,columnName:String) = row.get[A](columnName)(m) 
}
}

// A parser combinator for the Sql result
case class StreamReader[T](s: Stream[T]) extends scala.util.parsing.input.Reader[T]{
  def first = s.head
  def rest = new StreamReader(s.drop(1))
  def pos = scala.util.parsing.input.NoPosition
  def atEnd = s.tail.isEmpty
}
object SqlRowsParser extends scala.util.parsing.combinator.Parsers{
  type Elem=Row
  import GetSimple.rowToSimple
  import scala.collection.generic.CanBuildFrom
  import scala.collection.mutable.Builder
  def sequence[A](ps:Traversable[Parser[A]])
                 (implicit bf:CanBuildFrom[Traversable[_], A, Traversable[A]]) =
        Parser[Traversable[A]]{ in =>       
          ps.foldLeft(success(bf(ps)))((s,p) =>
            for( ss <- s; pp <- p) yield ss += pp) map (_.result) apply in 
        }

  def current[T](columnName:String)(implicit extractor:ColumnTo[T]): Parser[T]=
   Parser[T]{in =>
      extractor.transform(in.first,columnName)
               .fold(e=>Failure(e.toString,in), a=>Success(a,in))}   

  def wholeRow[T](p:Parser[T])=p <~ newLine
  def current1[T](columnName:String)(implicit extractor:ColumnTo[T]): Parser[T]=
   commit(current[T](columnName)(extractor))
  
  def newLine:Parser[Unit]=  Parser[Unit]{
    in => if(in.atEnd) Failure("end",in) else Success(Unit,in.rest) 
  }
  def distingwishList[A](differentiator:Parser[Any],a:Parser[A]):Parser[Seq[A]]={
    val d=guard(differentiator)
    d >> 
      (first => ((d ^? {case curr if curr == first => curr }) ~> a) +)
   }
}
trait MagicParser[T]{
import SqlRowsParser._
  def apply()(implicit m : ClassManifest[T]):Parser[T]={
    val cons= m.erasure.getConstructors()(0)
    val zipped= cons.getParameterTypes()
                    .zip(m.erasure.getDeclaredFields()).toList
    def clean(fieldName:String)=fieldName.split('$').last
    // maybe I need to check in declared methods too?
    val coherent= zipped.forall(i=> i._1==i._2.getType())
    if(!coherent) throw new java.lang.Error("not coherent to me!")
    val paramParser=sequence(zipped.map(i => 
                       current(clean(i._2.getName))(GetSimple.rowToSimple(
                                               scala.reflect.ClassManifest.fromClass(i._1)))))

    paramParser ^^ {case args => 
                      {cons.newInstance( args.toSeq.map(_.asInstanceOf[Object]):_*)
                           .asInstanceOf[T] } }
  }
}

trait Row{
  import scala.reflect.Manifest  
  val tuples:List[(String,Any)]
  lazy val ColumnsDictionary=tuples.toMap

  def get[B](a:String)(implicit m : ClassManifest[B]):MayErr[SqlRequestError,B]=
    ColumnsDictionary
      .get(a)
      .toRight(ColumnNotFound(a))
      .flatMap({case b:AnyRef 
                  if (m.erasure.isAssignableFrom(b.getClass)) => Right(b.asInstanceOf[B])
                case b:AnyRef => Left(TypeDoesNotMatch(m.erasure.toString + " - " + b.getClass.toString))})
  def apply[B](a:String)(implicit m : ClassManifest[B]):B=get[B](a)(m).get
}
case class MockRow(tuples: List[(String,Any)]) extends Row 
class SqlRow(rs:java.sql.ResultSet) extends Row{
  import java.sql._
  import java.sql.ResultSetMetaData._

  val tuples:List[(String,Any)]={
    val meta=rs.getMetaData()
    val nbColumns= meta.getColumnCount()
    List.range(1,nbColumns+1).map(nb =>{
           val (key,value)=(meta.getColumnName(nb),rs.getObject(nb))
           (key,if(meta.isNullable(nb)==columnNullable)
                 Option(value)
                else value)})
  }
}
object Useful{
    case class Var[T](var content:T)
    def drop[A]( these:Var[Stream[A]],n: Int): Stream[A] = {
      var count = n
      while (!these.content.isEmpty && count > 0) {
        these.content = these.content.tail
        count -= 1
      }
    these.content} 
   def unfold1[T, R](init: T)(f: T => Option[(R, T)]): (Stream[R],T) = f(init) match {
     case None => (Stream.Empty,init)
     case Some((r, v)) => (Stream.cons(r,unfold(v)(f)),v)
   }
   def unfold[T, R](init: T)(f: T => Option[(R, T)]): Stream[R] = f(init) match {
     case None => Stream.Empty
     case Some((r, v)) => Stream.cons(r,unfold(v)(f))
   }
}
case class Sql(sqlQuery:String,argsInitialOrder:List[String],params:Seq[(String,Any)]=List.empty){
  def on(args:(String,Any)*):Sql=this.copy(params=(this.params) ++ args)
  def onParams(args:Any*):Sql=this.copy(params=(this.params) ++ argsInitialOrder.zip(args))
  lazy val filledStatement={getFilledStatement(play.db.DB.getConnection)}
  def getFilledStatement(connection:java.sql.Connection)={
    val s=connection.prepareStatement(sqlQuery)
    val argsMap=Map(params:_*)
    argsInitialOrder.map(argsMap)
               .zipWithIndex
               .map(_.swap)
               .foldLeft(s)((s,e)=>{s.setObject(e._1+1,e._2);s})
  }
  def apply()=result()
  def result()=Sql.resultSetToStream(filledStatement.executeQuery())
}
object Sql{
  import SqlParser._
  def apply(inSql:String):Sql={val (sql,paramsNames)= parse(inSql);Sql(sql,paramsNames)}
  import java.sql._
  import java.sql.ResultSetMetaData._
  def resultSetToStream(rs:java.sql.ResultSet):Stream[SqlRow]={
    Useful.unfold(rs)(rs => if(!rs.next()) {rs.close();None} else Some ((new SqlRow(rs),rs)))
  }
}

