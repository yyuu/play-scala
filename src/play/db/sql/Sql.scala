package play.db.sql
/*
 * sample usage:
 *  val sql=Sql("select * from tasks where id={id}") on "id"->1
 *  val tasks:Stream[Task]=sql.result() collect {
 *    case Row(Some(i:Int),Some(name:String))=> Task(i,name)
 *  }
 *
 *
 *  val in=Sql("select * from tasks where id={id}") on "id"->1 result
 *  ( Task() <~ newLine * )(StreamReader(in))
 *
 *  //having
 *  case class Task(id: Option[Int], name: Option[String]) 
 *  object Task extends MagicParser[Task]
 * 
 */

import play.utils.Scala.MayErr
import play.utils.Scala.MayErr._
import java.util.Date
abstract class SqlRequestError
case class ColumnNotFound(columnName:String) extends SqlRequestError
case class TypeDoesNotMatch(message:String) extends SqlRequestError
case class UnexpectedNullableFound(on:String) extends SqlRequestError

trait ColumnTo[A]{
 def transform(row:Row,columnName:String):MayErr[SqlRequestError,A]
}
case class StreamReader[T](s: Stream[T]) extends scala.util.parsing.input.Reader[Either[EndOfStream,T]]{
  def first = s.headOption.toRight(EndOfStream())
  def rest = new StreamReader(s.drop(1))
  def pos = scala.util.parsing.input.NoPosition
  def atEnd = s.isEmpty
}

case class EndOfStream
object SqlRowsParser extends scala.util.parsing.combinator.Parsers{
  import Row._
  type Elem=Either[EndOfStream,Row]
  import scala.collection.generic.CanBuildFrom
  import scala.collection.mutable.Builder
  implicit def extendParser[A](a:Parser[A]):ExtendedParser[A]= ExtendedParser(a)
 
  case class ExtendedParser[A](p:Parser[A]){
    // a combinator that keeps first parser from consuming input
    def ~<[B](b:Parser[B]):Parser[A ~ B]= guard(p) ~ b
  }
  def sequence[A](ps:Traversable[Parser[A]])
                 (implicit bf:CanBuildFrom[Traversable[_], A, Traversable[A]]) =
        Parser[Traversable[A]]{ in => 
           ps.foldLeft(success(bf(ps)))((s,p) =>
            for( ss <- s; pp <- p) yield ss += pp) map (_.result) apply in 
                             }
  implicit def rowFunctionToParser[T](f:(Row => MayErr[SqlRequestError,T])):Parser[T]=
   eatRow( Parser[T]{in=>
      in.first.left.map(_=>Failure("End of Stream",in))
                   .flatMap(f(_).left.map(e=>Failure(e.toString,in)))
                   .fold(e=>e, a=> {Success(a,in)}) })
  implicit def rowParserToFunction[T](p:RowParser[T]):(Row => MayErr[SqlRequestError,T])=p.f

  case class RowParser[A](f: (Row=>MayErr[SqlRequestError,A])) extends Parser[A]{
    lazy val parser=rowFunctionToParser(f)
    def apply(in:Input)=parser(in)
  }
                  
  def str(columnName:String):RowParser[String]=get[String](columnName)(implicitly[ColumnTo[String]])
  def int(columnName:String):(Row => MayErr[SqlRequestError,Int])=get[Int](columnName)(implicitly[ColumnTo[Int]])
  def get[T](columnName:String)(implicit extractor:ColumnTo[T]):RowParser[T] =
    RowParser( extractor.transform(_,columnName))

  def current[T](columnName:String)(implicit extractor:ColumnTo[T]): RowParser[T]=
   RowParser( extractor.transform(_,columnName))

  def wholeRow[T](p:Parser[T])=p <~ newLine
  def eatRow[T](p:Parser[T])=p <~ newLine
  def current1[T](columnName:String)(implicit extractor:ColumnTo[T]): Parser[T]=
   commit(current[T](columnName)(extractor))
  
  def newLine:Parser[Unit]=  Parser[Unit]{
    in => if(in.atEnd) Failure("end",in) else Success(Unit,in.rest) 
  }

  def group[A](by:(Row=> MayErr[SqlRequestError,Any]),a:Parser[A]):Parser[Seq[A]]={
    val d=guard(by)
    d >> (first => Parser[Seq[A]] {in =>
      {val (groupy,rest)=in.asInstanceOf[StreamReader[Row]]
                           .s.span(by(_).right.toOption.exists(r=>r==first));
       val g=(a *)(StreamReader(groupy))
       g match{
         case Success(a,_)=> Success(a,StreamReader(rest))
         case Failure(msg,_) => Failure(msg,in)
         case Error(msg,_) => Error(msg,in) }
       }})}
}

object Magic{
import  SqlRowsParser._
  def group[B <: {val id:Any},D](by: Parser[B],p:Parser[D])(implicit m:ClassManifest[B])={
      val name=(m.erasure.getSimpleName.toUpperCase()+".ID")
      by ~< SqlRowsParser.group(by=(row=>Right(row.ColumnsDictionary.get(name).orNull)),p) ^^
      {case c ~ p => (c,p)}
  }
}
case class Magic[T](implicit m:ClassManifest[T]) extends SqlRowsParser.Parser[T]{
  import java.lang.reflect._
  import scala.reflect.Manifest
  def manifestFor(t: Type): Manifest[AnyRef] = t match {
    case c: Class[_] => Manifest.classType[AnyRef](c)
    case p: ParameterizedType =>
      Manifest.classType[AnyRef](
        p.getRawType.asInstanceOf[Class[AnyRef]],
        manifestFor(p.getActualTypeArguments.head),
        p.getActualTypeArguments.tail.map(manifestFor): _*)
  }
  import SqlRowsParser._
  import Sql._
  
  def clean(fieldName:String)=fieldName.split('$').last
  val name=clean(m.erasure.getSimpleName)

   def findById(id:Any):Option[T] =
    sql("select * from "+name+" where Id={id}")
          .on("id"->id)
          .as[Option[T]](phrase(this*).map(_.headOption))

  def all():Seq[T] =
    sql("select * from " + name)
        .asSimple
        .as[Seq[T]](this*)
        
  def find(stmt:String):Seq[T]=
     sql(stmt match {
         case s if s.startsWith("select") => s
         case s if s.startsWith("where") => "select * from " + name + " " + s
         case s if s.startsWith("order by") => "select * from " + name + " " + s
         case s => "select * from " + name + " where " + s
     }).asSimple.as[Seq[T]](this*)
 
  val electConstructorAndGetInfo:(Constructor[_],Seq[(String,ColumnTo[_])])={
      
      def supportesTypes[C](m:ClassManifest[C]):Option[ColumnTo[C]]= 
      (m.erasure match {
        case c if c==classOf[String] => Some(Row.rowToString)
        case c if c==classOf[Int] => Some(Row.rowToInt)
        case c if c==classOf[Long] => Some(Row.rowToLong)
        case c if c==classOf[Date] => Some(Row.rowToDate)
       //TODO for Option you need to call recursively, this makes it applicable to future extensions (Option[List[Int]])
        case c if c==classOf[Option[_]] =>{
           val typeParam=m.typeArguments.headOption.collect { case m:ClassManifest[_] => m}
               .getOrElse(implicitly[ClassManifest[Any]])
           supportesTypes(typeParam).flatMap(_=> Some(Row.rowToOption1(typeParam)):Option[_])}
        case _ => None
      }).asInstanceOf[Option[ColumnTo[C]]]
    def isConstructorSupported(c:Constructor[_])={
      c.getGenericParameterTypes().forall(t=>supportesTypes(manifestFor(t)).isDefined)
    }

    def getParametersNames(c:Constructor[_]):Seq[String]={
      import scala.collection.JavaConversions._
      play.classloading.enhancers.LocalvariablesNamesEnhancer.lookupParameterNames(c)
    }
    val consInfo= 
        m.erasure
          .getConstructors()
          .sortBy(- _.getGenericParameterTypes().length)
          .find(isConstructorSupported)
          .map(c=>(c,c.getGenericParameterTypes().map(manifestFor),getParametersNames(c)))
          .getOrElse(throw new java.lang.Error("no supported constructors for type " +m))

    val coherent=consInfo._2.length==consInfo._3.length
    val names_types= consInfo._3.zip(consInfo._2.map(t=>supportesTypes(t).get))
                             .map(nt=>(name.toUpperCase()+"."+clean(nt._1.toUpperCase()),nt._2))
    if(!coherent && names_types.map(_._1).exists(_.contains("outer")))
      throw new java.lang.Error("It seems that your class uses a closure to an outer instance. For MagicParser, please use only top level classes.")
    if(!coherent) throw new java.lang.Error("not coherent to me!")
    (consInfo._1,names_types)
  }

    def apply(input:Input):ParseResult[T]={
    val name=clean(m.erasure.getName)   
    val (c,names_types)=electConstructorAndGetInfo
    val paramParser=eatRow(sequence(names_types.map(i => 
                       guard[Any](current(i._1)(i._2)))))

    (paramParser ^^ {case args => 
                      {c.newInstance( args.toSeq.map(_.asInstanceOf[Object]):_*)
                           .asInstanceOf[T] } }) (input)
  }

}
object Row{
  def unapplySeq(row:Row):Option[List[Any]]={
    Some(row.data.zip(row.metaData.ms.map(_.nullable)).map(i=> if(i._2) Option(i._1) else i._1))
  }
  
  implicit def rowToString :ColumnTo[String]= 
   new ColumnTo[String]{
     def transform(row:Row,columnName:String) = row.get1[String](columnName,false) 
   }
  implicit def rowToInt :ColumnTo[Int]= 
   new ColumnTo[Int]{
     def transform(row:Row,columnName:String) = row.get1[Int](columnName,false) 
   }
   implicit def rowToLong :ColumnTo[Long]= 
    new ColumnTo[Long]{
      def transform(row:Row,columnName:String) = row.get1[Long](columnName,false) 
    } 
   implicit def rowToDate :ColumnTo[Date]= 
    new ColumnTo[Date]{
      def transform(row:Row,columnName:String) = row.get1[Date](columnName,false)
    }  
   
  //TODO better to require an implicit of ColumnTo[T] can be useful for extensiblity
  implicit def rowToOption1[T](implicit m:ClassManifest[T]) :ColumnTo[Option[T]]= 
   new ColumnTo[Option[T]]{
     def transform(row:Row,columnName:String) = {
       for(meta <- row.metaData.dictionary.get(columnName).toRight(ColumnNotFound(columnName));
            val (nullable,_)=meta;
           result <- (if(!nullable)   Left(UnexpectedNullableFound(columnName)):MayErr[SqlRequestError,T]
                      else  row.get1[T](columnName,true)(m))) yield Option(result)
     }  
   }
}

case class MetaDataItem(column:String,nullable:Boolean,clazz:String)
case class MetaData(ms:List[MetaDataItem]){
  lazy val dictionary= ms.map(m => (m.column,(m.nullable,m.clazz))).toMap
}

trait Row{
 val metaData:MetaData
  import scala.reflect.Manifest  
  val data:List[Any]
  private[sql] lazy val ColumnsDictionary:Map[String,Any]=metaData.ms.map(_.column).zip(data).toMap
  def get[A](a:String)(implicit c:ColumnTo[A]):MayErr[SqlRequestError,A]=
    c.transform(this,a)
    
  private def getType(t:String) = t match {
      case "long" => Class.forName("java.lang.Long")
      case "int" => Class.forName("java.lang.Int")
      case "boolean" => Class.forName("java.lang.Boolean")
      case _ => Class.forName(t)
  }

  private[sql] def get1[B](a:String,nullableAlreadyHandled:Boolean)(implicit m : ClassManifest[B]):MayErr[SqlRequestError,B]=
   {for(  meta <- metaData.dictionary.get(a).toRight(ColumnNotFound(a));
          val (nullable,clazz)=meta;
          val requiredDataType =
            if(m.erasure==classOf[Option[_]]) 
              m.typeArguments.headOption.collect { case m:ClassManifest[_] => m.erasure}
               .getOrElse(classOf[Any]).getName
            else m.erasure.getName;
          v <- ColumnsDictionary.get(a).toRight(ColumnNotFound(a));
          result <- v match {//case b: AnyRef if(nullable != (m.erasure == classOf[Option[_]])) =>  Left(UnexpectedNullableFound(a))
                             case b if(nullable && !nullableAlreadyHandled ) =>  Left(UnexpectedNullableFound(a))
                             case b if(getType(requiredDataType).isAssignableFrom(getType(clazz))) => Right(b.asInstanceOf[B])
                             case b => Left(TypeDoesNotMatch(requiredDataType + " - " + clazz))}) yield result
  }
  def apply[B](a:String)(implicit c:ColumnTo[B]):B=get[B](a)(c).get
}

case class MockRow(data: List[Any],metaData:MetaData) extends Row

class SqlRow(rs:java.sql.ResultSet) extends Row{
  import java.sql._
  import java.sql.ResultSetMetaData._

  val meta = rs.getMetaData()
  val nbColumns = meta.getColumnCount()
  val metaData = MetaData(List.range(1,nbColumns+1).map(i=>MetaDataItem( (meta.getTableName(i) + "." + meta.getColumnName(i)).toUpperCase, meta.isNullable(i)==columnNullable,meta.getColumnClassName(i))))
  val data:List[Any] = List.range(1,nbColumns+1).map(nb =>rs.getObject(nb))
  
  override def toString() = "Row(" + ( Range.inclusive(1, nbColumns).map( i => "'" + (meta.getTableName(i) + "." + meta.getColumnName(i)).toUpperCase + "':" + rs.getObject(i) + " as " + meta.getColumnClassName(i) ).reduceLeft(_ + ", " + _) ) + ")"
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
case class SimpleSql(sql:SqlQuery,params:Seq[(String,Any)]) extends Sql{
  def on(args:(String,Any)*):SimpleSql = 
    this.copy(params=(this.params) ++ args)
  def onParams(args:Any*):SimpleSql = 
    this.copy(params=(this.params) ++ sql.argsInitialOrder.zip(args))

  def getFilledStatement(connection:java.sql.Connection)={
    val s=connection.prepareStatement(sql.query)
    val argsMap=Map(params:_*)
    sql.argsInitialOrder.map(argsMap)
               .zipWithIndex
               .map(_.swap)
               .foldLeft(s)((s,e)=>{s.setObject(e._1+1,e._2);s})
  }
}
case class BatchSql(sql:SqlQuery,params:Seq[Seq[(String,Any)]]) extends Sql{

  def addBatch(args:(String,Any)*):BatchSql = 
    this.copy(params=(this.params) :+ args)

  def addBatchParams(args:Any*):BatchSql =
     this.copy(params=(this.params) :+ sql.argsInitialOrder.zip(args))

  def getFilledStatement(connection:java.sql.Connection)={
    val statement=connection.prepareStatement(sql.query)
    params.foldLeft(statement)((s,ps)=>{
      s.addBatch()
      val argsMap=Map(ps:_*)
      sql.argsInitialOrder
         .map(argsMap)
         .zipWithIndex
         .map(_.swap)
         .foldLeft(s)((s,e)=>
           {s.setObject(e._1+1,e._2);s})})
  }
}
trait Sql{
  def connection=play.db.DB.getConnection
  def getFilledStatement(connection:java.sql.Connection):java.sql.PreparedStatement
  def filledStatement=getFilledStatement(connection)
  def apply(conn:java.sql.Connection=connection) = result(connection)
  def result(conn:java.sql.Connection=connection) =
    Sql.resultSetToStream(getFilledStatement(connection).executeQuery())
  import SqlRowsParser._
  def as[T](parser:Parser[T])(implicit conn:java.sql.Connection=connection):T =
    parser(StreamReader(result(connection))) match{
            case Success(a,_)=>a
            case Failure(e,_)  => error(e)
            case Error(e,_) => error(e) }
  def execute(conn:java.sql.Connection=connection):Boolean =
    getFilledStatement(connection).execute()
  def executeUpdate(conn:java.sql.Connection=connection):Int =
    getFilledStatement(connection).executeUpdate()
} 

case class SqlQuery(query:String,argsInitialOrder:List[String]=List.empty) extends Sql{
  def getFilledStatement(connection:java.sql.Connection):java.sql.PreparedStatement =
    asSimple.getFilledStatement(connection)
  def asSimple:SimpleSql=SimpleSql(this,Nil)
  def asBatch:BatchSql=BatchSql(this,Nil)  
}
object Sql{
  implicit def sqlToSimple(sql:SqlQuery):SimpleSql=sql.asSimple
  implicit def sqlToBatch(sql:SqlQuery):BatchSql=sql.asBatch

  import SqlParser._
  def sql(inSql:String):SqlQuery={val (sql,paramsNames)= parse(inSql);SqlQuery(sql,paramsNames)}
  import java.sql._
  import java.sql.ResultSetMetaData._
  def resultSetToStream(rs:java.sql.ResultSet):Stream[SqlRow]={
    Useful.unfold(rs)(rs => if(!rs.next()) {rs.close();None} else Some ((new SqlRow(rs),rs)))
  }
}
