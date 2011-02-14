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
 *  val tasks: List[Task] = in.as( Task * )
 *  val alsoTasks : List[Task] = Task.all()
 *
 *  //having
 *  case class Task(id: Option[Int], name: Option[String]) 
 *  object Task extends Magic[Task]
 * 
 */

import play.utils.Scala.MayErr
import play.utils.Scala.MayErr._
import java.util.Date

abstract class SqlRequestError
case class ColumnNotFound(columnName:String) extends SqlRequestError
case class TypeDoesNotMatch(message:String) extends SqlRequestError
case class UnexpectedNullableFound(on:String) extends SqlRequestError
case object NoColumnsInReturnedResult extends SqlRequestError

trait ColumnTo[A]{
  def transform(row:Row,columnName:String):MayErr[SqlRequestError,A]
}
case class StreamReader[T](s: Stream[T]) extends scala.util.parsing.input.Reader[Either[EndOfStream,T]]{
  def first = s.headOption.toRight(EndOfStream())
  def rest = new StreamReader(s.drop(1))
  def pos = scala.util.parsing.input.NoPosition
  def atEnd = s.isEmpty
}

case class EndOfStream()

object SqlRowsParser extends SqlRowsParser

trait SqlRowsParser extends scala.util.parsing.combinator.PackratParsers{
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

  case class RowParser[A](f: (Row=>MayErr[SqlRequestError,A])) extends Parser[A] {
    lazy val parser=rowFunctionToParser(f)
    def apply(in:Input)=parser(in)
  }
                  
  def str(columnName:String):RowParser[String] =
    get[String](columnName)(implicitly[ColumnTo[String]])

  def int(columnName:String): RowParser[Int] =
    get[Int](columnName)(implicitly[ColumnTo[Int]])

  def get[T](columnName:String)(implicit extractor:ColumnTo[T]):RowParser[T] =
    RowParser( extractor.transform(_,columnName))

  def contains[T](columnName:String,t:T)(implicit extractor:ColumnTo[T]) :Parser[Unit]=
      guard(get[T](columnName)(extractor) ^? {case a if a==t => Unit})

  def current[T](columnName:String)(implicit extractor:ColumnTo[T]): RowParser[T]=
   RowParser( extractor.transform(_,columnName))

  def eatRow[T](p:Parser[T])=p <~ newLine

  def current1[T](columnName:String)(implicit extractor:ColumnTo[T]): Parser[T]=
   commit(current[T](columnName)(extractor))
  
  def newLine:Parser[Unit]=  Parser[Unit]{
    in => if(in.atEnd) Failure("end",in) else Success(Unit,in.rest) 
  }
  def scalar[T](implicit m:Manifest[T])=
    SqlRowsParser.RowParser(row => 
      row.asList
         .headOption.toRight(NoColumnsInReturnedResult)
                    .flatMap(a=>
                             if (m >:>  TypeWrangler.javaType(a.asInstanceOf[AnyRef].getClass))
                               Right(a.asInstanceOf[T]) 
                             else Left(TypeDoesNotMatch(m.erasure + " and "+a.asInstanceOf[AnyRef].getClass)) ))

  def group[A](by:(Row=> MayErr[SqlRequestError,Any]),a:Parser[A]):Parser[Seq[A]] = {
    val d=guard(by)
    d >> (first => Parser[Seq[A]] {in =>
        //instead of cast it'd be much better to override type Reader
      {val (groupy,rest) =in.asInstanceOf[StreamReader[Row]]
                            .s.span(by(_).right.toOption.exists(r=>r==first));
       val g=(a *)(StreamReader(groupy))
       g match{
         case Success(a,_)=> Success(a,StreamReader(rest))
         case Failure(msg,_) => Failure(msg,in)
         case Error(msg,_) => Error(msg,in) }
       }})
  }

  implicit def symbolToColumn(columnName:Symbol):ColumnSymbol=ColumnSymbol(columnName)

  case class ColumnSymbol(name:Symbol){
    def of[T](implicit extractor:ColumnTo[T] ):Parser[T]=
      get[T](name.name)(extractor)
    def is[T](t:T)(implicit extractor:ColumnTo[T] ):Parser[Unit]=
      contains[T](name.name,t)(extractor)
  }
}



object Magic{
  import  SqlRowsParser._
  def group[B <: {val id:Any},D](by: Parser[B],p:Parser[D])(implicit m:ClassManifest[B])={
      val name=(m.erasure.getSimpleName.toUpperCase()+".ID")
      by ~< SqlRowsParser.group(by=(row=>row.get1[Any](name,true)),p) ^^
      {case c ~ p => (c,p)}
  }
}

abstract class Pk[+ID]{
  def isAssigned:Boolean = this match{
    case Id(_) => true
    case TODO => false
  } 

}
case class Id[ID](id:ID) extends Pk[ID]{
    override def toString() = id.toString
}
case object TODO extends Pk[Nothing]{
    override def toString() = ""
}

case class Magic[T]  ( override val tableName:Option[String]=None)(implicit val m:ClassManifest[T])  extends  M[T]  {

    def apply(tableName:Symbol)= this.copy(tableName=Some(tableName.name))

}
trait M[T] extends MParser[T]{
  self =>

    val msql:MSql[T] = new MSql[T] {
      override lazy val analyser = self.analyser
      override val m = self.m
    }

    val idParser:SqlRowsParser.Parser[_]=
    SqlRowsParser.RowParser(row => row.asList.headOption.flatMap(a => (if (a.isInstanceOf[Option[_]]) a else Option(a)).asInstanceOf[Option[_]]).toRight(NoColumnsInReturnedResult))

    import SqlRowsParser._
    import Sql._
    def find(stmt:String=""):SimpleSql[T]= msql.find(stmt).using(self) 
    def count(stmt:String=""):SimpleSql[Long]= msql.count(stmt).using(scalar[Long])
    import scala.util.control.Exception._
    import java.sql.SQLIntegrityConstraintViolationException
    
    def delete(where:String):BatchSql={
      sql("delete from "+analyser.name+" where "+where)
    }

    def update(v:T){
      val names_attributes = analyser.names_methods.map(nm => (nm._1.split('.').last.toLowerCase, nm._2.invoke(v) ))
      val (ids,toSet) = 
          names_attributes.map(na => (na._1, na._2 match {case v:Option[_]=>v.getOrElse(null);case v=>v})).partition(na => na._2.isInstanceOf[Pk[_]])
      if(ids==Nil) throw new Exception("cannot update without Ids, no Ids found on "+analyser.name)
      val toUpdate=toSet.map(_._1).map(n => n+" = "+"{"+n+"}").mkString(", ")
      sql("update "+analyser.name+" set "+toUpdate+" where "+ ids.map(_._1).map( n => n+" = "+"{"+n+"}").mkString(" and "))
      .onParams(toSet.map(_._2) ++ 
                ids.map(_._2).map{ 
                  case Id(id)=>id;
                  case other => throw new Exception("not set ids in the passed object")} : _*)
      .executeUpdate()
    }

    def create(v:T):MayErr[SQLIntegrityConstraintViolationException,T]={
      val names_attributes = analyser.names_methods.map(nm => (nm._1, nm._2.invoke(v) ))
      val (notSetIds,toSet) = 
          names_attributes.map(na => ( na._1,
                                       na._2 match {case Id(id)=>id;case v:Option[_]=>v.getOrElse(null);case v=>v}))
                          .partition(na => na._2 == TODO)
      if(notSetIds.length>1) throw new Exception("multi ids not supported")
      val toInsert = toSet.map(_._1.split('.').last.toLowerCase)
     
      val query=sql("insert into "+analyser.name+" ( "+toInsert.mkString(", ")+" ) values ( "+toInsert.map("{"+_+"}").mkString(", ")+")")
                    .onParams(toSet.map(_._2):_*)
      
      val result = catching(classOf[SQLIntegrityConstraintViolationException])
                    .either(query.execute1())
                    .left.map(_.asInstanceOf[SQLIntegrityConstraintViolationException])

      for{ r <- result;
           val (statement,ok) = r;
           val rs = statement.getGeneratedKeys();
          val id=idParser(StreamReader(Sql.resultSetToStream(rs))).get
           val params = names_attributes.map(_._2).map({case TODO => Id(id); case other => other})
        } yield analyser.c.newInstance(params:_*).asInstanceOf[T]//StreamReader(Sql.resultSetToStream(rs))
    }

  def insert(v:T):MayErr[SQLIntegrityConstraintViolationException,Boolean]={
      val names_attributes = analyser.names_methods.map(nm => (nm._1, nm._2.invoke(v) ))
      val (notSetIds,toSet) = 
          names_attributes.map(na => ( na._1,
                                       na._2 match {case Id(id)=>id;case v:Option[_]=>v.getOrElse(null);case v=>v}))
                          .partition(na => na._2 == TODO)

      val toInsert = toSet.map(_._1.split('.').last.toLowerCase)
     
      val query=sql("insert into "+analyser.name+" ( "+toInsert.mkString(", ")+" ) values ( "+toInsert.map("{"+_+"}").mkString(", ")+")")
                    .onParams(toSet.map(_._2):_*)
      
      catching(classOf[SQLIntegrityConstraintViolationException])
            .either(query.execute())
            .left.map(_.asInstanceOf[SQLIntegrityConstraintViolationException])

  }    
}
case class MagicSql[T] ( override val tableName:Option[String]=None)(implicit val m:ClassManifest[T]) extends  MSql[T] {
  def apply(tableName:Symbol)= this.copy(tableName=Some(tableName.name))

}
trait MSql[T]{
    val m:ClassManifest[T]
    val tableName:Option[String]=None
    lazy val analyser= new Analyse[T](tableName,m)
    import Sql._
    import java.lang.reflect._
     def find(stmt:String=""):SimpleSql[Seq[Row]]=
     sql(stmt match {
         case s if s.startsWith("select") => s
         case s if s.startsWith("where") => "select * from " + analyser.name + " " + s
         case s if s.startsWith("order by") => "select * from " + analyser.name + " " + s
         case "" => "select * from " + analyser.name
         case s => "select * from " + analyser.name + " where " + s
     }).asSimple
   

    def count(stmt:String=""):SimpleSql[Seq[Row]]=sql("select count(*) from "+analyser.name+" "+stmt).asSimple
}

case class MagicParser[T] ( override val tableName:Option[String]=None)(implicit val m:ClassManifest[T]) extends  MParser[T] with Analyser[T]{
  def apply(tableName:Symbol)= this.copy(tableName=Some(tableName.name))
}

trait MParser[T] extends SqlRowsParser.Parser[T]{
    val m:ClassManifest[T]
    val tableName:Option[String]=None
    val analyser= new Analyse[T](tableName,m){
      import java.lang.reflect._
      override def isConstructorSupported(c:Constructor[_]):Boolean =
            c.getGenericParameterTypes().forall(t => getExtractor(manifestFor(t)).isDefined)
    }
    import java.lang.reflect._
    import scala.reflect.Manifest
    import scala.reflect.ClassManifest

    def getExtractor[C](m:Manifest[C]):Option[ColumnTo[C]]= 
        (m match {
          case m if m == Manifest.classType(classOf[String])   => Some(Row.rowToString)
          case m if m == Manifest.Int => Some(Row.rowToInt)
          case m if m == Manifest.Long => Some(Row.rowToLong)
          case m if m >:> Manifest.classType(classOf[Date]) => Some(Row.rowToDate)
          //TODO for Option you need to call recursively, this makes it applicable to future extensions (Option[List[Int]])
          case m if m.erasure==classOf[Option[_]] =>{
            val typeParam=m.typeArguments
                            .headOption
                            .collect { case m:ClassManifest[_] => m}
                            .getOrElse( implicitly[Manifest[Any]] )
            getExtractor(typeParam).flatMap( _ =>
              Some(Row.rowToOption1(typeParam)):Option[_] ) 
          }
          case m if m >:> Manifest.classType(classOf[Id[_]]) =>{
             val typeParam=m.typeArguments
                            .headOption
                            .collect { case m:ClassManifest[_] => m}
                            .getOrElse( implicitly[Manifest[Any]] )
             getExtractor(typeParam).map( mapper => Row.rowToPk(mapper) )
          }
          case _ => None
        }).asInstanceOf[Option[ColumnTo[C]]]



    import SqlRowsParser._
    def apply(input:Input):ParseResult[T] = {
        val paramParser=eatRow( sequence(analyser.names_types.map(i => 
          guard[Any](current(i._1)(getExtractor(i._2).get)))) )

        (paramParser ^^ {case args => 
          {analyser.c.newInstance( args.toSeq.map(_.asInstanceOf[Object]):_*)
            .asInstanceOf[T] } }) (input)
    }
}
case class Analyse[T](override val tableName:Option[String]=None,val m:ClassManifest[T]) extends Analyser[T]

trait Analyser[T]{
  import scala.util.control.Exception._
  import java.lang.reflect._
  import scala.reflect.Manifest
  import scala.reflect.ClassManifest
  val m:ClassManifest[T]
  val tableName:Option[String]=None
  def clean(fieldName:String)=fieldName.split('$').last
  val name=tableName.getOrElse(clean(m.erasure.getSimpleName).toUpperCase())
  def getQualifiedColumnName(column:String)= name+"."+column

  def isConstructorSupported(c:Constructor[_]):Boolean = true

  val electConstructorAndGetInfo:(Constructor[_],Seq[(String,Manifest[_])]) = {

    def getParametersNames(c:Constructor[_]):Seq[String]={
        import scala.collection.JavaConversions._
        play.classloading.enhancers.LocalvariablesNamesEnhancer.lookupParameterNames(c)
      }

      val (cons,paramTypes,paramNames)= 
          m.erasure
           .getConstructors()
           .sortBy(- _.getGenericParameterTypes().length)
           .find(isConstructorSupported)
           .map(c=>(c,c.getGenericParameterTypes().map(manifestFor),getParametersNames(c)))
           .getOrElse(throw new java.lang.Error("no supported constructors for type " +m))

      val coherent=paramTypes.length==paramNames.length
      val names_types =
        paramNames.zip(paramTypes)
                  .map(nt=>(getQualifiedColumnName(clean(nt._1.toUpperCase())),nt._2))

      if(!coherent && names_types.map(_._1).exists(_.contains("outer")))
         throw new java.lang.Error("It seems that your class uses a closure to an outer instance. For MagicParser, please use only top level classes.")

      if(!coherent) throw new java.lang.Error("not coherent to me!")

      (cons,names_types)
  }

  val (c,names_types)=electConstructorAndGetInfo

  val names_methods = 
    handling(classOf[NoSuchMethodException])
        .by(e =>throw new Exception( "The elected constructor doesn't have corresponding methods for all its parameters. "+e.toString))
        .apply(names_types.map(nt=>(nt._1,m.erasure.getDeclaredMethod(nt._1.split('.').last.toLowerCase))))


  def manifestFor(t: Type): Manifest[_] = t match {
    case c: Class[_] => TypeWrangler.manifestOf(c) : Manifest[_]
    case p: ParameterizedType =>
      Manifest.classType[AnyRef](
        p.getRawType.asInstanceOf[Class[AnyRef]],
        manifestFor(p.getActualTypeArguments.head),
        p.getActualTypeArguments.tail.map(manifestFor): _*)
    }
}

object Row{
  def unapplySeq(row:Row):Option[List[Any]]=Some(row.asList)
  
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
  implicit def rowToDate[A >: Date] :ColumnTo[A]= 
    new ColumnTo[A]{
      def transform(row:Row,columnName:String) = row.get1[Date](columnName,false)
    }
  implicit def rowToPk[T](implicit c:ColumnTo[T]) :ColumnTo[Pk[T]]= 
    new ColumnTo[Pk[T]]{
      def transform(row:Row,columnName:String) = c.transform(row,columnName).map(a => Id(a))  
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
  lazy val dictionary2:Map[String,(String,Boolean,String)] = 
    ms.map(m => {val Array(table,column)=m.column.split('.');
                 (column,(table,m.nullable,m.clazz))}).toMap

}

trait Row{
 val metaData:MetaData
  import scala.reflect.Manifest
  protected[sql] val data:List[Any]
  
  lazy val asList =
    data.zip(metaData.ms.map(_.nullable))
        .map(i=> if(i._2) Option(i._1) else i._1)
  lazy val asMap =
    metaData.ms.map(_.column).zip(asList).toMap

  private lazy val ColumnsDictionary:Map[String,Any] =
    metaData.ms.map(_.column).zip(data).toMap

  def get[A](a:String)(implicit c:ColumnTo[A]):MayErr[SqlRequestError,A]=
    c.transform(this,a)

  private def getType(t:String) = t match {
      case "long" => Class.forName("java.lang.Long")
      case "int" => Class.forName("java.lang.Integer")
      case "boolean" => Class.forName("java.lang.Boolean")
      case _ => Class.forName(t)
  }
 
  private[sql] def get1[B](a:String,nullableAlreadyHandled:Boolean)(implicit m : ClassManifest[B]):MayErr[SqlRequestError,B]=
   {for(  meta <- metaData.dictionary2.get(a).map(m=>(m._1+"."+a,m._2,m._3))
                          .orElse(metaData.dictionary.get(a).map(m=> (a,m._1,m._2)))
                          .toRight(ColumnNotFound(a));
          val (qualified,nullable,clazz)=meta;
          val requiredDataType =
            if(m.erasure==classOf[Option[_]]) 
              m.typeArguments.headOption.collect { case m:ClassManifest[_] => m}
               .getOrElse(implicitly[ClassManifest[Any]])
            else m;
          v <- ColumnsDictionary.get(qualified).toRight(ColumnNotFound(qualified));
          result <- v match {
            case b if(nullable && !nullableAlreadyHandled ) =>
              Left(UnexpectedNullableFound(qualified))
            case null if(!nullable ) => Left( ColumnNotFound(qualified))
            case b if(requiredDataType >:>  TypeWrangler.javaType(getType(clazz))) =>
              Right(b.asInstanceOf[B])
            case b => Left(TypeDoesNotMatch(requiredDataType + " - " + clazz))} )
    yield result
  }

  def apply[B](a:String)(implicit c:ColumnTo[B]):B=get[B](a)(c).get
}

case class MockRow(data: List[Any],metaData:MetaData) extends Row

case class SqlRow(metaData:MetaData,data: List[Any]) extends Row {
  override def toString() = "Row("+metaData.ms.zip(data).map(t => "'" + t._1.column + "':" + t._2 + " as " + t._1.clazz).mkString(", ") + ")"
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
import  SqlRowsParser._
case class SimpleSql[T](sql:SqlQuery,params:Seq[(String,Any)], defaultParser:Parser[T]) extends Sql{
  def on(args:(String,Any)*):SimpleSql[T] = 
    this.copy(params=(this.params) ++ args)
  def onParams(args:Any*):SimpleSql[T] = 
    this.copy(params=(this.params) ++ sql.argsInitialOrder.zip(args))
  
  def list(conn:java.sql.Connection=connection) = as(defaultParser*)
  def single(conn:java.sql.Connection=connection) = as(phrase(defaultParser))
  def first(conn:java.sql.Connection=connection) = as(defaultParser)

  def getFilledStatement(connection:java.sql.Connection)={
    val s =connection.prepareStatement(sql.query,java.sql.Statement.RETURN_GENERATED_KEYS)
    val argsMap=Map(params:_*)
    sql.argsInitialOrder.map(argsMap)
               .zipWithIndex
               .map(_.swap)
               .foldLeft(s)((s,e)=>{s.setObject(e._1+1,e._2);s})
  }
  def using[U](p:Parser[U]):SimpleSql[U]=SimpleSql(sql,params,p)
}

case class BatchSql(sql:SqlQuery,params:Seq[Seq[(String,Any)]] ) extends Sql{

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
  import  SqlRowsParser._
  def connection=play.db.DB.getConnection
  def getFilledStatement(connection:java.sql.Connection):java.sql.PreparedStatement
  def filledStatement=getFilledStatement(connection)
  def apply(conn:java.sql.Connection=connection) =     Sql.resultSetToStream(resultSet(connection))

  def resultSet (conn:java.sql.Connection=connection)= 
    (getFilledStatement(connection).executeQuery())
  import SqlRowsParser._
  def as[T](parser:Parser[T], conn:java.sql.Connection=connection):T =
    Sql.as[T](parser,resultSet(connection))

  def execute(conn:java.sql.Connection=connection):Boolean =
    getFilledStatement(connection).execute()

   def execute1(conn:java.sql.Connection=connection):(java.sql.PreparedStatement,Int) =
    {val statement=getFilledStatement(connection);(statement,{statement.executeUpdate()})}

  def executeUpdate(conn:java.sql.Connection=connection):Int =
    getFilledStatement(connection).executeUpdate()
} 

case class SqlQuery(query:String,argsInitialOrder:List[String]=List.empty) extends Sql{
  def getFilledStatement(connection:java.sql.Connection):java.sql.PreparedStatement =
    asSimple().getFilledStatement(connection)
  private def defaultParser : Parser[Seq[Row]] = acceptMatch("not end.", { case Right(r) => r } )*
  def asSimple:SimpleSql[Seq[Row]]=SimpleSql(this,Nil,defaultParser)
  def asSimple[T](parser:Parser[T]=defaultParser):SimpleSql[T]=SimpleSql(this,Nil,parser)
  def asBatch[T]():BatchSql = BatchSql(this,Nil)  
}
object Sql{
  implicit def sqlToSimple(sql:SqlQuery):SimpleSql[Seq[Row]]=sql.asSimple()
  implicit def sqlToBatch(sql:SqlQuery):BatchSql=sql.asBatch()


  def sql(inSql:String):SqlQuery={val (sql,paramsNames)= SqlParser.parse(inSql);SqlQuery(sql,paramsNames)}
  import java.sql._
  import java.sql.ResultSetMetaData._
  def metaData(rs:java.sql.ResultSet)={
    val meta = rs.getMetaData()
    val nbColumns = meta.getColumnCount()
    MetaData(List.range(1,nbColumns+1).map(i =>
      MetaDataItem(column = ( meta.getTableName(i) + "." +
                              meta.getColumnName(i) ).toUpperCase,
                   nullable = meta.isNullable(i)==columnNullable,
                   clazz = meta.getColumnClassName(i))))
  }
 def data(rs:java.sql.ResultSet):List[Any] = {
   val meta = rs.getMetaData()
   val nbColumns = meta.getColumnCount()
   List.range(1,nbColumns+1)
       .map(nb =>rs.getObject(nb))
 }
  def resultSetToStream(rs:java.sql.ResultSet):Stream[SqlRow]={
    Useful.unfold(rs)(rs => if(!rs.next()) {rs.close();None} else Some ((new SqlRow(metaData(rs),data(rs)),rs)))
  }
  import SqlRowsParser._
  def as[T](parser:Parser[T],rs:java.sql.ResultSet):T =
    parser(StreamReader(resultSetToStream(rs))) match{
            case Success(a,_)=>a
            case Failure(e,_)  => error(e)
            case Error(e,_) => error(e) }
}
