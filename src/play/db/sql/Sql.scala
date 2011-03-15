package play.db

package object sql {
    
    implicit def sqlToSimple(sql:SqlQuery): SimpleSql[Row] = sql.asSimple
    implicit def sqlToBatch(sql:SqlQuery): BatchSql = sql.asBatch
    
    implicit def implicitID[ID](id: Id[ID]): ID = id.id
    
    def SQL(stmt: String) = Sql.sql(stmt)
    
}

package sql {

    import play.utils.Scala.MayErr
    import play.utils.Scala.MayErr._
    import java.util.Date
    
    abstract class SqlRequestError
    case class ColumnNotFound(columnName:String) extends SqlRequestError
    case class TypeDoesNotMatch(message:String) extends SqlRequestError
    case class UnexpectedNullableFound(on:String) extends SqlRequestError
    case object NoColumnsInReturnedResult extends SqlRequestError
    case class IntegrityConstraintViolation(message:String) extends SqlRequestError
    
    trait ColumnTo[A] {
        def transform(row: Row, columnName: String): MayErr[SqlRequestError,A]
    }
    
    case class Column[A] (
    
        nullHandler: Function2[Any,MetaDataItem,Option[SqlRequestError]] = (value, meta) => {
            val MetaDataItem(qualified,nullable,clazz) = meta
            if (nullable || value == null)
                Some(UnexpectedNullableFound(qualified)) 
            else 
                None
        },
    
        transformer: Function2[Any,MetaDataItem,play.utils.Scala.MayErr[SqlRequestError,A]] = (value:Any, meta:MetaDataItem) => {
            Left(TypeDoesNotMatch("Default matcher doesn't match anything"))        
        }
    
    ) extends ColumnTo[A] {
    
        def transform(row:Row, columnName:String): MayErr[SqlRequestError,A] = {
            for(
                meta <- row.metaData.dictionary2.get(columnName).map( m => (m._1 + "." + columnName, m._2, m._3)).orElse(row.metaData.dictionary.get(columnName).map(m=> (columnName,m._1,m._2))).toRight(ColumnNotFound(columnName));
                value <- row.get1(columnName);
                _ <- nullHandler(value, MetaDataItem(meta._1, meta._2, meta._3)).toLeft(value);
                result <- transformer(value, MetaDataItem(meta._1, meta._2, meta._3))
            ) yield result
        }
    
    } 

    case class StreamReader[T](s: Stream[T]) extends scala.util.parsing.input.Reader[Either[EndOfStream,T]] {
  
        def first = s.headOption.toRight(EndOfStream())
        def rest = new StreamReader(s.drop(1))
        def pos = scala.util.parsing.input.NoPosition
        def atEnd = s.isEmpty

    }

    case class EndOfStream()

    import SqlParser.~
    import SqlParser.Parser

    case class TupleFlattener[F](f:F) 

    trait PriorityOne {
        implicit def flattenerTo2[T1,T2] = TupleFlattener[(T1 ~ T2) => (T1,T2)]{case (t1 ~ t2 ) => (t1,t2) }
    }
    
    trait PriorityTwo extends PriorityOne {
        implicit def flattenerTo3[T1,T2,T3] = TupleFlattener[(T1 ~ T2 ~ T3) =>(T1,T2,T3)]{case (t1 ~ t2 ~ t3) => (t1,t2,t3) }
    }
    
    trait PriorityThree extends PriorityTwo {
        implicit def flattenerTo4[T1,T2,T3,T4] :TupleFlattener[(T1 ~ T2 ~T3 ~ T4) =>(T1,T2,T3,T4)] = TupleFlattener[(T1 ~ T2 ~ T3 ~ T4) =>(T1,T2,T3,T4)]{ case (t1~t2~t3~t4) => (t1,t2,t3,t4) }
    }
    
    object TupleFlattener extends PriorityThree {
        implicit def flattenerTo5[T1,T2,T3,T4,T5] = TupleFlattener[(T1 ~ T2 ~ T3 ~ T4 ~ T5) => (T1,T2,T3,T4,T5)]{ case (t1~t2~t3~t4~t5) => (t1,t2,t3,t4,t5) }
    }

    object SqlParser extends SqlParser {
        def flatten[T1,T2,R](implicit f:  TupleFlattener[(T1~T2) => R] ): ((T1~T2) => R) = f.f   
    }

    trait SqlParser extends scala.util.parsing.combinator.PackratParsers {
            
        type Elem = Either[EndOfStream,Row]
  
        import scala.collection.generic.CanBuildFrom
        import scala.collection.mutable.Builder
  
        implicit def extendParser[A](a:Parser[A]): ExtendedParser[A] = ExtendedParser(a)
  
        case class ExtendedParser[A](p:Parser[A]) {
            // a combinator that keeps first parser from consuming input
            def ~<[B](b:Parser[B]): Parser[A ~ B] = guard(p) ~ b
        }
        
        def sequence[A](ps:Traversable[Parser[A]])(implicit bf:CanBuildFrom[Traversable[_], A, Traversable[A]]) = {
            Parser[Traversable[A]] { in => 
                ps.foldLeft(success(bf(ps)))( (s,p) =>
                    for( ss <- s; pp <- p) yield ss += pp
                ) map (_.result) apply in
        }
                             }
        implicit def rowFunctionToParser[T](f:(Row => MayErr[SqlRequestError,T])): Parser[T] = {
            eatRow( Parser[T] { in=>
               in.first.left.map(_=>Failure("End of Stream",in))
                            .flatMap(f(_).left.map(e=>Failure(e.toString,in)))
                            .fold(e=>e, a=> {Success(a,in)}) })
        }


        implicit def rowParserToFunction[T](p:RowParser[T]): (Row => MayErr[SqlRequestError,T]) = p.f

        case class RowParser[A](f: (Row=>MayErr[SqlRequestError,A])) extends Parser[A] {
            lazy val parser=rowFunctionToParser(f)
            def apply(in:Input) = parser(in)
        }
                  
        def str(columnName:String): RowParser[String] = get[String](columnName)(implicitly[ColumnTo[String]])
        
        def bool(columnName:String): RowParser[Boolean] = get[Boolean](columnName)(implicitly[ColumnTo[Boolean]])
        
        def int(columnName:String): RowParser[Int] = get[Int](columnName)(implicitly[ColumnTo[Int]])
        
        def long(columnName:String): RowParser[Long] = get[Long](columnName)(implicitly[ColumnTo[Long]])
        
        def date(columnName:String): RowParser[Date] = get[Date](columnName)(implicitly[ColumnTo[Date]])
      
        def get[T](columnName:String)(implicit extractor:ColumnTo[T]): RowParser[T] = RowParser( extractor.transform(_,columnName))

        def contains[T](columnName:String,t:T)(implicit extractor:ColumnTo[T]): Parser[Unit] = guard(get[T](columnName)(extractor) ^? {case a if a==t => Unit})

        def current[T](columnName:String)(implicit extractor:ColumnTo[T]): RowParser[T] = RowParser( extractor.transform(_,columnName))

        def eatRow[T](p:Parser[T]) = p <~ newLine

        def current1[T](columnName:String)(implicit extractor:ColumnTo[T]): Parser[T] = commit(current[T](columnName)(extractor))
  
        def newLine:Parser[Unit] = Parser[Unit] {
            in => if(in.atEnd) Failure("end",in) else Success(Unit,in.rest) 
        }
        
        def scalar[T](implicit m:Manifest[T]) = {
            SqlParser.RowParser(row => 
                row.asList
                .headOption.toRight(NoColumnsInReturnedResult)
                   .flatMap( a =>
                       if (m >:>  TypeWrangler.javaType(a.asInstanceOf[AnyRef].getClass))
                            Right(a.asInstanceOf[T]) 
                       else 
                            Left(TypeDoesNotMatch(m.erasure + " and "+a.asInstanceOf[AnyRef].getClass)) 
                    )
                )
        }

        def spanM[A](by:(Row=> MayErr[SqlRequestError,Any]),a:Parser[A]):Parser[Seq[A]] = {
            val d=guard(by)
            d >> (first => Parser[Seq[A]] { in =>
                //instead of cast it'd be much better to override type Reader
                {
                    val (groupy,rest) =in.asInstanceOf[StreamReader[Row]].s.span(by(_).right.toOption.exists(r=>r==first))
                    val g = (a *)(StreamReader(groupy))
                    g match{
                        case Success(a,_)=> Success(a,StreamReader(rest))
                        case Failure(msg,_) => Failure(msg,in)
                        case Error(msg,_) => Error(msg,in) 
                    }
                }
            })
        }

        implicit def symbolToColumn(columnName:Symbol):ColumnSymbol = ColumnSymbol(columnName)

        case class ColumnSymbol(name:Symbol) {
      
            def of[T](implicit extractor:ColumnTo[T] ):Parser[T] = get[T](name.name)(extractor)
            def is[T](t:T)(implicit extractor:ColumnTo[T] ):Parser[Unit] = contains[T](name.name,t)(extractor)
  
        }
  
        implicit def rowToString: Column[String] = {
            Column[String](transformer = { (value, meta) =>
                val MetaDataItem(qualified,nullable,clazz) = meta
                value match {
                    case string:String => Right(string)
                    case clob:java.sql.Clob => Right(clob.getSubString(1,clob.length.asInstanceOf[Int]))
                    case _ => Left(TypeDoesNotMatch("Cannot convert " + value + " to String for column " + qualified))
                }        
            })
        }  
    
        implicit def rowToInt: Column[Int] = {
            Column[Int](transformer = { (value, meta) =>
                val MetaDataItem(qualified,nullable,clazz) = meta
                value match {
                    case int:Int => Right(int)
                    case _ => Left(TypeDoesNotMatch("Cannot convert " + value + " to Int for column " + qualified))
                }            
            })
        } 

        implicit def rowToBoolean: Column[Boolean] = {
            Column[Boolean](transformer = { (value, meta) =>
                val MetaDataItem(qualified,nullable,clazz) = meta
                value match {
                     case bool:Boolean => Right(bool)
                     case _ => Left(TypeDoesNotMatch("Cannot convert " + value + " to Boolean for column " + qualified))
                }            
            })
        } 

        implicit def rowToLong: Column[Long] = {
            Column[Long](transformer = { (value, meta) =>
                val MetaDataItem(qualified,nullable,clazz) = meta
                value match {
                    case int:Int => Right(int:Long)
                    case long:Long => Right(long)
                    case _ => Left(TypeDoesNotMatch("Cannot convert " + value + " to Long for column " + qualified))
                }            
            })
        } 
      
        implicit def rowToDate: Column[Date] = {
            Column[Date](transformer = { (value, meta) =>
                val MetaDataItem(qualified,nullable,clazz) = meta
                value match {
                     case date:Date => Right(date)
                     case _ => Left(TypeDoesNotMatch("Cannot convert " + value + " to Date for column " + qualified))
                 }            
            })
        } 

        implicit def rowToPk[T](implicit c:ColumnTo[T]) :ColumnTo[Pk[T]] = {
            new ColumnTo[Pk[T]]{
              override def transform(row:Row,columnName:String) = c.transform(row,columnName).map(a => Id(a))  
            }
        } 
   
        implicit def rowToOption1[T](implicit c:Column[T]): ColumnTo[Option[T]] = {
            Column[Option[T]](
                nullHandler = (v, meta) => {
                    val MetaDataItem(qualified,nullable,clazz) = meta
                    if(!nullable) Some(UnexpectedNullableFound(qualified)) else None
                },
                transformer = (v, meta) => {
                    if(v == null) Right(None) else c.transformer(v, meta).map( x => Some(x) )
                }
            )
        }
  
    }

    abstract class Pk[+ID] {
        def isAssigned:Boolean = this match {
            case Id(_) => true
            case NotAssigned => false
        } 
  
        def apply() = get.get
  
        def get = this match{
            case Id(id) => Some(id)
            case NotAssigned => None
        }

    }
    
    case class Id[ID](id:ID) extends Pk[ID] {
        override def toString() = id.toString
    }
    
    case object NotAssigned extends Pk[Nothing] {
        override def toString() = "NotAssigned"
    }

    case class Magic[T](override val tableName:Option[String]=None)(implicit val m:ClassManifest[T]) extends M[T] {
        def using(tableName:Symbol) = this.copy(tableName=Some(tableName.name))
    }

    trait M[T] extends MParser[T] {
        self =>

        val msql:MSql[T] = new MSql[T] {
            override lazy val analyser = self.analyser
            override val m = self.m
        }

        val idParser:SqlParser.Parser[_] = {
            SqlParser.RowParser(row => 
                row.asList.headOption.flatMap(a => 
                    (if (a.isInstanceOf[Option[_]]) a else Option(a)).asInstanceOf[Option[_]]
                ).toRight(NoColumnsInReturnedResult)
            )
        }

        import SqlParser._
        import Sql._
        
        def find(stmt:String=""):SimpleSql[T] = msql.find(stmt).using(self) 
        def count(stmt:String=""):SimpleSql[Long] = msql.count(stmt).using(scalar[Long])
  
        import scala.util.control.Exception._
  
        def delete(stmt:String):SqlQuery = {
            sql(stmt match {
                case s if s.startsWith("delete") => s
                case s if s.startsWith("where") => "delete from " + analyser.name + " " + s
                case s => "delete from " + analyser.name + " where " + s
            })
        }

        def update(v:T) {
            val names_attributes = analyser.names_methods.map(nm => (nm._1, nm._2.invoke(v) ))
            val (ids,toSet) = names_attributes.map(na => (na._1, na._2 match {
                case v:Option[_]=>v.getOrElse(null)
                case v=>v
            })).partition(na => na._2.isInstanceOf[Pk[_]])
    
            if(ids == Nil) throw new Exception("cannot update without Ids, no Ids found on "+analyser.name)
    
            val toUpdate = toSet.map(_._1).map(n => n+" = "+"{"+n+"}").mkString(", ")
            
            sql("update "+analyser.name+" set "+toUpdate+" where "+ ids.map(_._1).map( n => n+" = "+"{"+n+"}").mkString(" and "))
                .onParams(toSet.map(_._2) ++ ids.map(_._2).map { 
                    case Id(id) => id
                    case other => throw new Exception("not set ids in the passed object")
                } : _*)
            .executeUpdate()
        }

        def create(v:T): MayErr[IntegrityConstraintViolation,T] = {
            val names_attributes = analyser.names_methods.map(nm => (nm._1, nm._2.invoke(v) ))
            val (notSetIds,toSet) = names_attributes.map(na => 
                (na._1, na._2 match {
                    case Id(id)=>id
                    case v:Option[_]=>v.getOrElse(null)
                    case v=>v
                })
            ).partition(na => na._2 == NotAssigned)
            
            if(notSetIds.length > 1) throw new Exception("multi ids not supported")
            val toInsert = toSet.map(_._1)
    
            val query = sql("insert into "+analyser.name+" ( "+toInsert.mkString(", ")+" ) values ( "+toInsert.map("{"+_+"}").mkString(", ")+")")
                            .onParams(toSet.map(_._2):_*)
    
            val result = catching(classOf[java.sql.SQLException])
                            .either(query.execute1())
                            .left.map( e => IntegrityConstraintViolation(e.asInstanceOf[java.sql.SQLException].getMessage))

            for { 
                r <- result;
                val (statement,ok) = r;
                val rs = statement.getGeneratedKeys();
                val id=idParser(StreamReader(Sql.resultSetToStream(rs))).get
                val params = names_attributes.map(_._2).map({case NotAssigned => Id(id); case other => other})
            } yield analyser.c.newInstance(params:_*).asInstanceOf[T] //StreamReader(Sql.resultSetToStream(rs))
            
        }

        def insert(v:T):MayErr[IntegrityConstraintViolation,Boolean] = {
            val names_attributes = analyser.names_methods.map(nm => (nm._1, nm._2.invoke(v) ))
            val (notSetIds,toSet) = names_attributes.map(na => 
                ( na._1, na._2 match {
                    case Id(id)=>id;
                    case v:Option[_]=>v.getOrElse(null);
                    case v=>v
                })
            ).partition(na => na._2 == NotAssigned)

            val toInsert = toSet.map(_._1)
    
            val query = sql("insert into "+analyser.name+" ( "+toInsert.mkString(", ")+" ) values ( "+toInsert.map("{"+_+"}").mkString(", ")+")")
                            .onParams(toSet.map(_._2):_*)
    
            catching(classOf[java.sql.SQLException])
                .either(query.execute())
                .left.map(e => IntegrityConstraintViolation(e.asInstanceOf[java.sql.SQLException].getMessage))

        }    
    }
    
    case class MagicSql[T] ( override val tableName:Option[String]=None)(implicit val m:ClassManifest[T]) extends  MSql[T] {
        def using(tableName:Symbol) = this.copy(tableName=Some(tableName.name))
    }
    
    trait MSql[T] {
        
        val m:ClassManifest[T]
        val tableName:Option[String] = None
        lazy val analyser = new Analyse[T](tableName,m)
    
        import Sql._
        import java.lang.reflect._
        
        def find(stmt:String=""):SimpleSql[Row] = sql(stmt.trim() match {
            case s if s.startsWith("select") => s
            case s if s.startsWith("where") => "select * from " + analyser.name + " " + s
            case s if s.startsWith("order by") => "select * from " + analyser.name + " " + s
            case "" => "select * from " + analyser.name
            case s => "select * from " + analyser.name + " where " + s
        }).asSimple
    
        def count(stmt:String=""):SimpleSql[Row] = sql(stmt.trim() match {
            case s if s.startsWith("select") => s
            case s if s.startsWith("where") => "select count(*) from " + analyser.name + " " + s
            case "" => "select count(*) from " + analyser.name
            case s => "select count(*) from " + analyser.name + " where " + s
        }).asSimple
    
    }

    case class MagicParser[T] ( override val tableName:Option[String]=None)(implicit val m:ClassManifest[T]) extends  MParser[T] with Analyser[T] {
        def using(tableName:Symbol) = this.copy(tableName=Some(tableName.name))
    }
    
    trait ParserWithId[T] extends SqlParser.Parser[T] {
        parent =>
  
        import  SqlParser._ 
  
        def apply(input:Input):ParseResult[T]
  
        val uniqueId : (Row=> MayErr[SqlRequestError,Any])
  
        def ~<[B](other: ParserWithId[B]):ParserWithId[T ~ B] = new ParserWithId[T ~ B] {
            
            def apply(input:Input) = ((parent: SqlParser.Parser[T]) ~< other)(input)
            
            val uniqueId : (Row=> MayErr[SqlRequestError,Any]) = {
                row => for (a <- (parent.uniqueId(row)) ; b <- other.uniqueId(row)) yield (a,b)
            }
        }
   
        def ~<[B](other: Parser[B]):Parser[T ~ B] = (this:Parser[T]).~<(other)

        def span[B](p:Parser[B]) : Parser[T ~ B] = {
            val d = guard(uniqueId)
            guard(this) ~ ( d >> (first => Parser[B] {in =>
                //instead of cast it'd be much better to override type Reader
                { 
                    val (groupy,rest) = in.asInstanceOf[StreamReader[Row]].s.span(uniqueId(_).right.toOption.exists(r=>r==first));
                    val g = p(StreamReader(groupy))
                    g match {
                        case Success(r,_)=> Success(r,StreamReader(rest))
                        case Failure(msg,_) => Failure(msg,in)
                        case Error(msg,_) => Error(msg,in) 
                    }
                }
            }))
        }

        def spanM[B](b:Parser[B]) : Parser[T ~ Seq[B]] = span(b *)
        
    }

    trait MParser[T] extends ParserWithId[T] {
    
        val m:ClassManifest[T]
        val tableName:Option[String] = None
        val analyser = new Analyse[T](tableName,m) {
            
            import java.lang.reflect._
      
            override def isConstructorSupported(c:Constructor[_]):Boolean = c.getGenericParameterTypes().forall(t => getExtractor(manifestFor(t)).isDefined)
        }
        
        import java.lang.reflect._
        import scala.reflect.Manifest
        import scala.reflect.ClassManifest

        def getExtractor[C](m:Manifest[C]):Option[ColumnTo[C]] = (m match {
            case m if m == Manifest.classType(classOf[String])   => Some(SqlParser.rowToString)
            case m if m == Manifest.Int => Some(SqlParser.rowToInt)
            case m if m == Manifest.Long => Some(SqlParser.rowToLong)
            case m if m == Manifest.Boolean => Some(SqlParser.rowToBoolean)
            case m if m >:> Manifest.classType(classOf[Date]) => Some(SqlParser.rowToDate)
            case m if m.erasure == classOf[Option[_]] => {
                val typeParam=m.typeArguments
                                .headOption
                                .collect { case m:ClassManifest[_] => m }
                                .getOrElse( implicitly[Manifest[Any]] )
                    
                getExtractor(typeParam).collect {
                    case e:Column[_] => SqlParser.rowToOption1(e)
                } 
            }
            case m if m >:> Manifest.classType(classOf[Id[_]]) => {
                val typeParam=m.typeArguments
                                .headOption
                                .collect { case m:ClassManifest[_] => m}
                                .getOrElse( implicitly[Manifest[Any]] )
                getExtractor(typeParam).map( mapper => SqlParser.rowToPk(mapper) )
            }
            case _ => None
        }).asInstanceOf[Option[ColumnTo[C]]]

        import SqlParser._

        val uniqueId : (Row=> MayErr[SqlRequestError,Any]) = {
            val ids = analyser.names_types.collect { 
                case (n,m) if m >:> Manifest.classType(classOf[Id[_]]) => (n,m)
            }
            if(ids != Nil) 
                row => 
                    ids.map(i => getExtractor(i._2).get.transform(row,i._1) )
                       .reduceLeft((a,b) => for( aa <- a ; bb <- b) yield new ~(a,b))
            else
                row => 
                    analyser.names_types.map(i =>getExtractor(i._2).get.transform(row,i._1) )
                            .reduceLeft((a,b) => for( aa <- a ; bb <- b) yield new ~(a,b))
        }

        def apply(input:Input):ParseResult[T] = {
            val paramParser=eatRow( sequence(analyser.names_types.map(i => 
                guard[Any](current(i._1)(getExtractor(i._2).get))
            )))

            (paramParser ^^ { 
                case args => { 
                    analyser.c.newInstance( args.toSeq.map(_.asInstanceOf[Object]):_*).asInstanceOf[T] 
                } 
            }) (input)
        }
    }

    case class Analyse[T](override val tableName:Option[String]=None,val m:ClassManifest[T]) extends Analyser[T]

    trait Analyser[T]{
        
        import scala.util.control.Exception._
        import java.lang.reflect._
        import scala.reflect.Manifest
        import scala.reflect.ClassManifest
  
        val m:ClassManifest[T]
        val tableName:Option[String] = None
        
        def clean(fieldName:String) = fieldName.split('$').last
  
        val name = tableName.getOrElse(clean(m.erasure.getSimpleName).toUpperCase())
  
        def getQualifiedColumnName(column:String) = name+"."+column

        def isConstructorSupported(c:Constructor[_]):Boolean = true

        val electConstructorAndGetInfo:(Constructor[_],Seq[(String,Manifest[_])],Seq[(String,java.lang.reflect.Method)]) = {

            def getParametersNames(c:Constructor[_]):Seq[String] = {
                import scala.collection.JavaConversions._
                play.classloading.enhancers.LocalvariablesNamesEnhancer.lookupParameterNames(c)
            }

            val (cons,paramTypes,paramNames) = m.erasure
                                                .getConstructors()
                                                .sortBy(- _.getGenericParameterTypes().length)
                                                .find(isConstructorSupported)
                                                .map(c=>(c,c.getGenericParameterTypes().map(manifestFor),getParametersNames(c)))
                                                .getOrElse(throw new java.lang.Error("no supported constructors for type " +m))

            val coherent = paramTypes.length == paramNames.length
      
            val names_types =  paramNames.zip(paramTypes).map( nt => 
                (getQualifiedColumnName(clean(nt._1.toUpperCase())),nt._2)
            )

            if(!coherent && names_types.map(_._1).exists(_.contains("outer")))
                throw new java.lang.Error("It seems that your class uses a closure to an outer instance. For MagicParser, please use only top level classes.")

            if(!coherent) throw new java.lang.Error("not coherent to me!")
      
            val names_methods = handling(classOf[NoSuchMethodException])
                  .by(e =>throw new RuntimeException( "The elected constructor doesn't have corresponding methods for all its parameters. "+e.toString))
                  .apply(paramNames.map(name=>(name,m.erasure.getDeclaredMethod(name))))

            (cons,names_types,names_methods)
        }

        val (c,names_types,names_methods) = electConstructorAndGetInfo

        def manifestFor(t: Type): Manifest[_] = t match {
            case c: Class[_] => TypeWrangler.manifestOf(c) : Manifest[_]
            case p: ParameterizedType => Manifest.classType[AnyRef](
                                            p.getRawType.asInstanceOf[Class[AnyRef]],
                                            manifestFor(p.getActualTypeArguments.head),
                                            p.getActualTypeArguments.tail.map(manifestFor): _*
                                        )
        }

    }

    object Row{
        def unapplySeq(row:Row):Option[List[Any]]=Some(row.asList)
    }

    case class MetaDataItem(column:String,nullable:Boolean,clazz:String)

    case class MetaData(ms:List[MetaDataItem]) {
        
        lazy val dictionary= ms.map(m => (m.column,(m.nullable,m.clazz))).toMap
        lazy val dictionary2:Map[String,(String,Boolean,String)] = {
            ms.map(m => {val Array(table,column)=m.column.split('.');
            (column,(table,m.nullable,m.clazz))}).toMap
        }

    }

    trait Row {
        
        val metaData:MetaData
  
        import scala.reflect.Manifest
  
        protected[sql] val data:List[Any]
  
        lazy val asList = data.zip(metaData.ms.map(_.nullable)).map(i=> if(i._2) Option(i._1) else i._1)
 
        lazy val asMap = metaData.ms.map(_.column).zip(asList).toMap

        private lazy val ColumnsDictionary:Map[String,Any] = metaData.ms.map(_.column).zip(data).toMap

        def get[A](a:String)(implicit c:ColumnTo[A]):MayErr[SqlRequestError,A] = c.transform(this,a)

        private def getType(t:String) = t match {
            case "long" => Class.forName("java.lang.Long")
            case "int" => Class.forName("java.lang.Integer")
            case "boolean" => Class.forName("java.lang.Boolean")
            case _ => Class.forName(t)
        }
 
        private[sql] def get1(a:String):MayErr[SqlRequestError,Any] = {
            for(
                meta <- metaData.dictionary2.get(a).map(m=>(m._1+"."+a,m._2,m._3))
                                .orElse(metaData.dictionary.get(a).map(m=> (a,m._1,m._2)))
                                .toRight(ColumnNotFound(a));
                val (qualified,nullable,clazz) = meta;
                result <- ColumnsDictionary.get(qualified).toRight(ColumnNotFound(qualified))
            ) yield result    
        }

        def apply[B](a:String)(implicit c:ColumnTo[B]):B = get[B](a)(c).get
        
    }

    case class MockRow(data:List[Any], metaData:MetaData) extends Row

    case class SqlRow(metaData:MetaData, data: List[Any]) extends Row {
        override def toString() = "Row("+metaData.ms.zip(data).map(t => "'" + t._1.column + "':" + t._2 + " as " + t._1.clazz).mkString(", ") + ")"
    }
    
    object Useful {
    
        case class Var[T](var content:T)
    
        def drop[A](these:Var[Stream[A]], n:Int): Stream[A] = {
            var count = n
            while (!these.content.isEmpty && count > 0) {
                these.content = these.content.tail
                count -= 1
            }
            these.content
        } 
        
        def unfold1[T, R](init: T)(f: T => Option[(R, T)]): (Stream[R],T) = f(init) match {
            case None => (Stream.Empty,init)
            case Some((r, v)) => (Stream.cons(r,unfold(v)(f)),v)
        }
        
        def unfold[T, R](init: T)(f: T => Option[(R, T)]): Stream[R] = f(init) match {
            case None => Stream.Empty
            case Some((r, v)) => Stream.cons(r,unfold(v)(f))
        }
    }
    
    import  SqlParser._
    
    case class SimpleSql[T](sql:SqlQuery,params:Seq[(String,Any)], defaultParser:SqlParser.Parser[T]) extends Sql {
        
        def on(args:(String,Any)*):SimpleSql[T] = this.copy(params=(this.params) ++ args)
        
        def onParams(args:Any*):SimpleSql[T] = this.copy(params=(this.params) ++ sql.argsInitialOrder.zip(args))
  
        def list(conn:java.sql.Connection=connection):Seq[T] = as(defaultParser*)
  
        def single(conn:java.sql.Connection=connection):T = as(phrase(defaultParser))
  
        def first(conn:java.sql.Connection=connection):Option[T] = as(defaultParser?)

        def getFilledStatement(connection:java.sql.Connection) = {
            val s =connection.prepareStatement(sql.query,java.sql.Statement.RETURN_GENERATED_KEYS)
            val argsMap=Map(params:_*)
            sql.argsInitialOrder.map(argsMap)
               .zipWithIndex
               .map(_.swap)
               .foldLeft(s)((s,e)=>{s.setObject(e._1+1,e._2);s})
        }
        
        def using[U](p:Parser[U]):SimpleSql[U] = SimpleSql(sql,params,p)
        
    }

    case class BatchSql(sql:SqlQuery, params:Seq[Seq[(String,Any)]]) extends Sql {

        def addBatch(args:(String,Any)*):BatchSql = this.copy(params=(this.params) :+ args)

        def addBatchParams(args:Any*):BatchSql = this.copy(params=(this.params) :+ sql.argsInitialOrder.zip(args))

        def getFilledStatement(connection:java.sql.Connection) = {
            val statement=connection.prepareStatement(sql.query)
            params.foldLeft(statement)( (s,ps) => {
                s.addBatch()
                val argsMap=Map(ps:_*)
                sql.argsInitialOrder
                    .map(argsMap)
                    .zipWithIndex
                    .map(_.swap)
                    .foldLeft(s)( (s,e) => {s.setObject(e._1+1,e._2);s} )
            })
        }
    }
    
    trait Sql {
        
        import SqlParser._
        import scala.util.control.Exception._
  
        def connection = play.db.DB.getConnection
        
        def getFilledStatement(connection:java.sql.Connection):java.sql.PreparedStatement
        
        def filledStatement = getFilledStatement(connection)
  
        def apply(conn:java.sql.Connection=connection) = Sql.resultSetToStream(resultSet(connection))

        def resultSet (conn:java.sql.Connection=connection) = (getFilledStatement(connection).executeQuery())
  
        import SqlParser._
  
        def as[T](parser:Parser[T], conn:java.sql.Connection=connection):T = Sql.as[T](parser,resultSet(connection))

        def execute(conn:java.sql.Connection=connection):Boolean = getFilledStatement(connection).execute()

        def execute1(conn:java.sql.Connection=connection):(java.sql.PreparedStatement,Int) = {
            val statement=getFilledStatement(connection)
            (statement, {statement.executeUpdate()} )
        }

        def executeUpdate(conn:java.sql.Connection=connection):MayErr[IntegrityConstraintViolation,Int] = {
            catching(classOf[java.sql.SQLException])
                .either(getFilledStatement(connection).executeUpdate())
                .left.map(e => IntegrityConstraintViolation(e.asInstanceOf[java.sql.SQLException].getMessage))
        }
        
    } 

    case class SqlQuery(query:String,argsInitialOrder:List[String]=List.empty) extends Sql {

        def getFilledStatement(connection:java.sql.Connection):java.sql.PreparedStatement = asSimple().getFilledStatement(connection)
  
        private def defaultParser : Parser[Row] = acceptMatch("not end.", { case Right(r) => r } )
  
        def asSimple:SimpleSql[Row]=SimpleSql(this,Nil,defaultParser)
  
        def asSimple[T](parser:Parser[T]=defaultParser):SimpleSql[T]=SimpleSql(this,Nil,parser)
  
        def asBatch[T]:BatchSql = BatchSql(this,Nil)  
    }
    
    object Sql {

        def sql(inSql:String):SqlQuery={val (sql,paramsNames)= SqlStatementParser.parse(inSql);SqlQuery(sql,paramsNames)}
  
        import java.sql._
        import java.sql.ResultSetMetaData._
  
        def metaData(rs:java.sql.ResultSet) = {    
            val meta = rs.getMetaData()
            val nbColumns = meta.getColumnCount()
            MetaData(List.range(1,nbColumns+1).map(i =>
                MetaDataItem(column = ( meta.getTableName(i) + "." + meta.getColumnName(i) ).toUpperCase,
                                nullable = meta.isNullable(i)==columnNullable,
                                clazz = meta.getColumnClassName(i)
                )
            ))            
        }
        
        def data(rs:java.sql.ResultSet):List[Any] = {
            val meta = rs.getMetaData()
            val nbColumns = meta.getColumnCount()
            List.range(1,nbColumns+1).map(nb =>rs.getObject(nb))
        }
        
        def resultSetToStream(rs:java.sql.ResultSet):Stream[SqlRow] = {
            Useful.unfold(rs)(rs => if(!rs.next()) {rs.close();None} else Some ((new SqlRow(metaData(rs),data(rs)),rs)))
        }
        
        import SqlParser._
  
        def as[T](parser:Parser[T],rs:java.sql.ResultSet):T = parser(StreamReader(resultSetToStream(rs))) match {
            case Success(a,_)=>a
            case Failure(e,_)  => error(e)
            case Error(e,_) => error(e) 
        }
    
    }

}