package play.db.sql

object Row{
 def unapplySeq(row:Row):Option[List[Any]]={
    Some(row.tuples.map(_._2))
  }
}
class Row(rs:java.sql.ResultSet){
  import java.sql._
  import java.sql.ResultSetMetaData._
  import scala.reflect.Manifest
  lazy val ColumnsDictionary=tuples.toMap
  def get[B](a:String)(implicit m : ClassManifest[B]):Option[B]=
    ColumnsDictionary.get(a).collect{ case b:AnyRef 
                                        if (m.erasure.isAssignableFrom(b.getClass)) => 
                                          b.asInstanceOf[B] }

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

   def unfold[T, R](init: T)(f: T => Option[(R, T)]): Stream[R] = f(init) match {
     case None => Stream.Empty
     case Some((r, v)) => Stream.cons(r,unfold(v)(f))
   }
}
case class Sql(sqlQuery:String,argsInitialOrder:List[String],params:Seq[(String,Any)]=List.empty){
  def on(args:(String,Any)*):Sql=this.copy(params=(this.params) ++ args)
  def onParams(args:Any*):Sql=this.copy(params=(this.params) ++ argsInitialOrder.zip(args))
  lazy val filledStatement={
    val s=play.db.DB.getConnection.prepareStatement(sqlQuery)
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
  def resultSetToStream(rs:java.sql.ResultSet):Stream[Row]={
    Useful.unfold(rs)(rs => if(!rs.next()) {rs.close();None} else Some(new Row(rs),rs))
  }
}

