package play.db

import play.utils.Scala.MayErr
import play.utils.Scala.MayErr._

package anorm {

    trait MParser2[A1,A2,R] extends ParserWithId[R] {
     
        val p1:ColumnTo[A1]
        val p2:ColumnTo[A2]
        def apply(a1:A1,a2:A2):R

        val containerName:String
        val columnNames:(String,String)
      
        val (name1,name2) = columnNames 

        import SqlParser._
        override def apply(input:Input):SqlParser.ParseResult[R] = 
            (get[A1](containerName+"."+name1)(p1) ~< get[A2](containerName+"."+name2)(p2) ^^ {case a1 ~ a2 => apply(a1,a2)} )(input)
          

        val uniqueId : (Row=> MayErr[SqlRequestError,Any]) = null

    }

    abstract class MagicParser2[A1,A2,R](names:Option[(String,String)] = None, tableName: Option[String]=None)
      (implicit val p1:ColumnTo[A1],
                val p2:ColumnTo[A2],
                val r:Manifest[R]) extends MParser2[A1,A2,R] {

          
          //needs clean
          val containerName = tableName.getOrElse(r.erasure.getSimpleName)

          import java.lang.reflect.Method
          
          def getParametersNames(m:Method):Seq[String] = {
              import scala.collection.JavaConversions._
              play.classloading.enhancers.LocalvariablesNamesEnhancer.lookupParameterNames(m)
          }

          val columnNames = names.getOrElse {
              implicitly[Manifest[this.type]]
                  .erasure
                  .getDeclaredMethods()
                  .filter(_.getName()=="apply")
                  .map(m => {println(getParametersNames(m));m})
                  .find(_.getParameterTypes().length == 2)
                  .map(getParametersNames)
                  .collect{case Seq(a1,a2) => (a1,a2)}.get
          }
      }

    trait M2[A1,A2,R]  {
      self : MParser2[A1,A2,R] =>

      val pt1:(ColumnTo[A1],ToStatement[A1])
      val pt2:(ColumnTo[A2],ToStatement[A2])

      def unapply(r:R):Option[(A1,A2)]
      
     // override val conventions: PartialFunction[AnalyserInfo,String] = asIs 
      
      val Self= this
      def update(v:R)(implicit hasId: A1 <:< Pk[_] |:| A2 <:< Pk[_]) {

            val all = ((v,hasId) match {
                case (Self(a1,a2), (e1 |:| e2)) => 
                    ( (e1,name1, toParameterValue(a1)(pt1._2)),
                      (e2,name2, toParameterValue(a2)(pt2._2) ) ) 
            }).productIterator
               .map(_.asInstanceOf[(Option[_],String,ParameterValue[_])])

            val (ids,toSet) = all.partition(_._1.isDefined)
            if(ids == all) throw new Exception("everything is a Pk, nothing left to set!")

            val toUpdate = toSet.map(_._2).map(n => n+" = "+"{"+n+"}").mkString(", ")

            import Sql._

            sql("update "+containerName +" set "+toUpdate+
                " where "+ ids.map(_._2).map( n => n+" = "+"{"+n+"}").mkString(" and ") )
                .onParams(all.map(v =>  v._3).toSeq: _* )
                .executeUpdate()
        }
    }


    abstract class Magic2[A1,A2,R](names:Option[(String,String)] = None, tableName: Option[String]=None)
      (implicit val pt1:(ColumnTo[A1],ToStatement[A1]),
                val pt2:(ColumnTo[A2],ToStatement[A2]),
                r:Manifest[R]) extends MagicParser2[A1,A2,R](names,tableName)(pt1._1,pt2._1,r) with M2[A1,A2,R]

}
