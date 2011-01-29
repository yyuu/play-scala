import play.db.jpa.JPABase

/**
 * overriding java types with  cala wrappers
 */
package object controllers {
    type CRUDFor[T] = play.modules.scala.crud.CRUDWrapper[T]
    type Secured = play.modules.scala.secure.SecureWrapper
    implicit def enrichJavaModel(underlying: JPABase) = play.db.jpa.asScala.enrichJavaModel(underlying)
}

package object jobs{
    implicit def enrichJavaModel(underlying: JPABase) = play.db.jpa.asScala.enrichJavaModel(underlying)
}

package object utils{
    implicit def enrichJavaModel(underlying: JPABase) = play.db.jpa.asScala.enrichJavaModel(underlying)
}

package object notifiers{
    implicit def enrichJavaModel(underlying: JPABase) = play.db.jpa.asScala.enrichJavaModel(underlying)
}

package object models{
    implicit def enrichJavaModel(underlying: JPABase) = play.db.jpa.asScala.enrichJavaModel(underlying)
}

package object tests{
    implicit def enrichJavaModel(underlying: JPABase) = play.db.jpa.asScala.enrichJavaModel(underlying)
}
