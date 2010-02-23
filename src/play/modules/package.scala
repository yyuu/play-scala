package object controllers {

    type CRUD[T] = play.scalasupport.crud.CRUDWrapper[T]
    type Secure = play.scalasupport.secure.SecureWrapper

}