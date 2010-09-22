package play.httpExtensions

import play.libs.WS.HttpResponse
object FunctionalWebResponse{
  implicit def httpResponse2FunctionalWebReponse(response:HttpResponse)= FunctionalWebResponse(response)
}
case class UndesiredStatus(statusCode: Int,body:String) {
override def toString()="Undesired http status code "+ statusCode+":"+body
}
case class FunctionalWebResponse(response:HttpResponse){
  def asOK():Either[UndesiredStatus,FunctionalWebResponse]=asStatus(200)
  def getXml()= response.getXml()
  def getJson()= response.getJson()
  def getString()= response.getString()

  
  //a better method would suggest a status pattern rather which is more flexible about status codes like 2xx
  def asStatus(statusCode:Int):Either[UndesiredStatus,FunctionalWebResponse]={
     if(response.getStatus()==statusCode) Right(this) else Left(UndesiredStatus(response.getStatus().intValue,response.getString() ))
  }
}
