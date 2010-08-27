package play.jobs

import play._
import play.exceptions._

import scala.actors.Actor 
import scala.actors.Actor._

object PlayActor extends Actor {
    
    def !!![T](msg: Function0[T]): Future[Either[Throwable,T]] = (PlayActor !! msg).asInstanceOf[Future[Either[Throwable,T]]]
    
    def act {
        loop {
            react {
                case f: Function0[_] => play.Invoker.invokeInThread(new play.Invoker.DirectInvocation() {
                    override def execute {
                        try {
                            sender ! Right(f())
                        } catch {
                            case e => val element = PlayException.getInterestingStrackTraceElement(e)
                                      if (element != null) {
                                          error(
                                              new JavaExecutionException(
                                                  Play.classes.getApplicationClass(element.getClassName()), element.getLineNumber(), e
                                              ),
                                              "Caught in PlayActor"
                                          )
                                      } else {
                                          error(e, "Caught in PlayActor")
                                      }
                                      sender ! Left(e)
                        }                        
                    }
                })
                case _ => sender ! Left(new Exception("Unsupported message type"))
            }   
        }
    }
    
    PlayActor.start

}