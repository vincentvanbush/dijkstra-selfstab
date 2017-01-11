import akka.actor.ActorRef

abstract class Message

case class Initialize(id: Int, nodes: List[ActorRef]) extends Message

case object Tamper extends Message
