package jp1.akka.lab14

import akka.actor.ActorLogging
import akka.actor.Actor
import akka.actor.Actor.Receive
import akka.actor.Props
import akka.actor.ActorSystem
import akka.actor.ActorRef
import scala.concurrent.duration.*
import java.util.Random
import akka.actor.PoisonPill
import javax.sound.midi.Receiver


case object Strzał
case class Strzał(opponent: ActorRef)
case object Ostrzał
case class Ostrzał(opponent: ActorRef)

case class Zamki(zamki: List[ActorRef])
case object Surrender

class SiłaWyższa extends Actor with ActorLogging {
  def receive: Receive = {
    case Zamki(zamki) => 
      context.actorSelection("../Zamek*") ! Zamki(zamki)
  }
}

class Zamek extends Actor with ActorLogging {
  def receive: Receive = {
    case Zamki(zamki) => 
      val garnizonObrońców = Array.fill(100)(0).foldLeft(List[ActorRef]())((acc, _) => context.actorOf(Props(Obrońca()), s"Defender-${zamki.indexOf(self)}-${acc.length}") :: acc).reverse
      // log.info(s"\n${garnizonObrońców.mkString("\n")}") // nasi obrońcy
      context.become(redCode(zamki.filter((n) => n != self).head, garnizonObrońców))
  }

  def redCode(opponent: ActorRef, garnizonObrońców: List[ActorRef]): Receive = {
    case Zamki(_) =>
      self ! Strzał
    case Strzał => 
      garnizonObrońców.foreach((n) => n ! Strzał(opponent))
    case Ostrzał =>
      log.info(s"${self} have ${garnizonObrońców.length} defenders!")
      if (garnizonObrońców.length == 0) {
        log.info(s"${self} surrendered !!!")
        context.system.terminate()
        context.become(kapitulacja)
      }
      val naKrańcuŻycia = Random().nextBoolean()
      val nieszczęśnik = garnizonObrońców(Random().nextInt(garnizonObrońców.length))
      if (naKrańcuŻycia) then context.become(redCode(opponent, garnizonObrońców.filter((n) => n != nieszczęśnik)))
  }

  def kapitulacja: Receive ={
    case mes =>
      log.info(s"$mes")
  }
}

class Obrońca extends Actor with ActorLogging {
  def receive: Receive = {
    case Strzał(opponent) => opponent ! Ostrzał
  }
}

@main def Castle: Unit = {
  val system = ActorSystem("system")
  implicit val executionContext = system.dispatcher

  val zamekA = system.actorOf(Props[Zamek](), "ZamekA")
  val zamekB = system.actorOf(Props[Zamek](), "ZamekB")

  val siłaWyższa = system.actorOf(Props(SiłaWyższa()), "MayorPotencia")


  val config = system.settings.config
  val delay = config.getInt("planista.delay").milli

  system.scheduler.scheduleWithFixedDelay(
    delay,
    delay,
    siłaWyższa,
    Zamki(List(zamekA, zamekB))
  )
}
