import javax.sound.midi.Receiver

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.ActorRef

case class Oblicz(n: Int)
case class Wynik(n: Int, fib: Int)
case class Odp(n: Int, fib: Int)
case class AddToDo(n: Int)
case class ObliczTuple(n1: Int, n2: Int)

class Boss extends Actor with ActorLogging {
    def receive: Receive = {
        case Oblicz(n) => 
            val nadzorca = context.actorOf(Props(Nadzorca(self)), "nadzorca")
            nadzorca ! Oblicz(n)
        case Wynik(n, fib) =>
            log.info(s"Your ${n} fibo is: ${fib}")
    }
}

class Nadzorca(boss: ActorRef, cache: Map[Int, Int] = Map(1 -> 1, 2 -> 1), toDo: List[ActorRef] = List()) extends Actor with ActorLogging { 
    def receive: Receive = {
        case Oblicz(n) =>
            if (cache.contains(n)) boss ! Wynik(n, cache.getOrElse(n, -1))
            else {
                val pracownik = context.actorOf(Props(Pracownik(n)), s"pracownik${n}")
                context.become(calculating(cache, pracownik :: toDo))
                pracownik ! Oblicz(n)
            }
    }

    def calculating(cache: Map[Int, Int], toDo: List[ActorRef]): Receive = {
        case ObliczTuple(n1, n2) =>
            if (!toDo.contains(sender())) context.become(calculating(cache, sender() :: toDo))
            if (cache.contains(n2) && cache.contains(n1)) sender() ! ObliczTuple(cache(n1), cache(n2))
            else self ! AddToDo(n1)
        
        case Wynik(n, fib) =>
            if (toDo.length > 0) context.become(calculating(cache + (n -> fib), toDo.tail))
            else context.become(calculating(cache + (n -> fib), toDo))
            self ! Odp(n, fib)

        case Odp(n, fib) => 
            if (toDo.size == 0) boss ! Wynik(n, cache(n))
            else toDo.head ! Oblicz(n + 1)
        
        case AddToDo(n) =>
            val pracownik = context.actorOf(Props(Pracownik(n)), s"pracownik${n}")
            pracownik ! Oblicz(n)
    }
}


class Pracownik(k: Int) extends Actor with ActorLogging {
    def receive: Receive = {
        case Oblicz(n) => 
            sender() ! ObliczTuple(n - 1, n - 2)
        case ObliczTuple(n1, n2) =>
            sender() ! Wynik(k, n1 + n2)
    }
}

@main def Fibo: Unit = {
    val system = ActorSystem("system")
    val boss = system.actorOf(Props[Boss](), "boss")
    boss ! Oblicz(28)
}

// Actor 1 -> 2, 3, 4, 5

// Actor 2 -> 1, 3 ,4, 5
// Actor 3 -> 1, 3 ,4, 5  ->
// Actor 4 -> 1, 3 ,4, 5 --> 
// Actor 5 -> 1, 3 ,4, 5
