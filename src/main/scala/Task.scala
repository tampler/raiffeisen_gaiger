package hello

import java.util.concurrent.atomic.AtomicReference

import scala.concurrent.duration.Duration
import scala.concurrent.{ blocking, Await, ExecutionContext, Future, Promise }
import scala.util.{ Random, Success }

final case class Particle(mass: Int, charge: Int)

trait GeigerCounter {
  def registerListener(listener: Particle => Unit): Unit
  def start(n: Int)(ec: ExecutionContext): Future[Unit]
}
object GeigerCounter {
  def make: GeigerCounter = new GeigerCounter {
    private val listeners = new AtomicReference(Vector.empty[Particle => Unit])

    def registerListener(listener: Particle => Unit): Unit = {
      listeners.getAndUpdate(_ :+ listener)
      ()
    }

    def start(n: Int)(ec: ExecutionContext): Future[Unit] = {
      implicit val _ec: ExecutionContext = ec
      val p                              = Promise[Unit]
      Future {
        Range(0, n).foreach { _ =>
          blocking {
            Thread.sleep(1000)
          }
          val prt = Particle(Random.between(0, 1000), Random.between(-1, 2))
          println(s"[${Thread.currentThread().getName}] Emitted $prt")
          val lsnrs = listeners.get()
          lsnrs.foreach(lsnr => Future(lsnr(prt)))

        }
        p.complete(Success(()))
      }
      p.future
    }
  }
}

object MyAppImpure extends App {
  val g = GeigerCounter.make
  val f = g.start(15)(ExecutionContext.global)

  Thread.sleep(3000)
  g.registerListener(p => println(s"[${Thread.currentThread().getName}] Listener1: caught $p"))
  Thread.sleep(3000)
  g.registerListener(p => println(s"[${Thread.currentThread().getName}] Listener2: caught $p"))

  Await.result(f, Duration.Inf)
}
