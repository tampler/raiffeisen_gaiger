package demo

import zio.{ App, Promise, Ref, Task, ZIO }
import zio.blocking.{ blocking, Blocking }
import zio.console.{ putStrLn, Console }
import Common._
import scala.util.Random

object Common {
  type Splash[A] = A => Unit

  final case class Particle(mass: Int, charge: Int)

  trait GeigerCounter[F[_], A] {
    def registerListener(listener: Splash[A], ref: Ref[Vector[Splash[A]]]): Unit
    def start(n: Int): F[Unit]
  }

  class GeigerZIOEff[A]() extends GeigerCounter[Task, A] {

    override def registerListener(listener: Splash[A], ref: zio.Ref[Vector[Splash[A]]]) = {
      ref.getAndUpdate(_ :+ listener)
      println(">>>>> ")
    }

    val env = Console.live ++ Blocking.live

    def newListener(n: Int, input: A) = println(s"[${Thread.currentThread().getName}] Listener$n: caught $input")

    def emit(num: Int): ZIO[zio.blocking.Blocking with zio.console.Console, Nothing, Particle] =
      for {
        _    <- blocking(ZIO.effectTotal(Thread.sleep(1000)))
        part = Particle(Random.between(0, 1000), Random.between(-1, 2))
        _    <- putStrLn(s"[${Thread.currentThread().getName}] Emitted $part")
      } yield part

    def work(listeners: Ref[Vector[Splash[A]]]) =
      for {
        p <- Promise.make[Nothing, Unit]
        // fibers <- /* p.succeed(()) *>
        // _      <- putStrLn(particles.toString)
        _ <- ZIO.effect(
              Range(0, 1).foreach(i => registerListener(p => newListener(i, p), listeners))
            )
        _ <- p.await
      } yield ()

    override def start(n: Int) =
      for {
        listeners <- Ref.make(Vector.empty[Splash[A]])
        particles <- ZIO.foreach(Range(0, n))(emit).provideLayer(env)
        _         <- ZIO.foreach(Range(0, 2))(_ => work(listeners))
      } yield ()

  }
}

object Main extends App {

  def run(args: List[String]) = prog.exitCode

  val gaiger = new GeigerZIOEff[Particle]()

  val prog = gaiger.start(2)
}
