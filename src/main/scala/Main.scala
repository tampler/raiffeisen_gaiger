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
    def registerListener(listener: Splash[A]): Unit
    def start(n: Int): F[Unit]
  }

  class GeigerZIOEff() extends GeigerCounter[Task, Particle] {

    override def registerListener(listener: Splash[Particle]) = ()

    val env = Console.live ++ Blocking.live

    def listener(n: Int, input: Particle) = println(s"[${Thread.currentThread().getName}] Listener$n: caught $input")

    val list0 = (p: Particle) => listener(0, p)
    val list1 = (p: Particle) => listener(1, p)

    def emit(num: Int): ZIO[zio.blocking.Blocking with zio.console.Console, Nothing, Particle] =
      for {
        _    <- blocking(ZIO.effectTotal(Thread.sleep(1000)))
        part = Particle(Random.between(0, 1000), Random.between(-1, 2))
        _    <- putStrLn(s"[${Thread.currentThread().getName}] Emitted $part")
      } yield part

    // def regEff = ZIO.effect(registerListener(???))

    def startEff(n: Int) =
      for {
        // p         <- Promise.make[Nothing, Unit]
        listeners <- Ref.make(Vector.empty[Splash[Particle]])
        _         <- listeners.update(_ :+ list0)
        _         <- listeners.update(_ :+ list1)
        particles <- ZIO.foreach(Range(0, n))(emit)
        _         <- listeners.get.map(_.foreach(v => v(particles.head)))
        // _ <- p.await
      } yield ()

    override def start(n: Int) = startEff(n).provideLayer(env)

  }
}

object Main extends App {

  def run(args: List[String]) = prog.exitCode

  val gaiger = new GeigerZIOEff()

  val prog = gaiger.start(2)
}
