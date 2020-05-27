package demo

import zio.{ App, Chunk, Task, ZIO }
import zio.blocking.{ blocking, Blocking }
import zio.console.{ putStrLn, Console }
import Common._
import scala.util.Random

object Common {
  type Splash[A] = A => Unit

  final case class Particle(mass: Int, charge: Int)

  trait GeigerCounter[F[_], A] {
    def registerListener(listener: Splash[A]): Unit
    def start(n: Int, m: Int): F[Unit]
  }

  class GeigerZIOEff() extends GeigerCounter[Task, Particle] {

    override def registerListener(listener: Splash[Particle]) = ()

    private def listener(n: Int, input: Particle) =
      println(s"[${Thread.currentThread().getName}] Listener$n: caught $input")

    private val env = Console.live ++ Blocking.live

    private def emitEff(num: Int): ZIO[zio.blocking.Blocking with zio.console.Console, Nothing, Particle] =
      for {
        _    <- blocking(ZIO.effectTotal(Thread.sleep(1000)))
        part = Particle(Random.between(0, 1000), Random.between(-1, 2))
        _    <- putStrLn(s"[${Thread.currentThread().getName}] Emitted $part")
      } yield part

    private def startEff(
      totalParticles: Int,
      totalSensors: Int
    ): ZIO[zio.blocking.Blocking with zio.console.Console, Throwable, Unit] =
      for {
        particles <- ZIO.foreachParN(totalParticles)(Range(0, totalParticles))(emitEff)
        sensors   = Chunk.fromIterable(Range(0, totalSensors).map(i => listener(i, _)))
        _         <- sensors.mapMPar(v => ZIO.effect(particles.foreach(v)))
      } yield ()

    override def start(n: Int, m: Int) = startEff(n, m).provideLayer(env)
  }
}

object Main extends App {

  def run(args: List[String]) = prog.exitCode

  val gaiger = new GeigerZIOEff()

  val prog = gaiger.start(2, 2)
}
