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

    override def registerListener(listener: Splash[A], ref: zio.Ref[Vector[Splash[A]]]) =
      println(">>>>> ")
    // ref.getAndUpdate(_ :+ listener)

    val env = Console.live ++ Blocking.live

    def work(ref: Ref[Vector[Splash[A]]]) =
      for {
        promise <- Promise.make[Nothing, Unit]
        _       <- blocking(ZIO.effectTotal(Thread.sleep(1000))).fork
        prt     = Particle(Random.between(0, 1000), Random.between(-1, 2))
        _       <- putStrLn(s"[${Thread.currentThread().getName}] Emitted $prt")
        _       <- ref.get.map(_.foreach(listener => registerListener(listener, ref)))
        _       <- promise.await
      } yield ()

    override def start(n: Int) =
      for {
        listeners <- Ref.make(Vector.empty[Splash[A]])
        _ = Range(0, 1).foreach(v =>
          listeners.update(_ :+ (p => println(s"[${Thread.currentThread().getName}] Listener$v: caught $p")))
        )
        _ <- ZIO.foreach(Range(0, n))(_ => work(listeners).provideLayer(env))
      } yield ()

    val list = List.fill(2)(println)
  }
}

object Main extends App {

  def run(args: List[String]) = prog.exitCode

  def registerListener(ref: Ref[Vector[Splash[Particle]]]): Unit = ???

  val gaiger = new GeigerZIOEff[Particle]()

  val prog = gaiger.start(15)
}
