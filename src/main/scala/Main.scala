package demo

import zio.{ App, Promise, Ref, Task, ZIO }
import zio.blocking.blocking
import zio.console.{ putStrLn, Console }
import Common._
import scala.util.Random
import scala.collection.immutable.Stream.Cons
import zio.ZLayer

object Common {
  type Splash[A] = A => Unit

  final case class Particle(mass: Int, charge: Int)

  trait GeigerCounter[F[_], A] {
    def registerListener(listener: Splash[A], ref: Ref[Vector[Splash[A]]]): Unit
    def start(n: Int): F[Unit]
  }

  class GeigerZIOEff[A]() extends GeigerCounter[Task, A] {

    override def registerListener(listener: Splash[A], ref: zio.Ref[Vector[Splash[A]]]): Unit =
      ref.update(_ :+ listener)

    def splashWork[A](item: Splash[A]) = ZIO.unit

    val env = Console.live

    def work[T](ref: Ref[Vector[Splash[T]]]) =
      for {
        promise <- Promise.make[Nothing, Unit]
        prt     = Particle(Random.between(0, 1000), Random.between(-1, 2))
        _       <- putStrLn(s"[${Thread.currentThread().getName}] Emitted $prt")
        _       <- ref.get.map(_.foreach(splashWork))
        _       <- promise.await
      } yield ()

    override def start(n: Int) =
      for {
        listeners <- Ref.make(Vector.empty[Splash[Particle]])
        go        <- ZIO.foreach(Range(0, n))(_ => work(listeners).provideLayer(env))
      } yield ()

  }

}

object Main extends App {

  def run(args: List[String]) =
    myAppLogic.exitCode

  val myAppLogic = ZIO.unit

  def registerListener(ref: Ref[Vector[Splash[Particle]]]): Unit = ???

  val prog = for {
    listeners <- Ref.make(Vector.empty[Splash[Particle]])
  } yield ()

}
