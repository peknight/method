package com.peknight.method.retry

import cats.Monad
import cats.data.{EitherT, StateT}
import cats.effect.{Async, Clock, GenTemporal, Sync}
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import com.peknight.error.Error
import com.peknight.error.syntax.applicativeError.asError
import com.peknight.error.syntax.either.asError
import com.peknight.random.Random
import com.peknight.random.provider.RandomProvider
import com.peknight.random.state.between
import com.peknight.spire.ext.syntax.bound.{lower, upper}
import spire.math.*
import spire.math.interval.ValueBound
import spire.syntax.std.int.**

import scala.concurrent.duration.*

sealed trait Retry derives CanEqual
object Retry:
  case object Stop extends Retry
  case object Now extends Retry
  case class After(time: Duration) extends Retry

  def stop: Retry = Stop
  def now: Retry = Now
  def after(time: Duration): Retry = After(time)

  def state[F[_]: Async, A, B, S](fe: F[Either[A, B]])(f: (Either[Error, B], RetryState) => StateT[F, S, Retry])
  : StateT[F, S, Either[Error, B]] =
    val run: StateT[F, S, Either[Error, B]] = StateT.liftF(fe.asError.map(_.flatMap(_.asError)))
    val monotonic: StateT[F, S, FiniteDuration] = StateT.liftF(Clock[F].monotonic)
    val state =
      for
        startTime <- monotonic
        either <- Monad[[X] =>> StateT[F, S, X]].tailRecM[Int, Either[Error, B]](1) { attempts =>
          for
            either <- run
            now <- monotonic
            retry <- f(either, RetryState(attempts, startTime, now))
            _ <- retry match
              case Retry.After(time) => StateT.liftF(GenTemporal[F].sleep(time))
              case _ => StateT.pure[F, S, Unit](())
          yield
            retry match
              case Retry.Stop => either.asRight
              case _ => (attempts + 1).asLeft
        }
      yield
        either
    StateT[F, S, Either[Error, B]](s => state.run(s).asError.map {
      case Left(error) => (s, error.asLeft[B])
      case Right((s, either)) => (s, either)
    })

  def apply[F[_]: Async, A, B](fe: F[Either[A, B]])(f: (Either[Error, B], RetryState) => F[Retry]): F[Either[Error, B]] =
    state[F, A, B, Unit](fe)((either, state) => StateT.liftF(f(either, state))).runA(())

  def random[F[_]: {Async, RandomProvider}, A, B](fe: F[Either[A, B]])
                                                 (f: (Either[Error, B], RetryState) => StateT[F, Random[F], Retry])
  : F[Either[Error, B]] =
    val eitherT =
      for
        random <- EitherT(RandomProvider[F].random.asError)
        result <- EitherT(state[F, A, B, Random[F]](fe)(f).runA(random))
      yield
        result
    eitherT.value

  def retryRandom[F[_]: {Async, RandomProvider}, A, B](fe: F[Either[A, B]])
                                                      (maxAttempts: Option[Int] = Some(3),
                                                       interval: Option[FiniteDuration] = Some(1.second),
                                                       offset: Option[Interval[FiniteDuration]] = None,
                                                       timeout: Option[FiniteDuration] = None,
                                                       exponentialBackoff: Boolean = false)
                                                      (success: Either[Error, B] => Boolean): F[Either[Error, B]] =
    random(fe) {
      case (either, state) =>
        if success(either) then StateT.pure(stop)
        else if maxAttempts.exists(_ <= state.attempts) then StateT.pure(stop)
        else
          val base = if exponentialBackoff then interval.map(_ * (2 ** (state.attempts - 1))) else interval
          val (minOffset, maxOffset) = offsetInterval(offset)
          for
            sleep <-
              if minOffset < maxOffset then
                for
                  random <- StateT.get[F, Random[F]]
                  offset <- between[F](minOffset.toNanos, (maxOffset + 1.nano).toNanos).map(_.nanos)
                yield
                  base.map(_ + offset).getOrElse(offset).some
              else if minOffset === maxOffset then StateT.pure(base.map(_ + minOffset).getOrElse(minOffset).some)
              else StateT.pure(base)
          yield
            timeout match
              case Some(timeout) if state.now + sleep.filter(_ >= 0.second).getOrElse(0.second) - state.start > timeout =>
                stop
              case _ => sleep.filter(_ > 0.second).fold(now)(after)
    }

  private def offsetInterval(offset: Option[Interval[FiniteDuration]]): (FiniteDuration, FiniteDuration) =
    offset match
      case Some(interval) =>
        val (l, u) = (interval.lowerBound, interval.upperBound) match
          case (lower: ValueBound[FiniteDuration], upper: ValueBound[FiniteDuration]) => (lower.lower, upper.upper)
          case (lower: ValueBound[FiniteDuration], _) => (lower.lower, lower.lower max 0.second)
          case (_, upper: ValueBound[FiniteDuration]) => (upper.upper min 0.second, upper.upper)
          case _ => (0.second, 0.second)
        if l <= u then (l, u) else (0.second, 0.second)
      case _ => (0.second, 0.second)

end Retry