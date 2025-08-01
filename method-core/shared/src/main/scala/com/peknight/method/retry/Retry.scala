package com.peknight.method.retry

import cats.data.{EitherT, StateT}
import cats.effect.{Async, Clock, GenTemporal}
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.{Functor, Monad, Show}
import com.peknight.error.Error
import com.peknight.error.syntax.applicativeError.{asError, faeLiftET}
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
  sealed trait Stop extends Retry
  case object Success extends Stop
  case class MaxAttempts(attempts: Int) extends Stop
  case class Timeout(sleep: FiniteDuration, overflow: FiniteDuration) extends Stop
  case object Now extends Retry
  case class After(time: Duration) extends Retry

  def stateT[F[_]: Async, S, A](fe: F[Either[Error, A]])(f: (Either[Error, A], RetryState) => StateT[F, S, Retry])
  : StateT[F, S, Either[Error, A]] =
    val run: StateT[F, S, Either[Error, A]] = StateT.liftF(fe)
    val monotonic: StateT[F, S, FiniteDuration] = StateT.liftF(Clock[F].monotonic)
    val state =
      for
        startTime <- monotonic
        either <- Monad[[X] =>> StateT[F, S, X]].tailRecM[Int, Either[Error, A]](1) { attempts =>
          for
            either <- run
            now <- monotonic
            retry <- f(either, RetryState(attempts, startTime, now))
            _ <- retry match
              case Retry.After(time) => StateT.liftF(GenTemporal[F].sleep(time))
              case _ => StateT.pure[F, S, Unit](())
          yield
            retry match
              case _: Retry.Stop => either.asRight
              case _ => (attempts + 1).asLeft
        }
      yield
        either
    StateT[F, S, Either[Error, A]](s => state.run(s).asError.map {
      case Left(error) => (s, error.asLeft[A])
      case Right((s, either)) => (s, either)
    })

  def state[F[_]: Async, S, A](fe: F[Either[Error, A]])(s: S)(f: (Either[Error, A], RetryState) => StateT[F, S, Retry])
  : F[Either[Error, A]] =
    stateT[F, S, A](fe)(f).runA(s)

  def stateless[F[_]: Async, A](fe: F[Either[Error, A]])(f: (Either[Error, A], RetryState) => F[Retry]): F[Either[Error, A]] =
    stateT[F, Unit, A](fe)((either, state) => StateT.liftF(f(either, state))).runA(())

  def random[F[_]: {Async, RandomProvider}, A](fe: F[Either[Error, A]])
                                              (f: (Either[Error, A], RetryState) => StateT[F, Random[F], Retry])
  : F[Either[Error, A]] =
    val eitherT =
      for
        random <- RandomProvider[F].random.faeLiftET
        result <- EitherT(stateT[F, Random[F], A](fe)(f).runA(random))
      yield
        result
    eitherT.value

  def retryRandom[F[_] : {Async, RandomProvider}, A](fe: F[Either[Error, A]])
                                                    (maxAttempts: Option[Int] = Some(3),
                                                     timeout: Option[FiniteDuration] = None,
                                                     interval: Option[FiniteDuration] = Some(1.second),
                                                     offset: Option[Interval[FiniteDuration]] = None,
                                                     exponentialBackoff: Boolean = false)
                                                    (success: Either[Error, A] => Boolean)
                                                    (effect: (Either[Error, A], RetryState, Retry) => F[Unit])
  : F[Either[Error, A]] =
    random(fe)(randomOffset(maxAttempts, timeout, interval, offset, exponentialBackoff)(success)(effect))

  def retry[F[_]: Async, A](fe: F[Either[Error, A]])
                           (maxAttempts: Option[Int] = Some(3),
                            timeout: Option[FiniteDuration] = None,
                            interval: Option[FiniteDuration] = Some(1.second),
                            offset: Option[FiniteDuration] = None,
                            exponentialBackoff: Boolean = false)
                           (success: Either[Error, A] => Boolean)
                           (effect: (Either[Error, A], RetryState, Retry) => F[Unit])
  : F[Either[Error, A]] =
    stateless[F, A](fe)(fixedOffset(maxAttempts, timeout, interval, offset, exponentialBackoff)(success)(effect))

  def handleInterval[F[_]: Functor, S](state: RetryState, timeout: Option[FiniteDuration] = None,
                                       interval: Option[FiniteDuration] = Some(1.second),
                                       exponentialBackoff: Boolean = false)
                                      (offsetS: StateT[F, S, Option[FiniteDuration]])
  : StateT[F, S, Retry] =
    val base = if exponentialBackoff then interval.map(_ * (2 ** (state.attempts - 1))) else interval
    offsetS.map { offset =>
      val sleep = (base, offset) match
        case (Some(base), Some(offset)) => base + offset
        case (Some(base), _) => base
        case (_, Some(offset)) => offset
        case _ => 0.nano
      val sleepTime = if sleep > 0.nano then sleep else 0.nano
      timeout match
        case Some(timeout) =>
          val overflow = (state.now + sleepTime) - (state.start + timeout)
          if overflow > 0.nano then Timeout(sleepTime, overflow)
          else if sleepTime > 0.nano then After(sleepTime)
          else Now
        case _ => if sleepTime > 0.nano then After(sleepTime) else Now
    }

  private def handleState[F[_]: Monad, S, A](maxAttempts: Option[Int] = Some(3),
                                             timeout: Option[FiniteDuration] = None,
                                             interval: Option[FiniteDuration] = Some(1.second),
                                             exponentialBackoff: Boolean = false)
                                            (success: A => Boolean)
                                            (effect: (A, RetryState, Retry) => F[Unit])
                                            (offsetS: StateT[F, S, Option[FiniteDuration]])
  : (A, RetryState) => StateT[F, S, Retry] =
    (value, state) =>
      val stateT =
        if success(value) then StateT.pure(Success)
        else if maxAttempts.exists(_ <= state.attempts) then StateT.pure(MaxAttempts(state.attempts))
        else handleInterval[F, S](state, timeout, interval, exponentialBackoff)(offsetS)
      stateT.flatMap(retry => StateT.liftF(effect(value, state, retry)).as(retry))

  private def randomOffset[F[_]: Monad, A](maxAttempts: Option[Int] = Some(3),
                                           timeout: Option[FiniteDuration] = None,
                                           interval: Option[FiniteDuration] = Some(1.second),
                                           offset: Option[Interval[FiniteDuration]] = None,
                                           exponentialBackoff: Boolean = false)
                                          (success: A => Boolean)
                                          (effect: (A, RetryState, Retry) => F[Unit])
  : (A, RetryState) => StateT[F, Random[F], Retry] =
    handleState(maxAttempts, timeout, interval, exponentialBackoff)(success)(effect) {
      val (minOffset, maxOffset) = offsetInterval(offset)
      if minOffset < maxOffset then
        between[F](minOffset.toNanos, (maxOffset + 1.nano).toNanos).map(_.nanos.some)
      else if minOffset === maxOffset then StateT.pure(minOffset.some)
      else StateT.pure(none[FiniteDuration])
    }

  private def offsetInterval(offset: Option[Interval[FiniteDuration]]): (FiniteDuration, FiniteDuration) =
    offset match
      case Some(interval) =>
        val (l, u) = (interval.lowerBound, interval.upperBound) match
          case (lower: ValueBound[FiniteDuration], upper: ValueBound[FiniteDuration]) => (lower.lower, upper.upper)
          case (lower: ValueBound[FiniteDuration], _) => (lower.lower, lower.lower max 0.nano)
          case (_, upper: ValueBound[FiniteDuration]) => (upper.upper min 0.nano, upper.upper)
          case _ => (0.nano, 0.nano)
        if l <= u then (l, u) else (0.nano, 0.nano)
      case _ => (0.nano, 0.nano)

  private def fixedOffset[F[_] : Monad, A](maxAttempts: Option[Int] = Some(3),
                                           timeout: Option[FiniteDuration] = None,
                                           interval: Option[FiniteDuration] = Some(1.second),
                                           offset: Option[FiniteDuration] = None,
                                           exponentialBackoff: Boolean = false)
                                          (success: A => Boolean)
                                          (effect: (A, RetryState, Retry) => F[Unit])
  : (A, RetryState) => F[Retry] =
    (value, state) => handleState[F, Unit, A](maxAttempts, timeout, interval, exponentialBackoff)(success)(effect)(
      StateT.pure(offset)
    ).apply(value, state).runA(())

  given showRetry: Show[Retry] = Show.fromToString[Retry]
end Retry