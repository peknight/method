package com.peknight.method.retry

import cats.effect.Sync
import cats.effect.std.Random
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import spire.math.Bounded
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

  def apply[F[_]: Sync, A](maxAttempts: Option[Int] = Some(3),
                           interval: Option[FiniteDuration] = Some(1.second),
                           offset: Option[Bounded[FiniteDuration]] = None,
                           timeout: Option[FiniteDuration] = None,
                           exponentialBackoff: Boolean = false
                          )(success: A => Boolean): (A, RetryState) => F[Retry] =
    (a, state) =>
      if success(a) then Retry.stop.pure[F]
      else if maxAttempts.exists(_ <= state.attempts) then Retry.stop.pure[F]
      else
        val base = if exponentialBackoff then interval.map(_ * (2 ** (state.attempts - 1))) else interval
        for
          sleep <- offset match
            case Some(offset) =>
              for
                random <- Random.scalaUtilRandom[F]
                offset <- random.betweenLong(offset.lower.toNanos, offset.upper.toNanos).map(_.nanos)
              yield
                base.map(_ + offset).getOrElse(offset).some
            case _ => base.pure[F]
        yield
          timeout match
            case Some(timeout) if state.now + sleep.filter(_ >= 0.second).getOrElse(0.second) - state.start > timeout =>
              Retry.stop
            case _ => sleep.filter(_ > 0.second).fold(Retry.now)(Retry.after)
end Retry