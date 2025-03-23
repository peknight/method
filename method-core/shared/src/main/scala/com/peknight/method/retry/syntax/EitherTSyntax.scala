package com.peknight.method.retry.syntax

import cats.data.{EitherT, StateT}
import cats.effect.Async
import com.peknight.error.Error
import com.peknight.method.retry.{Retry, RetryState}
import com.peknight.random.Random
import com.peknight.random.provider.RandomProvider
import spire.math.Interval

import scala.concurrent.duration.*

trait EitherTSyntax:
  extension [F[_], A, B] (eitherT: EitherT[F, A, B])
    def state[S](f: (Either[Error, B], RetryState) => StateT[F, S, Retry])(using Async[F])
    : StateT[F, S, Either[Error, B]] =
      Retry.state(eitherT.value)(f)
    def stateless(f: (Either[Error, B], RetryState) => F[Retry])(using Async[F]): EitherT[F, Error, B] =
      EitherT(Retry.stateless(eitherT.value)(f))
    def random(f: (Either[Error, B], RetryState) => StateT[F, Random[F], Retry])(using Async[F], RandomProvider[F])
    : EitherT[F, Error, B] =
      EitherT(Retry.random(eitherT.value)(f))
    def retryRandom(maxAttempts: Option[Int] = Some(3),
                    timeout: Option[FiniteDuration] = None,
                    interval: Option[FiniteDuration] = Some(1.second),
                    offset: Option[Interval[FiniteDuration]] = None,
                    exponentialBackoff: Boolean = false)
                   (success: Either[Error, B] => Boolean)
                   (effect: (Either[Error, B], RetryState, Retry) => F[Unit])
                   (using Async[F], RandomProvider[F]): EitherT[F, Error, B] =
      EitherT(
        Retry.retryRandom(eitherT.value)(maxAttempts, timeout, interval, offset, exponentialBackoff)(success)(effect)
      )
    def retry(maxAttempts: Option[Int] = Some(3),
              timeout: Option[FiniteDuration] = None,
              interval: Option[FiniteDuration] = Some(1.second),
              offset: Option[FiniteDuration] = None,
              exponentialBackoff: Boolean = false)
             (success: Either[Error, B] => Boolean)
             (effect: (Either[Error, B], RetryState, Retry) => F[Unit])
             (using Async[F]): EitherT[F, Error, B] =
      EitherT(
        Retry.retry(eitherT.value)(maxAttempts, timeout, interval, offset, exponentialBackoff)(success)(effect)
      )
  end extension
end EitherTSyntax
object EitherTSyntax extends EitherTSyntax
