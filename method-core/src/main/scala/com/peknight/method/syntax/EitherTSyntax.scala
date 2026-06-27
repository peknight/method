package com.peknight.method.syntax

import cats.data.{EitherT, StateT}
import cats.effect.Async
import com.peknight.error.Error
import com.peknight.method.retry.{Retry, RetryState}
import com.peknight.random.Random
import com.peknight.random.provider.RandomProvider
import spire.math.Interval

import scala.concurrent.duration.*

trait EitherTSyntax:
  extension [F[_], A](eitherT: EitherT[F, Error, A])
    def retryStateT[S](f: (Either[Error, A], RetryState) => StateT[F, S, Retry])(using Async[F])
    : StateT[F, S, Either[Error, A]] =
      Retry.stateT(eitherT.value)(f)
    def retryState[S](s: S)(f: (Either[Error, A], RetryState) => StateT[F, S, Retry])(using Async[F])
    : EitherT[F, Error, A] =
      EitherT(Retry.state(eitherT.value)(s)(f))
    def retryStateless(f: (Either[Error, A], RetryState) => F[Retry])(using Async[F]): EitherT[F, Error, A] =
      EitherT(Retry.stateless(eitherT.value)(f))
    def retryRandomState(f: (Either[Error, A], RetryState) => StateT[F, Random[F], Retry])(using Async[F], RandomProvider[F])
    : EitherT[F, Error, A] =
      EitherT(Retry.random(eitherT.value)(f))
    def retryRandom(maxAttempts: Option[Int] = Some(3),
                    timeout: Option[FiniteDuration] = None,
                    interval: Option[FiniteDuration] = Some(1.second),
                    offset: Option[Interval[FiniteDuration]] = None,
                    exponentialBackoff: Boolean = false)
                   (success: Either[Error, A] => Boolean)
                   (effect: (Either[Error, A], RetryState, Retry) => F[Unit])
                   (using Async[F], RandomProvider[F]): EitherT[F, Error, A] =
      EitherT(
        Retry.retryRandom(eitherT.value)(maxAttempts, timeout, interval, offset, exponentialBackoff)(success)(effect)
      )
    def retry(maxAttempts: Option[Int] = Some(3),
              timeout: Option[FiniteDuration] = None,
              interval: Option[FiniteDuration] = Some(1.second),
              offset: Option[FiniteDuration] = None,
              exponentialBackoff: Boolean = false)
             (success: Either[Error, A] => Boolean)
             (effect: (Either[Error, A], RetryState, Retry) => F[Unit])
             (using Async[F]): EitherT[F, Error, A] =
      EitherT(
        Retry.retry(eitherT.value)(maxAttempts, timeout, interval, offset, exponentialBackoff)(success)(effect)
      )
  end extension
end EitherTSyntax
object EitherTSyntax extends EitherTSyntax
