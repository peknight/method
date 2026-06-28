package com.peknight.method.syntax

import cats.data.StateT
import cats.effect.Async
import com.peknight.error.Error
import com.peknight.method.retry.{Retry, RetryState}
import com.peknight.random.Random
import com.peknight.random.provider.RandomProvider
import spire.math.Interval

import scala.concurrent.duration.*

trait EitherFSyntax:
  extension [F[_], A](fe: F[Either[Error, A]])
    def retryStateT[S](f: (Either[Error, A], RetryState) => StateT[F, S, Retry])(using Async[F])
    : StateT[F, S, Either[Error, A]] =
      Retry.stateT(fe)(f)
    def retryState[S](s: S)(f: (Either[Error, A], RetryState) => StateT[F, S, Retry])(using Async[F])
    : F[Either[Error, A]] =
      Retry.state(fe)(s)(f)
    def retryStateless(f: (Either[Error, A], RetryState) => F[Retry])(using Async[F]): F[Either[Error, A]] =
      Retry.stateless(fe)(f)
    def retryRandomState(f: (Either[Error, A], RetryState) => StateT[F, Random[F], Retry])(using Async[F], RandomProvider[F])
    : F[Either[Error, A]] =
      Retry.random(fe)(f)
    def retryRandom(maxAttempts: Option[Int] = Some(3),
                    timeout: Option[FiniteDuration] = None,
                    interval: Option[FiniteDuration] = Some(1.second),
                    offset: Option[Interval[FiniteDuration]] = None,
                    exponentialBackoff: Boolean = false)
                   (success: Either[Error, A] => Boolean)
                   (effect: (Either[Error, A], RetryState, Retry) => F[Unit])
                   (using Async[F], RandomProvider[F]): F[Either[Error, A]] =
      Retry.retryRandom(fe)(maxAttempts, timeout, interval, offset, exponentialBackoff)(success)(effect)
    def retry(maxAttempts: Option[Int] = Some(3),
              timeout: Option[FiniteDuration] = None,
              interval: Option[FiniteDuration] = Some(1.second),
              offset: Option[FiniteDuration] = None,
              exponentialBackoff: Boolean = false)
             (success: Either[Error, A] => Boolean)
             (effect: (Either[Error, A], RetryState, Retry) => F[Unit])
             (using Async[F]): F[Either[Error, A]] =
      Retry.retry(fe)(maxAttempts, timeout, interval, offset, exponentialBackoff)(success)(effect)
  end extension
end EitherFSyntax
object EitherFSyntax extends EitherFSyntax
