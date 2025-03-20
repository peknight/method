package com.peknight.method.error.syntax

import cats.data.StateT
import cats.effect.Async
import com.peknight.error.Error
import com.peknight.method.error.retry.{retry as _retry, retryS as _retryS}
import com.peknight.method.retry.{Retry, RetryState}

trait EitherFSyntax:
  extension [F[_], A, B] (fe: F[Either[A, B]])
    def retryS[S](f: (Either[Error, B], RetryState) => StateT[F, S, Retry])(using Async[F])
    : StateT[F, S, Either[Error, B]] =
      _retryS(fe)(f)
    def retry(f: (Either[Error, B], RetryState) => F[Retry])(using Async[F]): F[Either[Error, B]] =
      _retry(fe)(f)
  end extension
end EitherFSyntax
object EitherFSyntax extends EitherFSyntax
