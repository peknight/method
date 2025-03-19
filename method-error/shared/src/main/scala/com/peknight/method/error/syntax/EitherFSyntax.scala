package com.peknight.method.error.syntax

import cats.Monad
import cats.effect.{Async, Clock, GenTemporal}
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import com.peknight.error.Error
import com.peknight.error.syntax.applicativeError.asError
import com.peknight.error.syntax.either.asError
import com.peknight.method.retry.{Retry, RetryState}

trait EitherFSyntax:
  extension [F[_], A, B] (fe: F[Either[A, B]])
    def retry(f: (Either[Error, B], RetryState) => F[Retry])(using Async[F]): F[Either[Error, B]] =
      val flatten: F[Either[Error, B]] = fe.asError.map(_.flatMap(_.asError))
      Clock[F].monotonic.flatMap { startTime =>
        Monad[F].tailRecM[RetryState, Either[Error, B]](RetryState(0, startTime, startTime)) { retryState =>
          for
            either <- flatten
            now <- Clock[F].monotonic
            nextState = retryState.copy(attempts = retryState.attempts + 1, now = now)
            retry <- f(either, nextState)
            _ <- retry match
              case Retry.After(time) => GenTemporal[F].sleep(time)
              case _ => ().pure[F]
          yield
            retry match
              case Retry.Stop => either.asRight
              case _ => nextState.asLeft
        }
      }
  end extension
end EitherFSyntax
