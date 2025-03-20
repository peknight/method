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
import spire.math.Interval

import scala.concurrent.duration.FiniteDuration

trait EitherFSyntax:
  extension [F[_], A, B] (fe: F[Either[A, B]])
    def retry(f: (Either[Error, B], RetryState) => F[Retry])(using Async[F]): F[Either[Error, B]] =
      val flatten: F[Either[Error, B]] = fe.asError.map(_.flatMap(_.asError))
      Clock[F].monotonic.flatMap { startTime =>
        Monad[F].tailRecM[(Int, List[Interval[FiniteDuration]]), Either[Error, B]]((0, Nil)) { (attempts, intervals) =>
          for
            start <- Clock[F].monotonic
            either <- flatten
            end <- Clock[F].monotonic
            nextAttempts = attempts + 1
            nextIntervals = Interval.closed(start, end) :: intervals
            retry <- f(either, RetryState(nextAttempts, startTime, end, nextIntervals))
            _ <- retry match
              case Retry.After(time) => GenTemporal[F].sleep(time)
              case _ => ().pure[F]
          yield
            retry match
              case Retry.Stop => either.asRight
              case _ => (nextAttempts, nextIntervals).asLeft
        }
      }.asError.map(_.flatten)
  end extension
end EitherFSyntax
