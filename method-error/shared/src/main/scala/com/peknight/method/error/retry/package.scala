package com.peknight.method.error

import cats.Monad
import cats.data.StateT
import cats.effect.{Async, Clock, GenTemporal}
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import com.peknight.error.Error
import com.peknight.error.syntax.applicativeError.asError
import com.peknight.error.syntax.either.asError
import com.peknight.method.retry.{Retry, RetryState}

import scala.concurrent.duration.FiniteDuration

package object retry:
  def retryS[F[_]: Async, A, B, S](fe: F[Either[A, B]])(f: (Either[Error, B], RetryState) => StateT[F, S, Retry])
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

  def retry[F[_]: Async, A, B](fe: F[Either[A, B]])(f: (Either[Error, B], RetryState) => F[Retry]): F[Either[Error, B]] =
    retryS[F, A, B, Unit](fe)((either, state) => StateT.liftF(f(either, state))).runA(())
end retry
