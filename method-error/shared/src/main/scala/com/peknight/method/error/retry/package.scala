package com.peknight.method.error

import cats.Applicative
import cats.syntax.applicative.*
import com.peknight.method.retry.{Retry, RetryState}

package object retry:
  def attempts[F[_]: Applicative, A](attempts: Int): (Either[Error, A], RetryState) => F[Retry] =
    case (Right(a), _) => Retry.stop.pure
    case (_, state) if state.attempts >= attempts => Retry.stop.pure
    case _ => ???
end retry
