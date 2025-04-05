package com.peknight.method

import cats.data.EitherT
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import cats.{Monad, MonadError}
import com.peknight.error.Error
import com.peknight.error.syntax.applicativeError.asError
import com.peknight.error.syntax.either.asError

package object cascade:
  def fetch[F[_], E, A](sources: Source[F, E, A]*)(using MonadError[F, Throwable]): F[Either[Error, Option[A]]] =
    Monad[F].tailRecM[
      (List[Source[F, E, A]], List[A => F[Either[E, Unit]]], Option[Either[Error, Option[A]]]),
      Either[Error, Option[A]]
    ]((sources.toList, Nil, None)) {
      case (head :: tail, hydrates, temp) => head.read.asError.flatMap {
        _.flatMap(_.asError) match
          case Left(error) =>
            val next = temp.getOrElse(error.asLeft[Option[A]])
            if head.continueOnError(error) then
              if head.writeOnError(error) then
                (tail, head.write :: hydrates, next.some).asLeft.pure[F]
              else (tail, hydrates, next.some).asLeft.pure[F]
            else next.asRight.pure[F]
          case Right(None) => (tail, head.write :: hydrates, none[A].asRight[Error].some).asLeft.pure[F]
          case Right(Some(value)) =>
            hydrates.traverse(hydrate => EitherT(hydrate(value).asError.map(_.flatMap(_.asError))))
              .value.as(value.some.asRight[Error].asRight)
      }
      case (_, _, temp) => temp.getOrElse(none[A].asRight[Error]).asRight.pure[F]
    }
end cascade
