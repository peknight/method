package com.peknight.method.cascade

import cats.Applicative
import cats.syntax.option.*
import cats.syntax.either.*
import cats.syntax.applicative.*
import com.peknight.error.Error

case class Source[F[_], E, A](
                               read: F[Either[E, Option[A]]],
                               write: A => F[Either[E, Unit]],
                               continueOnError: Error => Boolean = _ => true,
                               writeOnError: Error => Boolean = _ => true,
                             )
object Source:
  def read[F[_]: Applicative, E, A](
                                     r: F[Either[E, Option[A]]],
                                     continueOnError: Error => Boolean = _ => true,
                                     writeOnError: Error => Boolean = _ => true
                                   ): Source[F, E, A] =
    Source(r, _ => ().asRight[E].pure[F], continueOnError, writeOnError)
  def write[F[_]: Applicative, E, A](
                                     w: A => F[Either[E, Unit]],
                                     continueOnError: Error => Boolean = _ => true,
                                     writeOnError: Error => Boolean = _ => true
                                   ): Source[F, E, A] =
    Source(none[A].asRight[E].pure[F], w, continueOnError, writeOnError)
end Source
