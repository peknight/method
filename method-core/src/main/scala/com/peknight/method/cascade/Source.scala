package com.peknight.method.cascade

import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.option.*
import cats.{Applicative, Show}
import com.peknight.error.Error

case class Source[F[_], A](
                            read: F[Either[Error, Option[A]]],
                            write: A => F[Either[Error, Unit]],
                            continueOnError: Error => Boolean = _ => false,
                            writeOnError: Error => Boolean = _ => true
                          )
object Source:
  def read[F[_]: Applicative, A](
                                  r: F[Either[Error, Option[A]]],
                                  continueOnError: Error => Boolean = _ => false,
                                  writeOnError: Error => Boolean = _ => true
                                ): Source[F, A] =
    Source(r, _ => ().asRight[Error].pure[F], continueOnError, writeOnError)
  def write[F[_]: Applicative, E, A](
                                      w: A => F[Either[Error, Unit]],
                                      continueOnError: Error => Boolean = _ => false,
                                      writeOnError: Error => Boolean = _ => true
                                    ): Source[F, A] =
    Source(none[A].asRight[Error].pure[F], w, continueOnError, writeOnError)

  given showSource[F[_], A]: Show[Source[F, A]] with
    def show(t: Source[F, A]): String = "Source(...)"
  end showSource
end Source
