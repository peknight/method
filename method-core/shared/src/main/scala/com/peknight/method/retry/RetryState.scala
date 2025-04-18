package com.peknight.method.retry

import cats.Show

import scala.concurrent.duration.FiniteDuration

case class RetryState(attempts: Int, start: FiniteDuration, now: FiniteDuration)
object RetryState:
  given showRetryState: Show[RetryState] = Show.fromToString[RetryState]
end RetryState

