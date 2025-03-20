package com.peknight.method.retry

import scala.concurrent.duration.FiniteDuration

case class RetryState(attempts: Int, start: FiniteDuration, now: FiniteDuration)
