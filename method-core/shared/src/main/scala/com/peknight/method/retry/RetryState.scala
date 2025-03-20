package com.peknight.method.retry

import spire.math.Interval

import scala.concurrent.duration.FiniteDuration

case class RetryState(attempts: Int, start: FiniteDuration, now: FiniteDuration, intervals: List[Interval[FiniteDuration]])
