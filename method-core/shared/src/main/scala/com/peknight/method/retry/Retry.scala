package com.peknight.method.retry

import scala.concurrent.duration.Duration

sealed trait Retry derives CanEqual
object Retry:
  case object Stop extends Retry
  case object Now extends Retry
  case class After(time: Duration) extends Retry

  def stop: Retry = Stop
  def now: Retry = Now
  def after(time: Duration): Retry = After(time)
end Retry