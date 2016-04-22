package it.unive.dais.dapa.utils

import java.util.Date

/**
  * Created by esteffin on 04/03/16.
  */
object profiling {

  class Stopwatch() {
    private var start_time: Date = new Date()
    private var running: Boolean = false
    private var actual_elapsed: Date = new Date(0)
    private def update =
      if (running)
        actual_elapsed = new Date(new Date().getTime() - start_time.getTime())

    // Total elapsed time
    def elapsed = {
      update
      actual_elapsed
    }

    // Total elapsed time in milliseconds
    def elapsedMilliseconds = elapsed.getTime()

    def isRunning = running

	// stop measuring and set elapsed time to zero
    def reset() = stop(); actual_elapsed = new Date(0)

	// stop measuring, set elapsed time to zero and restart measuring
    def restart() = reset(); start()

	// start/restart measruing time
    def start() = running = true; start_time = new Date(); update

	// stop measuring elapsed time for an interval
    def stop() = update; running = false

  }
  object Stopwatch {
    def startNew() = {
      val res = new Stopwatch()
      res.start()
      res
    }
  }

  def execute_time[A](f: Unit => A): (A, Long) = {
    val chrono = new Stopwatch()
    chrono.start()
    val res = f(())
    chrono.stop()
    (res, chrono.elapsedMilliseconds)
  }

  def execute_print_time[A](caption: String)(f: Unit => A): A = {
    val chrono = new Stopwatch()
    chrono.start()
    val res = f(())
    chrono.stop()
    println("%s done in %s milliseconds..." format (caption, chrono.elapsedMilliseconds))
    res
  }
}

