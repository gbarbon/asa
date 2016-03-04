package it.unive.dais.dapa.utils

/*import java.util.Date

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

    /** Ottiene il tempo totale trascorso.**/
    def elapsed = {
      update
      actual_elapsed
    }

    /**Ottiene il tempo totale trascorso in millisecondi.**/
    def elapsedMilliseconds = elapsed.getTime()

    def isRunning = running

    /**Interrompe la misurazione e imposta il tempo trascorso su zero.**/
    def reset() = stop(); actual_elapsed = new Date(0)

    /**Arresta la misurazione del tempo, azzera il tempo trascorso e riavvia la misurazione.**/
    def restart() = reset(); start()
    /**Avvia o riprende la misurazione del tempo.**/
    def start() = running = true; start_time = new Date(); update

    /**Interrompe la misurazione del tempo trascorso per un intervallo.**/
    def stop() = update; running = false

  }
  object Stopwatch {
    def startNew() = {
      val res = new Stopwatch()
      res.start()
      res
    }
  }

  /*def execute_time[A](f: Unit => A): (A, Long) = {
    val chrono = new Stopwatch()
    chrono.start()
    val res = f()
    chrono.stop()
    (res, chrono.elapsedMilliseconds)
  }*/

  def execute_print_time[A](caption: String)(f: Unit => A): A = {
    val chrono = new Stopwatch()
    chrono.start()
    val res = f()
    chrono.stop()
    println("%s done in %s milliseconds..." format (caption, chrono.elapsedMilliseconds))
    res
  }
}
*/
