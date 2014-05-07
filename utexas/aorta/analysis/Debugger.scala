// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import scala.tools.nsc.{Interpreter, Settings}
import java.io.File
import jline.console.ConsoleReader

import utexas.aorta.sim.{Simulation, EV_Heartbeat}

class REPL() {
  protected val e = new Interpreter(new Settings())
  private val reader = new ConsoleReader()
  reader.setPrompt("> ")
  private var first_time = true

  protected def init() {
    first_time = false
    println("Initializing REPL, please wait a few seconds...")
  }

  protected def welcome() {
    println("Type 'quit' to exit the REPL.")
  }

  def run() {
    if (first_time) {
      init()
    }
    welcome()

    while (true) {
      val input = reader.readLine()
      if (input == "quit") {
        return
      }
      try {
        e.interpret(input)
      } catch {
        // Catches both syntax errors and actual in-simulation exceptions
        case e: Throwable => e.printStackTrace()
      }
    }
  }
}

class SimREPL(sim: Simulation) extends REPL {
  override def init() {
    super.init()
    e.bind("sim", "utexas.aorta.sim.Simulation", sim)
    e.interpret("import utexas.aorta.common._")
  }

  override def welcome() {
    super.welcome()
    println("The main simulation object is bound to 'sim'.")
  }
}

class REPLDebugger(sim: Simulation) {
  // When this file exists, launch the REPL
  private val signal = new File(".headless_repl")
  private val repl = new SimREPL(sim)

  sim.listen(classOf[EV_Heartbeat], _ match { case e: EV_Heartbeat => {
    if (signal.exists) {
      signal.delete()
      repl.run()
    }
  }})
}
