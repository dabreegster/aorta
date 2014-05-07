// AORTA is copyright (C) 2013 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import utexas.aorta.sim.{Simulation, EV_Step}
import utexas.aorta.common.{StateReader, StateWriter, Util, AgentID, cfg}

import java.io.{File, EOFException}

// Record what every agent is doing every few timesteps, so that when we
// re-simulate something with slight tweaks, we can detect exactly who and what
// starts deviating. This can be used for
// 1) testing if a code change actually messes with anything
// 2) quickly determining if a change helps/hurts drivers
object ReplayChecker {
  // Encode agents who don't exist like this
  val MISSING = -1.0
}

// Record what happens
class ReplayWriter(sim: Simulation, writer: StateWriter) {
  Util.log(s"Recording this run to compare to another later")
  private val num_agents = sim.scenario.agents.size

  sim.listen(classOf[EV_Step], _ match {
    case EV_Step(tick) if tick % cfg.replay_freq == 0 => {
      // We get agents in sorted order, so we can detect the ones who aren't around
      var expect_id = 0
      for (a <- sim.agents) {
        // Fill in gaps
        for (id <- Range(expect_id, a.id.int)) {
          writer.double(ReplayChecker.MISSING)
        }
        writer.double(a.characterize_choice)
        expect_id = a.id.int + 1
      }
      // Handle the tail
      for (id <- Range(expect_id, num_agents)) {
        writer.double(ReplayChecker.MISSING)
      }
    }
    case _ =>
  })
}

// Check what's happening now against what happened previously
class ReplayReader(sim: Simulation, reader: StateReader) {
  Util.log(s"Comparing this run against a previous")
  private val num_agents = sim.scenario.agents.size
  private var active = true

  sim.listen(classOf[EV_Step], _ match {
    case EV_Step(tick) if active && tick % cfg.replay_freq == 0 => {
      var expect_id = 0
      try {
        for (a <- sim.agents) {
          for (id <- Range(expect_id, a.id.int)) {
            val expected = reader.double
            if (expected != ReplayChecker.MISSING) {
              difference(new AgentID(id), expected, ReplayChecker.MISSING)
            }
          }
          val expected = reader.double
          if (expected != a.characterize_choice) {
            difference(a.id, expected, a.characterize_choice)
          }
          expect_id = a.id.int + 1
        }
        for (id <- Range(expect_id, num_agents)) {
          val expected = reader.double
          if (expected != ReplayChecker.MISSING) {
            difference(new AgentID(id), expected, ReplayChecker.MISSING)
          }
        }
      } catch {
        case _: EOFException => {
          Util.log(s"Nothing more to replay at $tick. Ignoring differences.")
          active = false
        }
      }
    }
    case _ =>
  })

  // TODO subclasses for diff behavior, or bcast an event.
  private var no_diff = true
  private def difference(a: AgentID, expected: Double, actual: Double) {
    if (no_diff) {
      Util.log(s"***** simulation differs from prior run! $a did $actual, not $expected")
      no_diff = false
    }
  }
}
