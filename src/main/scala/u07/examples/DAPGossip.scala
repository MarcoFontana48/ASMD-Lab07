package u07.examples

import java.util.Random
import u07.modelling.{CTMC, CTMCSimulation, DAP, DAPGrid}
import u07.modelling.CTMCSimulation.*
import u07.utils.{Grids, MSet}

object DAPGossip:
  enum Place:
    case A, B, C
  type ID = (Int, Int)
  export Place.*
  export u07.modelling.DAP.*
  export u07.modelling.DAPGrid.*
  export u07.modelling.CTMCSimulation.*

  private val gossipRules = DAP[Place](
    // same as slide 38 of "07 - Stochastic Modelling"
    // a|a --100_000--> a
    Rule(MSet(A, A), m => 100_000, MSet(A), MSet()),
    // a|b --1--> b (erases tokens "a" where "b" occurs)
    Rule(MSet(A, B), m => 1, MSet(B), MSet()),
    // a --1--> a|ā (spreads tokens "a" into neighbors)
    Rule(MSet(A), m => 1, MSet(A), MSet(A)),

    // bidirectional communication (same as before but when message A reaches target C a reply B is generated)
    // when message A reaches target C, generate reply B
    Rule(MSet(A, C), m => 1, MSet(C, B), MSet()),
    // reply message B spreads back (same spreading pattern as A) b --1--> b|b̅ (spreads tokens "b" into neighbors)
    Rule(MSet(B), m => 1, MSet(B), MSet(B)),
    // b|b --100_000--> b
    Rule(MSet(B, B), m => 100_000, MSet(B), MSet())
  )
  val gossipCTMC: CTMC[State[(Int, Int), Place]] = DAP.toCTMC[ID, Place](gossipRules)
  private val net = Grids.createRectangularGrid(3, 3)
  val state: State[(Int, Int), Place] = State[ID, Place](
    MSet(
      Token((0, 0), A),  // message from sender
      Token((2, 2), C)   // target
    ),
    MSet(),
    net
  )

@main def mainDAPGossip(): Unit =
  import DAPGossip.*
  gossipCTMC.newSimulationTrace(state, new Random).take(150).toList.foreach: step =>
    println(step._1)
    println("spreading of sender message A")
    println(DAPGrid.simpleGridStateToString[Place](step._2, A))
    println("spreading of receiver C reply message B")
    println(DAPGrid.simpleGridStateToString[Place](step._2, B))