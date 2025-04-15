package scala.u07.examples

import u07.modelling.SPN
import u07.utils.MSet

import java.util.Random

object StochasticReadersAndWriters extends App:
  enum Place:
    case p1, p2, p3, p4, p5, p6, p7

  private val stochasticRW = SPN[Place](
    Trn(MSet(p1), m => 1.0, MSet(p2), MSet()),
    Trn(MSet(p2), m => 200_000.0, MSet(p3), MSet()),
    Trn(MSet(p2), m => 100_000.0, MSet(p4), MSet()),
    Trn(MSet(p3, p5), m => 100_000.0, MSet(p6, p5), MSet()),
    Trn(MSet(p4, p5), m => 100_000.0, MSet(p7), MSet(p6)),
    Trn(MSet(p6), m => m(p6) * 0.1, MSet(p1), MSet()),
    Trn(MSet(p7), m => 0.2, MSet(p1, p5), MSet()),
  )

  export Place.*
  export u07.modelling.CTMCSimulation.*
  export u07.modelling.SPN.*

  println:
    toCTMC(stochasticRW).newSimulationTrace(MSet(p1, p5), new Random)
      .take(20)
      .toList.mkString("\n")