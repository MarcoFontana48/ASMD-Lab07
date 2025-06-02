# Operational tasks
## Stochastic readers & writers
Implementazione della rete come da slide
```scala 3
  private val stochasticRW = SPN[Place](
    Trn(MSet(p1), m => 1.0, MSet(p2), MSet()),
    Trn(MSet(p2), m => 200_000.0, MSet(p3), MSet()),
    Trn(MSet(p2), m => 100_000.0, MSet(p4), MSet()),
    Trn(MSet(p3, p5), m => 100_000.0, MSet(p6, p5), MSet()),
    Trn(MSet(p4, p5), m => 100_000.0, MSet(p7), MSet(p6)),
    Trn(MSet(p6), m => m(p6) * 0.1, MSet(p1), MSet()),
    Trn(MSet(p7), m => 0.2, MSet(p1, p5), MSet()),
  )
```

## DAP Gossip
Comunicazione bidirezionale, il messaggio A viene inviato, trasmesso ai vicini, una volta raggiunto C (target) un nuovo
messaggio di risposta B è generato
```scala 3
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
```

# SIMULATOR TASK
Per calcolare il tempo medio per cui una comunicazione avviene in 'n' distinte esecuzioni e calcolare il tempo relativo
tra 0 e 100% che il sistema passa nello stato di 'fail' è necessario implementare un oggetto che valuti le metriche
delle simulazioni.

Per farlo è stato necessario definire mediante extension methods alcuni metodi utili per effettuare questi calcoli.
Il metodo 'simulateNRuns' permette di effettuare 'n' simulazioni e restituirle in un LazyList di Trace (Simulations[S])
Il metodo 'pruneAllAt' permette di mantenere gli stati fino a sono quello specificato 's' escluso e restituisce una
LazyList di Trace (Simulations[S]).
Il metodo 'getAverageTime' calcola il tempo medio di tutte le simulazioni effettuate sommando i tempi di tutte le
simulazioni e dividendo per il numero di simulazioni.
Il metodo 'getAverageSpentTimeAt' calcola il tempo medio speso in uno stato specifico 's' sommando i tempi spesi
in quello stato in tutte le simulazioni e dividendo per il numero di simulazioni.
Per calcolare il tempo relativo speso in uno stato specifico 's' è sufficiente dividere il tempo medio speso
in quello stato per il tempo medio totale.

```scala 3
package scala.u07.examples

import u07.examples.StochasticChannel.State.*
import u07.examples.StochasticChannel.{State, stocChannel}
import u07.modelling.CTMC
import u07.modelling.CTMCSimulation.*

import java.util.Random

object StochasticSimulationMetrics:
  type Simulations[S] = LazyList[Trace[S]]
  given Random = new Random


  extension [S](self: CTMC[S])
    def simulateNRuns(n: Int, s0: S)(using rnd: Random): Simulations[S] =
      LazyList.range(0, n) map (i => self.newSimulationTrace(s0, rnd))

  extension [S](self: Trace[S])
    def intervals: Iterator[(S, Double)] = self.toList.sliding(2).collect {
      case List(e1: Event[S], e2: Event[S]) => (e1.state, e2.time - e1.time)
    }

  extension [S](self: Simulations[S])
    def prune(n: Int): Simulations[S] = self map (_.take(n))
    def pruneAllAt(s: S): Simulations[S] = self map (trace => trace.takeWhile(_.state != s))
    def getAverageTime: Double = if (self.nonEmpty) self.map(_.last.time).sum / self.size else 0.0
    def getAverageSpentTimeAt(s: S): Double = self.map(trace => trace.intervals.collect { case (`s`, t) => t }.sum).sum / self.size
    def getRelativeTimeSpentAt(s: S): Double = getAverageSpentTimeAt(s) / self.getAverageTime

object TryStochasticChannelSimulationMetrics:
  import StochasticSimulationMetrics.{*, given}

  @main def mainStochasticChannelSimulationMetrics =
    val successful = stocChannel.simulateNRuns(50, IDLE).pruneAllAt(DONE)
    val avgTime = successful.getAverageTime
    val avgTimeSpentFailing = successful.getAverageSpentTimeAt(FAIL)
    val percentageTimeFailing = successful.getRelativeTimeSpentAt(FAIL) * 100

    println(s"Percentage of time spent in FAIL state: $percentageTimeFailing % ($avgTimeSpentFailing s / $avgTime s)")
```
