package dotty.tools
package dotc
package eff

import core.*
import Phases.*, DenotTransformers.*, SymDenotations.*
import Contexts.*, Names.*, Flags.*, Symbols.*, Decorators.*
import Types.*, StdNames.*
import Annotations.Annotation
import ast.tpd
import transform.{PreRecheck, Recheck}
import Recheck.*

object Setup:
  val name: String = "setupEff"
  val description: String = "prepare compilation unit for effect checking"

class Setup extends PreRecheck, SymTransformer:
  thisPhase =>

  override def phaseName: String = Setup.name
  override def description: String = Setup.description

  override def isRunnable(using Context): Boolean = true

  def CCPhase: Phase =
    if this.prev.phaseName == "cc" then
      this.prev
    else
      assert(false, "Phase before effect checking setup phase is not capture checking!")

  def setupCCPhase: Phase =
    if CCPhase.prev.phaseName == "setupCC" then
      CCPhase.prev
    else
      assert(false, "Phase before capture checker is not capture checking setup!")

  def transformSym(symd: SymDenotation)(using Context): SymDenotation =
    val sym = symd.symbol
    def updatedAfter(p: Phase): Boolean =
      sym.isUpdatedAfter(p)
    if updatedAfter(setupCCPhase) then
      println("wallaahaoihaihaiho")
      atPhase(CCPhase)(sym.denot.copySymDenotation())
    symd

