package dotty.tools
package dotc
package eff

import core.*
import Types.*, Symbols.*, Contexts.*
import ast.tpd.*
import Annotations.Annotation
import Decorators.i
import CheckEffects.*


/**
  * Future work
  * Make setup phase where we transform all effect types to
  * annotated types with custom KillAnnotation() which has a set of refs as a params
  */

object EffectType:
  // small destructor for now
  def unapply(tp: Type)(using Context): Option[(Type, List[Tree])] =
    tp match
      case AnnotatedType(parent, annot) if annot.symbol.isKillEff =>
        Some(parent, annot.tree.killedElems)
      case _ => None


