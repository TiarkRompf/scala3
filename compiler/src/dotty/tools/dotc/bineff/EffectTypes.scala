package dotty.tools
package dotc
package bineff

import core.*
import Types.*, Symbols.*, Contexts.*, Annotations.*
import ast.tpd

case class EffectAnnotation() extends Annotation:
  // temporary tree for now, later make it like CaptureAnnotation and have EffectAnnotation take in symbol?
  def tree(using Context): tpd.Tree =
    // println(ctx.phase)
    tpd.Literal(Constants.Constant("eff"))
    // tpd.New(defn.AnnotationDefaultAnnot.typeRef, Nil) // temp for now

  def |>(that: EffectAnnotation)(using Context): EffectAnnotation = // maybe useful in future?
    EffectAnnotation()

object EffectType:
  def apply(parent: Type)(using Context): AnnotatedType =
      AnnotatedType(parent, EffectAnnotation())

  def unapply(tp: Type)(using Context): Option[(Type, EffectAnnotation)] = {
    tp match
      case AnnotatedType(parent, annot: EffectAnnotation) =>
        Some(parent, annot)
      case _ => None
  }

extension (tp: Type)
  def deriveEffectType(using Context): Type = {
    tp match
      case EffectType(_) => tp
      case _ => EffectType(tp)
  }

  def isEffType(using Context): Boolean = {
    tp match
      case EffectType(_) => true
      case _ => false
  }



