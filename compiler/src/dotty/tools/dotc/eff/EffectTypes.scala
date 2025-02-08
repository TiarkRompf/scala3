package dotty.tools
package dotc
package eff

import core.*
import Types.*, Symbols.*, Contexts.*, Annotations.*
import printing.{Showable, Printer, Texts}
import Texts.Text
import ast.tpd

enum BinaryEff extends Showable:
  case Bot, IO

  def join(that: BinaryEff): BinaryEff = {
    that match
      case Bot => this
      case IO => that
  }

  def subsume(that: BinaryEff): Boolean = this.join(that) == that

  def |>(that: BinaryEff): BinaryEff = {
    (this, that) match
      case (Bot, Bot) => Bot
      case _ => IO
  }

  def toText(printer: Printer): Text =
    this match
      case Bot => "Bot"
      case IO => "IO"


case class EffectAnnotation() extends Annotation:
  // temporary tree for now, later make it like CaptureAnnotation and have EffectAnnotation take in symbol
  def tree(using Context): tpd.Tree = tpd.New(defn.AnnotationDefaultAnnot.typeRef, Nil) // temp for now

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

//   def extract(using Context): Type = {
//     tp match
//       case EffectType(parent, _) => parent
//       case _ => tp
//   }

//   def |>(that: Type)(using Context): BinaryEff = {
//     (tp, that) match
//       case (EffectType(_, tpEff), EffectType(_, thatEff)) => tpEff |> thatEff
//       case (EffectType(_, tpEff), _) => tpEff
//       case (_, EffectType(_, thatEff)) => thatEff
//       case _ => BinaryEff.Bot
//   }

//   def >>=(op: Type => Type)(using Context): Type = {
//     tp match
//       case EffectType(parent, eff) => EffectType(op(parent), eff)
//       case _ => op(tp)
//   }





