package com.github.gchudnov.bscript.builder

import com.github.gchudnov.bscript.builder.TypeCheckLaws.*
import com.github.gchudnov.bscript.lang.symbols.{ Type }

/**
 * A collection of tables that define rules for type checking
 */
trait TypeCheckLaws:
  def commonTable: CommonResult
  def additionTable: AdditionResult
  def arithmeticTable: ArithmeticResult
  def relationalTable: RelationalResult
  def equalityTable: EqualityResult
  def logicTable: LogicResult
  def unaryArithmeticSet: UnaryArithmeticAllow
  def unaryLogicSet: UnaryLogicAllow
  def promoteFromToTable: PromoteFromTo


object TypeCheckLaws:

  type TypeTable = Map[(Type, Type), Type] // (type -> type) -> type
  type TypeSet   = Set[Type]

  final case class CommonResult(tt: TypeTable)       extends AnyVal
  final case class AdditionResult(tt: TypeTable)     extends AnyVal
  final case class ArithmeticResult(tt: TypeTable)   extends AnyVal
  final case class RelationalResult(tt: TypeTable)   extends AnyVal
  final case class EqualityResult(tt: TypeTable)     extends AnyVal
  final case class LogicResult(tt: TypeTable)        extends AnyVal
  final case class UnaryArithmeticAllow(ts: TypeSet) extends AnyVal
  final case class UnaryLogicAllow(ts: TypeSet)      extends AnyVal
  final case class PromoteFromTo(tt: TypeTable)      extends AnyVal

  object OpName:
    val plus: String         = "+"
    val minus: String        = "-"
    val multiply: String     = "*"
    val division: String     = "/"
    val modulo: String       = "%"
    val less: String         = "<"
    val lessEqual: String    = "<="
    val greater: String      = ">"
    val greaterEqual: String = ">="
    val equal: String        = "=="
    val notEqual: String     = "!="
    val not: String          = "!"
    val and: String          = "&&"
    val or: String           = "||"
    val assign: String       = "="
