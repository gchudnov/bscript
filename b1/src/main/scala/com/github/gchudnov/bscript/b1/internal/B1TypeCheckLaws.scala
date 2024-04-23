package com.github.gchudnov.bscript.b1.internal

import com.github.gchudnov.bscript.builder.TypeCheckLaws
import com.github.gchudnov.bscript.builder.TypeCheckLaws.*
import com.github.gchudnov.bscript.lang.symbols.{ Symbol, Type }
import com.github.gchudnov.bscript.lang.types.Types

private[b1] object B1TypeCheckLaws:

  def make(types: Types): TypeCheckLaws =
    val autoType: Symbol & Type     = types.autoType
    val nothingType: Symbol & Type  = types.nothingType
    val voidType: Symbol & Type     = types.voidType
    val boolType: Symbol & Type     = types.boolType
    val i32Type: Symbol & Type      = types.i32Type
    val i64Type: Symbol & Type      = types.i64Type
    val f32Type: Symbol & Type      = types.f32Type
    val f64Type: Symbol & Type      = types.f64Type
    val decType: Symbol & Type      = types.decType
    val strType: Symbol & Type      = types.strType
    val dateType: Symbol & Type     = types.dateType
    val datetimeType: Symbol & Type = types.datetimeType

    /**
     * Concatenation rules for strings. If there is no entry, the combination is illegal.
     */
    val concatTable1: TypeTable =
      Map(
        // string
        (strType, strType) -> strType
      )

    /**
     * Maps (t1 op t2) to result type; If there is no entry, the combination is illegal.
     */
    val arithmeticTable1: TypeTable =
      Map(
        // int32
        (i32Type, i32Type) -> i32Type,
        (i32Type, i64Type) -> i64Type,
        (i32Type, f32Type) -> f32Type,
        (i32Type, f64Type) -> f64Type,
        (i32Type, decType) -> decType,
        // int64
        (i64Type, i32Type) -> i64Type,
        (i64Type, i64Type) -> i64Type,
        (i64Type, f32Type) -> f32Type,
        (i64Type, f64Type) -> f64Type,
        (i64Type, decType) -> decType,
        // float32
        (f32Type, i32Type) -> f32Type,
        (f32Type, i64Type) -> f32Type,
        (f32Type, f32Type) -> f32Type,
        (f32Type, f64Type) -> f64Type,
        (f32Type, decType) -> decType,
        // float64
        (f64Type, i32Type) -> f64Type,
        (f64Type, i64Type) -> f64Type,
        (f64Type, f32Type) -> f64Type,
        (f64Type, f64Type) -> f64Type,
        (f64Type, decType) -> decType,
        // decimal
        (decType, i32Type) -> decType,
        (decType, i64Type) -> decType,
        (decType, f32Type) -> decType,
        (decType, f64Type) -> decType,
        (decType, decType) -> decType
      )

    val additionTable1: TypeTable =
      arithmeticTable1 ++ concatTable1

    /**
     * Maps if (..) (t1) else (t2) to result type; If there is no entry, the combination is illegal.
     */
    val commonTable1: TypeTable =
      arithmeticTable1 ++
        Map(
          // void
          (voidType, voidType)     -> voidType,
          (voidType, boolType)     -> voidType,
          (voidType, i32Type)      -> voidType,
          (voidType, i64Type)      -> voidType,
          (voidType, f32Type)      -> voidType,
          (voidType, f64Type)      -> voidType,
          (voidType, decType)      -> voidType,
          (voidType, strType)      -> voidType,
          (voidType, dateType)     -> voidType,
          (voidType, datetimeType) -> voidType,
          // bool
          (boolType, voidType) -> voidType,
          // int32
          (i32Type, voidType) -> voidType,
          // int64
          (i64Type, voidType) -> voidType,
          // float32
          (f32Type, voidType) -> voidType,
          // float64
          (f64Type, voidType) -> voidType,
          // decimal
          (decType, voidType) -> voidType,
          // string
          (strType, voidType) -> voidType,
          // date
          (dateType, voidType) -> voidType,
          // datetime
          (datetimeType, voidType) -> voidType
        )

    /**
     * Maps t1 relOp t2 to the result type, e.g. 1 < 2. If there is no entry, the operation is illegal.
     */
    val relationalTable1: TypeTable =
      Map(
        // int32
        (i32Type, i32Type) -> boolType,
        (i32Type, i64Type) -> boolType,
        (i32Type, f32Type) -> boolType,
        (i32Type, f64Type) -> boolType,
        (i32Type, decType) -> boolType,
        // int64
        (i64Type, i32Type) -> boolType,
        (i64Type, i64Type) -> boolType,
        (i64Type, f32Type) -> boolType,
        (i64Type, f64Type) -> boolType,
        (i64Type, decType) -> boolType,
        // float32
        (f32Type, i32Type) -> boolType,
        (f32Type, i64Type) -> boolType,
        (f32Type, f32Type) -> boolType,
        (f32Type, f64Type) -> boolType,
        (f32Type, decType) -> boolType,
        // float64
        (f64Type, i32Type) -> boolType,
        (f64Type, i64Type) -> boolType,
        (f64Type, f32Type) -> boolType,
        (f64Type, f64Type) -> boolType,
        (f64Type, decType) -> boolType,
        // decimal
        (decType, i32Type) -> boolType,
        (decType, i64Type) -> boolType,
        (decType, f32Type) -> boolType,
        (decType, f64Type) -> boolType,
        (decType, decType) -> boolType,
        // string
        (strType, strType) -> boolType,
        // date
        (dateType, dateType)     -> boolType,
        (dateType, datetimeType) -> boolType,
        // datetime
        (datetimeType, dateType)     -> boolType,
        (datetimeType, datetimeType) -> boolType
      )

    /**
     * Maps t1 == t2 to the result type. If there is no entry, the operation is illegal.
     */
    val equalityTable1: TypeTable =
      relationalTable1 ++ Map(
        (boolType, boolType) -> boolType
      )

    /**
     * Maps t1 && t2 to the result type. IF there is no entry, the operation is illegal.
     */
    val logicTable1: TypeTable =
      Map(
        (boolType, boolType) -> boolType
      )

    /**
     * Maps -(t) { minus t } to the types that support unary operations. If there is no entry, the operation is illegal.
     */
    val unaryArithmeticTable1: TypeSet =
      Set(
        i32Type,
        i64Type,
        f32Type,
        f64Type,
        decType
      )

    /**
     * Maps !t to the result type. If there is no entry, the operation is illegal.
     */
    val unaryLogicTable1: TypeSet =
      Set(
        boolType
      )

    /**
     * Type promotion table. Whether a type needs a promotion to a wider type. If there is no value - no promotion is needed.
     */
    val promoteFromToTable1: TypeTable =
      Map(
        // int32
        (i32Type, i64Type) -> i64Type,
        (i32Type, f32Type) -> f32Type,
        (i32Type, f64Type) -> f64Type,
        (i32Type, decType) -> decType,
        // int64
        (i64Type, f32Type) -> f32Type,
        (i64Type, f64Type) -> f64Type,
        (i64Type, decType) -> decType,
        // float32DefaultType,
        // float64
        (f64Type, decType) -> decType,
        // decimal

        // nothing -- nothing can be promoted to anything (but it remains Nothing)
        (nothingType, voidType)     -> voidType,
        (nothingType, boolType)     -> boolType,
        (nothingType, i32Type)      -> i32Type,
        (nothingType, i64Type)      -> i64Type,
        (nothingType, f32Type)      -> f32Type,
        (nothingType, f64Type)      -> f64Type,
        (nothingType, decType)      -> decType,
        (nothingType, strType)      -> strType,
        (nothingType, dateType)     -> dateType,
        (nothingType, datetimeType) -> datetimeType
      )

    new TypeCheckLaws:
      override val commonTable: CommonResult                = CommonResult(commonTable1)
      override val additionTable: AdditionResult            = AdditionResult(additionTable1)
      override val arithmeticTable: ArithmeticResult        = ArithmeticResult(arithmeticTable1)
      override val relationalTable: RelationalResult        = RelationalResult(relationalTable1)
      override val equalityTable: EqualityResult            = EqualityResult(equalityTable1)
      override val logicTable: LogicResult                  = LogicResult(logicTable1)
      override val unaryArithmeticSet: UnaryArithmeticAllow = UnaryArithmeticAllow(unaryArithmeticTable1)
      override val unaryLogicSet: UnaryLogicAllow           = UnaryLogicAllow(unaryLogicTable1)
      override val promoteFromToTable: PromoteFromTo        = PromoteFromTo(promoteFromToTable1)
