package com.github.gchudnov.bscript.builder.internal

import com.github.gchudnov.bscript.builder.TypeCheckLaws
import com.github.gchudnov.bscript.builder.TypeCheckLaws.*
import com.github.gchudnov.bscript.builder.internal.BTypeCheckLaws.*
import com.github.gchudnov.bscript.lang.symbols.{ Symbol, Type }
import com.github.gchudnov.bscript.lang.types.Types


final case class BTypeCheckLaws(
  commonTable: CommonResult,
  additionTable: AdditionResult,
  arithmeticTable: ArithmeticResult,
  relationalTable: RelationalResult,
  equalityTable: EqualityResult,
  logicTable: LogicResult,
  unaryArithmeticSet: UnaryArithmeticAllow,
  unaryLogicSet: UnaryLogicAllow,
  promoteFromToTable: PromoteFromTo
) extends TypeCheckLaws


object BTypeCheckLaws:

  def make(types: Types): TypeCheckLaws =
    val autoType: Symbol with Type     = types.autoType
    val nothingType: Symbol with Type  = types.nothingType
    val voidType: Symbol with Type     = types.voidType
    val boolType: Symbol with Type     = types.boolType
    val i32Type: Symbol with Type      = types.i32Type
    val i64Type: Symbol with Type      = types.i64Type
    val f32Type: Symbol with Type      = types.f32Type
    val f64Type: Symbol with Type      = types.f64Type
    val decType: Symbol with Type      = types.decType
    val strType: Symbol with Type      = types.strType
    val dateType: Symbol with Type     = types.dateType
    val datetimeType: Symbol with Type = types.datetimeType

    /**
     * Concatenation rules for strings. If there is no entry, the combination is illegal.
     */
    val concatTable: TypeTable =
      Map(
        // string
        (strType, strType) -> strType
      )

    /**
     * Maps (t1 op t2) to result type; If there is no entry, the combination is illegal.
     */
    val arithmeticTable: TypeTable =
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

    val additionTable: TypeTable =
      arithmeticTable ++ concatTable

    /**
     * Maps if (..) (t1) else (t2) to result type; If there is no entry, the combination is illegal.
     */
    val commonTable: TypeTable =
      arithmeticTable ++
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
    val relationalTable: TypeTable =
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
    val equalityTable: TypeTable =
      relationalTable ++ Map(
        (boolType, boolType) -> boolType
      )

    /**
     * Maps t1 && t2 to the result type. IF there is no entry, the operation is illegal.
     */
    val logicTable: TypeTable =
      Map(
        (boolType, boolType) -> boolType
      )

    /**
     * Maps -(t) { minus t } to the types that support unary operations. If there is no entry, the operation is illegal.
     */
    val unaryArithmeticTable: TypeSet =
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
    val unaryLogicTable: TypeSet =
      Set(
        boolType
      )

    /**
     * Type promotion table. Whether a type needs a promotion to a wider type. If there is no value - no promotion is needed.
     */
    val promoteFromToTable: TypeTable =
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
        // float32
        (f32Type, f64Type) -> f64Type,
        (f32Type, decType) -> decType,
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

    new BTypeCheckLaws(
      commonTable = CommonResult(commonTable),
      additionTable = AdditionResult(additionTable),
      arithmeticTable = ArithmeticResult(arithmeticTable),
      relationalTable = RelationalResult(relationalTable),
      equalityTable = EqualityResult(equalityTable),
      logicTable = LogicResult(logicTable),
      unaryArithmeticSet = UnaryArithmeticAllow(unaryArithmeticTable),
      unaryLogicSet = UnaryLogicAllow(unaryLogicTable),
      promoteFromToTable = PromoteFromTo(promoteFromToTable)
    )
