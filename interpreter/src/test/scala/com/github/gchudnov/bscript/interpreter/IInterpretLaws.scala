package com.github.gchudnov.bscript.interpreter

// import com.github.gchudnov.bscript.lang.types.Types
// import com.github.gchudnov.bscript.builder.state.Meta
// import com.github.gchudnov.bscript.interpreter.laws.*

// final case class IInterpretLaws(
//   mathLaws: Arithmetic,
//   boolLaws: BoolArithmetic,
//   cmpLaws: Comparator,
//   initLaws: Initializer,
//   typeCaster: TypeCaster
// ) extends InterpreterLaws

// object IInterpretLaws:
//   def make(types: Types, meta: Meta): InterpreterLaws = IInterpretLaws(
//     mathLaws = new IArithmetic(),
//     boolLaws = new IBoolArithmetic(),
//     cmpLaws = new IComparator(),
//     initLaws = new IInitializer(types, meta),
//     typeCaster = new ITypeCaster(types)
//   )

trait IInterpretLaws {}