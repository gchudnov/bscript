package com.github.gchudnov.bscript.translator.internal.scala3

import scala.quoted.*
import com.github.gchudnov.bscript.lang.ast as B
import com.github.gchudnov.bscript.lang.symbols as S

object Scala3Import:

  given ToExpr[S.Type] with
    def apply(x: S.Type)(using Quotes): Expr[S.Type] =
      import quotes.reflect.*

      x match
        case S.TypeRef(name) =>
          '{ S.TypeRef(${ Expr(name) }) }
        case other =>
          throw new MatchError(s"Unsupported S.Type: ${other}")

  given ToExpr[B.Expr] with
    def apply(x: B.Expr)(using Quotes): Expr[B.Expr] =
      import quotes.reflect.*

      x match
        case B.BoolVal(value, _, _) =>
          '{ B.BoolVal(${ Expr(value) }) }
        case B.IntVal(value, _, _) =>
          '{ B.IntVal(${ Expr(value) }) }
        case B.LongVal(value, _, _) =>
          '{ B.LongVal(${ Expr(value) }) }
        case B.FloatVal(value, _, _) =>
          '{ B.FloatVal(${ Expr(value) }) }
        case B.DoubleVal(value, _, _) =>
          '{ B.DoubleVal(${ Expr(value) }) }
        case B.StrVal(value, _, _) =>
          '{ B.StrVal(${ Expr(value) }) }
        case B.VoidVal(_, _) =>
          '{ B.VoidVal() }
        case B.NothingVal(_, _) =>
          '{ B.NothingVal() }

        case B.VarDecl(t, name, valueExpr, _, _, _) =>
          '{ B.VarDecl(${ Expr(t) }, ${ Expr(name) }, ${ Expr(valueExpr) }) }

        case B.Block(statements, _, _, _) =>
          '{ B.Block.ofSeq(${ Expr.ofSeq(statements.map(Expr(_))) }) }

        case B.UnaryMinus(y, _, _) =>
          '{ B.UnaryMinus(${ Expr(y) }) }

        case other =>
          println(s"TO_EXPR, OTHER: ${other}")
          '{ B.NothingVal() }

  inline def make[T](inline x: T): B.AST =
    ${ makeImpl('x) }

  private def makeImpl[T: Type](expr: Expr[T])(using qctx: Quotes): Expr[B.AST] =
    import qctx.reflect.*

    def iterate(tree: Tree): B.Expr = tree match

      case Literal(c) =>
        c match
          case BooleanConstant(value) =>
            B.BoolVal(value)
          case ByteConstant(value) =>
            B.IntVal(value.toInt)
          case ShortConstant(value) =>
            B.IntVal(value.toInt)
          case IntConstant(value) =>
            B.IntVal(value.toInt)
          case LongConstant(value) =>
            B.LongVal(value)
          case FloatConstant(value) =>
            B.FloatVal(value)
          case DoubleConstant(value) =>
            B.DoubleVal(value)
          case CharConstant(value) =>
            B.StrVal(value.toString())
          case StringConstant(value) =>
            B.StrVal(value)
          case UnitConstant() =>
            B.VoidVal()
          case NullConstant() =>
            B.NothingVal()
          case other =>
            throw new MatchError(s"Unsupported Constant: ${other}")

      case ValDef(name, typ, maybeTerm) =>
        val valueExpr = maybeTerm.map(t => iterate(t)).getOrElse(B.NothingVal())
        B.VarDecl(S.TypeRef("auto"), name, valueExpr)

      case Block(statements, term) =>
        val ss     = statements.map(s => iterate(s))
        val retVal = iterate(term)
        B.Block((ss :+ retVal)*)

      case Inlined(_, _, term) => iterate(term)

      case other =>
        println("TREE_ACC, OTHER TREE:")
        println(other.show(using Printer.TreeStructure))
        B.NothingVal()

    val bast = iterate(expr.asTerm)

    Expr(bast)

/*
    // // Block(List(ValDef("a", Inferred(), Some(Literal(IntConstant(10))))), Literal(UnitConstant()))

        // VarDecl(TypeRef("A"), "a", Init(TypeRef("A")))
        // VarDecl(TypeRef(typeNames.i32Type), "x", IntVal(0)),
        // VarDecl(TypeRef(typeNames.autoType), "x", IntVal(10)),


import scala.quoted.*

object Transpiler:

  final case class MyState()

  /// TRACE

  inline def trace[T](inline x: T): Unit =
    ${traceImpl('x)}

  private def traceImpl[T: Type](expr: Expr[T])(using qctx: Quotes): Expr[Unit] =
    import qctx.reflect.*

    def collectAST(tree: Tree): MyState =
      val acc = new TreeAccumulator[MyState]:
        override def foldTree(x: MyState, tree: Tree)(owner: Symbol): MyState = tree match
          case Literal(c) =>
            c match {
              case y: BooleanConstant(value) =>
                BoolVal(value)
              case _ =>
                NothingVal()
            }
          case Block(ss, t) =>
            println("BLOCK: " + t)
            foldTrees(x, ss)(owner)
          case other =>
            println("OTHER: " + other)
            MyState()
            // foldOverTree(x, tree)(owner)
      acc.foldOverTree(MyState(), tree)(Symbol.noSymbol)

      //   def foldTree(syms: List[Symbol], tree: Tree)(owner: Symbol): List[Symbol] = tree match
      //     case ValDef(_, _, rhs) =>
      //       val newSyms = tree.symbol :: syms
      //       foldTree(newSyms, body)(tree.symbol)
      //     case other =>
      //       println(other)
      //       foldOverTree(syms, tree)(owner)
      // acc(Nil, tree)

    val tree: Tree = expr.asTerm

    // println(tree.show(using Printer.TreeStructure))

    collectAST(tree)

    '{()}

  /// DEBUG

  inline def debug(inline exprs: Any*): Unit =
    ${debugImpl('exprs)}

  private def debugImpl(exprs: Expr[Seq[Any]])(using Quotes): Expr[Unit] =
    import quotes.reflect.*

    def showWithValue(e: Expr[_]): Expr[String] =
      '{${Expr(e.show)} + " = " + $e}

    val stringExps: Seq[Expr[String]] = exprs match
      case Varargs(es) =>
        es.map { e =>
          e.asTerm match {
            case Literal(c: Constant) => Expr(c.value.toString)
            case _ => showWithValue(e)
          }
        }
      case e => List(showWithValue(e))

    val concatenatedStringsExp = stringExps
      .reduceOption((e1, e2) => '{$e1 + ", " + $e2})
      .getOrElse('{""})

    '{println($concatenatedStringsExp)}

  /// DEBUG-SINGLE

  inline def debugSingle(inline expr: Any): Unit =
    ${ debugSingleImpl('expr) }

  private def debugSingleImpl(expr: Expr[Any])(using Quotes): Expr[Unit] =
    '{ println("Value of " + ${ Expr(expr.show) } + " is " + $expr) }


// macro
object PrintTree {
  inline def printTree[T](inline x: T): Unit = ${printTreeImpl('x)}
  def printTreeImpl[T: Type](x: Expr[T])(using qctx: Quotes): Expr[Unit] =
    import qctx.reflect.*
    println(x.asTerm.show(using Printer.TreeStructure))
    '{()}
}

// usage
printTree {
  (s: String) => s.length
}

// output
Inlined(None, Nil, Block(Nil, Block(List(DefDef("$anonfun", List(TermParamClause(List(ValDef("s", TypeIdent("String"), None)))), Inferred(), Some(Block(Nil, Apply(Select(Ident("s"), "length"), Nil))))), Closure(Ident("$anonfun"), None))))

// after a while the above representation becomes semi-readable


 */
