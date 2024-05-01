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

  given ToExpr[S.Symbol] with
    def apply(x: S.Symbol)(using Quotes): Expr[S.Symbol] =
      import quotes.reflect.*

      x match
        case S.SymbolRef(name) =>
          '{ S.SymbolRef(${ Expr(name) }) }
        case other =>
          throw new MatchError(s"Unsupported S.Symbol: ${other}")

  given ToExpr[B.LValue] with
    def apply(x: B.LValue)(using Quotes): Expr[B.LValue] =
      import quotes.reflect.*

      x match
        case B.Var(symRef, _, _) =>
          '{ B.Var(${ Expr(symRef) }) }

        case B.Access(bLhs, bRhs, _, _) =>
          '{ B.Access(${ Expr(bLhs) }, ${ Expr(bRhs) }) }

        case other =>
          throw new MatchError(s"Unsupported B.LValue: ${other}")

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

        case B.Var(symRef, _, _) =>
          '{ B.Var(${ Expr(symRef) }) }
        case B.Access(bLhs, bRhs, _, _) =>
          '{ B.Access(${ Expr(bLhs) }, ${ Expr(bRhs) }) }

        case B.VarDecl(t, name, valueExpr, _, _, _) =>
          '{ B.VarDecl(${ Expr(t) }, ${ Expr(name) }, ${ Expr(valueExpr) }) }

        case B.Block(statements, _, _, _) =>
          '{ B.Block.ofSeq(${ Expr.ofSeq(statements.map(Expr(_))) }) }

        case B.Assign(id, expr, _, _) =>
          '{ B.Assign(${ Expr(id) }, ${ Expr(expr) }) }

        case B.Call(id, args, _, _) =>
          '{ B.Call(${ Expr(id) }, ${ Expr.ofSeq(args.map(Expr(_))) }) }

        case B.UnaryMinus(y, _, _) =>
          '{ B.UnaryMinus(${ Expr(y) }) }

        case B.Vec(elems, _, _, _) =>
          '{ B.Vec(${ Expr.ofSeq(elems.map(Expr(_))) }) }

        case B.If(cond, then1, else1, _, _) =>
          '{ B.If(${ Expr(cond) }, ${ Expr(then1) }, ${ Expr(else1) }) }

        case other =>
          throw new MatchError(s"Unsupported B.Expr: ${other}")

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

      case ValDef(name, _, maybeTerm) =>
        val valueExpr = maybeTerm.map(t => iterate(t)).getOrElse(B.NothingVal())
        B.VarDecl(S.TypeRef("auto"), name, valueExpr)

      case Typed(expr, tpt) =>
        iterate(expr)

      case Block(statements, term) =>
        val ss     = statements.map(s => iterate(s))
        val retVal = iterate(term)
        B.Block((ss :+ retVal)*)

      case Ident(name) =>
        B.Var(S.SymbolRef(name))

      case Select(qualifier, sym) =>
        val bLhs: B.LValue = iterate(qualifier).asInstanceOf[B.LValue]
        val bRhs: B.LValue = B.Var(S.SymbolRef(sym))
        B.Access(bLhs, bRhs)

      case Assign(lhsTerm, rhsTerm) =>
        val bLhs: B.LValue = iterate(lhsTerm).asInstanceOf[B.LValue]
        val bRhs           = iterate(rhsTerm)
        B.Assign(bLhs, bRhs)

      case If(cond, thenp, elsep) =>
        val bCond: B.Expr         = iterate(cond)
        val bThen: B.Expr         = iterate(thenp)
        val bElse: Option[B.Expr] = Some(iterate(elsep))
        B.If(bCond, bThen, bElse)

      // Apply(Select(Ident("a"), "=="), List(Ident("b")))
      case Apply(fun, args) =>
        fun match
          case Select(qualifier, sym) =>
            val bArg  = iterate(qualifier)
            val bArgs = args.map(t => iterate(t))
            val bId   = S.SymbolRef(sym)
            B.Call(bId, bArg +: bArgs)
          case TypeApply(tFun, tArgs) =>
            tFun match
              case Select(Ident("List"), "apply") =>
                val vs = args.flatMap(a =>
                  a match
                    case Typed(Repeated(elems, _), _) =>
                      elems.map(e => iterate(e))
                    case other =>
                      List(iterate(other))
                )
                B.Vec(vs)
              case other =>
                throw new MatchError(s"Unsupported 'tFun' of TypeApply: ${other.show(using Printer.TreeStructure)}")
          case Ident(name) =>
            val bId   = S.SymbolRef(name)
            val bArgs = args.map(t => iterate(t))
            B.Call(bId, bArgs)

          case other =>
            throw new MatchError(s"Unsupported 'fun' of Apply: ${other.show(using Printer.TreeStructure)}")

      case Inlined(_, _, term) =>
        iterate(term)

      case other =>
        throw new MatchError(s"Unsupported Tree: ${other.show(using Printer.TreeStructure)}")

    // println(expr.asTerm.show(using Printer.TreeStructure))

    val bast = iterate(expr.asTerm)

    Expr(bast)
