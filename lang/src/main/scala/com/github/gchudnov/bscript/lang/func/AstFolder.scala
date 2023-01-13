
package com.github.gchudnov.bscript.lang.func

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.types.*
import com.github.gchudnov.bscript.lang.ast.decls.*

/**
 * Folds AST
 */
trait AstFolder[S]:

  def foldAST(s: S, ast: AST): S

  def foldASTs(s: S, asts: Iterable[AST]): S =
    asts.foldLeft(s)(foldAST)

  def foldOverAST(s: S, ast: AST): S =
    ast match
      case Access(lhs, rhs) =>
        foldAST(foldAST(s, lhs), rhs)
      case Id(_) =>
        s

      case MethodDecl(name, mType, body) =>
        foldAST(foldAST(s, mType), body)
      case StructDecl(name, sType) =>
        foldAST(s, sType)
      case VarDecl(_, vType, expr) =>
        foldAST(foldAST(s, vType), expr)
      case TypeDecl(_) =>
        s

      case Annotated(expr, id, tparams, params) =>
        ???
      case Assign(lhs, rhs) =>
        foldAST(foldAST(s, lhs), rhs)


      case Block(exprs) =>
        foldASTs(s, exprs)
      case Literal(_) =>
        s
      case Call(id, args) =>
        foldASTs(foldAST(s, id), args)
      case Compiled(_, retType) =>
        foldAST(s, retType)
      case If(cond, then1, else1) =>
        foldAST(foldAST(foldAST(s, cond), then1), else1)
      case Init() =>
        s
      case Auto() =>
        s
      case TypeId(name) =>
        s
      case VecType(elemType) =>
        foldAST(s, elemType)
      case MapType(keyType, valueType) =>
        foldAST(foldAST(s, keyType), valueType)
      case StructType(tfields, fields) =>
        fields.foldLeft(foldASTs(s, tfields))(foldAST)
      case MethodType(tparams, params, retType) =>
        foldAST(params.foldLeft(foldASTs(s, tparams))(foldAST), retType)


      case Applied(aType, args) =>
        foldASTs(foldAST(s, aType), args)
      case Vec(elems, elemType) =>
        foldASTs(foldAST(s, elemType), elems)
      case Dict(m, keyType, valType) =>
        m.foldLeft(foldAST(foldAST(s, keyType), valType))({ case (acc, (k, v)) => foldAST(acc, v) })



      case other =>
        throw new MatchError(s"Unsupported AST type: ${other}")

/*

trait TreeAccumulator[X] {

  val reflect: Reflection
  import reflect.{given _, _}

  // Ties the knot of the traversal: call `foldOver(x, tree))` to dive in the `tree` node.
  def foldTree(x: X, tree: Tree)(using ctx: Context): X

  def foldTrees(x: X, trees: Iterable[Tree])(using ctx: Context): X = trees.foldLeft(x)(foldTree)

  def foldOverTree(x: X, tree: Tree)(using ctx: Context): X = {
    def localCtx(definition: Definition): Context = definition.symbol.localContext
    tree match {
      case Ident(_) =>
        x
      case Select(qualifier, _) =>
        foldTree(x, qualifier)
      case This(qual) =>
        x
      case Super(qual, _) =>
        foldTree(x, qual)
      case Apply(fun, args) =>
        foldTrees(foldTree(x, fun), args)
      case TypeApply(fun, args) =>
        foldTrees(foldTree(x, fun), args)
      case Literal(const) =>
        x
      case New(tpt) =>
        foldTree(x, tpt)
      case Typed(expr, tpt) =>
        foldTree(foldTree(x, expr), tpt)
      case NamedArg(_, arg) =>
        foldTree(x, arg)
      case Assign(lhs, rhs) =>
        foldTree(foldTree(x, lhs), rhs)
      case Block(stats, expr) =>
        foldTree(foldTrees(x, stats), expr)
      case If(cond, thenp, elsep) =>
        foldTree(foldTree(foldTree(x, cond), thenp), elsep)
      case While(cond, body) =>
        foldTree(foldTree(x, cond), body)
      case Closure(meth, tpt) =>
        foldTree(x, meth)
      case Match(selector, cases) =>
        foldTrees(foldTree(x, selector), cases)
      case Return(expr) =>
        foldTree(x, expr)
      case Try(block, handler, finalizer) =>
        foldTrees(foldTrees(foldTree(x, block), handler), finalizer)
      case Repeated(elems, elemtpt) =>
        foldTrees(foldTree(x, elemtpt), elems)
      case Inlined(call, bindings, expansion) =>
        foldTree(foldTrees(x, bindings), expansion)
      case vdef @ ValDef(_, tpt, rhs) =>
        val ctx = localCtx(vdef)
        given Context = ctx
        foldTrees(foldTree(x, tpt), rhs)
      case ddef @ DefDef(_, tparams, vparamss, tpt, rhs) =>
        val ctx = localCtx(ddef)
        given Context = ctx
        foldTrees(foldTree(vparamss.foldLeft(foldTrees(x, tparams))(foldTrees), tpt), rhs)
      case tdef @ TypeDef(_, rhs) =>
        val ctx = localCtx(tdef)
        given Context = ctx
        foldTree(x, rhs)
      case cdef @ ClassDef(_, constr, parents, derived, self, body) =>
        val ctx = localCtx(cdef)
        given Context = ctx
        foldTrees(foldTrees(foldTrees(foldTrees(foldTree(x, constr), parents), derived), self), body)
      case Import(expr, _) =>
        foldTree(x, expr)
      case clause @ PackageClause(pid, stats) =>
        foldTrees(foldTree(x, pid), stats)(using clause.symbol.localContext)
      case Inferred() => x
      case TypeIdent(_) => x
      case TypeSelect(qualifier, _) => foldTree(x, qualifier)
      case Projection(qualifier, _) => foldTree(x, qualifier)
      case Singleton(ref) => foldTree(x, ref)
      case Refined(tpt, refinements) => foldTrees(foldTree(x, tpt), refinements)
      case Applied(tpt, args) => foldTrees(foldTree(x, tpt), args)
      case ByName(result) => foldTree(x, result)
      case Annotated(arg, annot) => foldTree(foldTree(x, arg), annot)
      case LambdaTypeTree(typedefs, arg) => foldTree(foldTrees(x, typedefs), arg)
      case TypeBind(_, tbt) => foldTree(x, tbt)
      case TypeBlock(typedefs, tpt) => foldTree(foldTrees(x, typedefs), tpt)
      case MatchTypeTree(boundopt, selector, cases) =>
        foldTrees(foldTree(boundopt.fold(x)(foldTree(x, _)), selector), cases)
      case WildcardTypeTree() => x
      case TypeBoundsTree(lo, hi) => foldTree(foldTree(x, lo), hi)
      case CaseDef(pat, guard, body) => foldTree(foldTrees(foldTree(x, pat), guard), body)
      case TypeCaseDef(pat, body) => foldTree(foldTree(x, pat), body)
      case Bind(_, body) => foldTree(x, body)
      case Unapply(fun, implicits, patterns) => foldTrees(foldTrees(foldTree(x, fun), implicits), patterns)
      case Alternatives(patterns) => foldTrees(x, patterns)
    }
  }
}


 */
