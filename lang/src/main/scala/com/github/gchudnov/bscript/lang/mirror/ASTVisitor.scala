package com.github.gchudnov.bscript.lang.mirror

import com.github.gchudnov.bscript.lang.ast.*

/**
 * Visit AST
 */
trait ASTVisitor[A]:

  def foldAST(a: A, ast: AST): A

  def foldASTs(a: A, asts: Iterable[AST]): A = 
    asts.foldLeft(a)(foldAST)

  def foldOverTree(a: A, ast: AST): A = {
    ast match {
      case Access(lhs, rhs) =>
        foldAST(foldAST(a, lhs), rhs)
    }
  }

/*
  def visit(s: S, n: Init): Either[Throwable, R]
  def visit(s: S, n: UnaryMinus): Either[Throwable, R]
  def visit(s: S, n: Add): Either[Throwable, R]
  def visit(s: S, n: Sub): Either[Throwable, R]
  def visit(s: S, n: Mul): Either[Throwable, R]
  def visit(s: S, n: Div): Either[Throwable, R]
  def visit(s: S, n: Mod): Either[Throwable, R]
  def visit(s: S, n: Less): Either[Throwable, R]
  def visit(s: S, n: LessEqual): Either[Throwable, R]
  def visit(s: S, n: Greater): Either[Throwable, R]
  def visit(s: S, n: GreaterEqual): Either[Throwable, R]
  def visit(s: S, n: Equal): Either[Throwable, R]
  def visit(s: S, n: NotEqual): Either[Throwable, R]
  def visit(s: S, n: Not): Either[Throwable, R]
  def visit(s: S, n: And): Either[Throwable, R]
  def visit(s: S, n: Or): Either[Throwable, R]
  def visit(s: S, n: Assign): Either[Throwable, R]
  def visit(s: S, n: NothingVal): Either[Throwable, R]
  def visit(s: S, n: VoidVal): Either[Throwable, R]
  def visit(s: S, n: BoolVal): Either[Throwable, R]
  def visit(s: S, n: IntVal): Either[Throwable, R]
  def visit(s: S, n: LongVal): Either[Throwable, R]
  def visit(s: S, n: FloatVal): Either[Throwable, R]
  def visit(s: S, n: DoubleVal): Either[Throwable, R]
  def visit(s: S, n: DecimalVal): Either[Throwable, R]
  def visit(s: S, n: StrVal): Either[Throwable, R]
  def visit(s: S, n: DateVal): Either[Throwable, R]
  def visit(s: S, n: DateTimeVal): Either[Throwable, R]
  def visit(s: S, n: StructVal): Either[Throwable, R]
  def visit(s: S, n: Vec): Either[Throwable, R]
  def visit(s: S, n: Var): Either[Throwable, R]
  def visit(s: S, n: ArgDecl): Either[Throwable, R]
  def visit(s: S, n: VarDecl): Either[Throwable, R]
  def visit(s: S, n: FieldDecl): Either[Throwable, R]
  def visit(s: S, n: MethodDecl): Either[Throwable, R]
  def visit(s: S, n: StructDecl): Either[Throwable, R]
  def visit(s: S, n: Block): Either[Throwable, R]
  def visit(s: S, n: Call): Either[Throwable, R]
  def visit(s: S, n: If): Either[Throwable, R]
  def visit(s: S, n: Access): Either[Throwable, R]
  def visit(s: S, n: CompiledExpr): Either[Throwable, R]

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