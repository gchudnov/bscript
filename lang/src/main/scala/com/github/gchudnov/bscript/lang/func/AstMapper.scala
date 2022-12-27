package com.github.gchudnov.bscript.lang.func

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.types.*

/**
  * Maps AST
 * 
 * Usage:
 * {{{
 *
 * }}}
  */
trait AstMapper {

  def mapAST(ast: AST): AST = {
    ast match {
      case a: Stat =>
        mapStat(a)
      case a: TypeAST =>
        mapTypeAST(a)
      case other => 
        throw new MatchError(s"Unsupported AST type: ${other}")
    }
  }

  def mapStat(ast: Stat): Stat = {
    ast match {
      case a: Expr =>
        mapExpr(a)
      case other => 
        throw new MatchError(s"Unsupported AST type: ${other}")
    }
  }

  def mapTypeAST(ast: TypeAST): TypeAST = {
    ast match {
      case a @ Auto() =>
        ast
      case a @ TypeId(name) =>
        a
      case a @ Applied(aType, args) =>
        a.copy(aType = mapTypeAST(aType), args = mapTypeASTs(args))
      case other => 
        throw new MatchError(s"Unsupported AST type: ${other}")
    }
  }

  def mapExpr(ast: Expr): Expr = {
    ast match {
      case a: Ref =>
        mapRef(a)
      case a: Decl =>
        mapDecl(a)
      case a @ Assign(lhs, rhs) =>
        a.copy(lhs = mapRef(lhs), rhs = mapExpr(rhs))
      case a @ Block(exprs) =>
        a.copy(exprs = mapExprs(exprs))
      case a @ Call(id, args) =>
        ???
      case a @ Compiled(callback, retType) =>
        ???
      case a @ If(cond, then1, else1) =>
        ???
      case a @ Init() =>
        ???
      case a @ Literal(const) =>
        ???
      case other => 
        throw new MatchError(s"Unsupported AST type: ${other}")
    }
  }

  def mapRef(ast: Ref): Ref = {
    ast match {
      case a @ Access(x, y) =>
        a.copy(a = mapRef(x), b = mapRef(y).asInstanceOf[Id])
      case a @ Id(name) =>
        a
      case other => 
        throw new MatchError(s"Unsupported AST type: ${other}")
    }
  }

  def mapDecl(ast: Decl): Decl = {
    ast match {
      case a @ MethodDecl(name, tparams, params, retType, body) =>
        ???
      case a @ StructDecl(name, tfields, fields) =>
        ???
      case a @ VarDecl(name, vType, expr) =>
        ???
      case a @ TypeDecl(name) =>
        ???
      case other => 
        throw new MatchError(s"Unsupported AST type: ${other}")
    }
  }

  def mapASTs(asts: List[AST]): List[AST] =
    asts.mapConserve(x => mapAST(x))

  def mapTypeASTs(asts: List[TypeAST]): List[TypeAST] =
    asts.mapConserve(x => mapTypeAST(x))

  def mapExprs(asts: List[Expr]): List[Expr] =
    asts.mapConserve(x => mapExpr(x))
}

/*
   trait TreeMap:

      def transformStatement(tree: Statement)(owner: Symbol): Statement = {
        tree match {
          case tree: Term =>
            transformTerm(tree)(owner)
          case tree: ValDef =>
            val owner = tree.symbol
            val tpt1 = transformTypeTree(tree.tpt)(owner)
            val rhs1 = tree.rhs.map(x => transformTerm(x)(owner))
            ValDef.copy(tree)(tree.name, tpt1, rhs1)
          case tree: DefDef =>
            val owner = tree.symbol
            val newParamClauses = tree.paramss.mapConserve {
              case TypeParamClause(params) => TypeParamClause(transformSubTrees(params)(owner))
              case TermParamClause(params) => TermParamClause(transformSubTrees(params)(owner))
            }
            DefDef.copy(tree)(tree.name, newParamClauses, transformTypeTree(tree.returnTpt)(owner), tree.rhs.map(x => transformTerm(x)(owner)))
          case tree: TypeDef =>
            val owner = tree.symbol
            TypeDef.copy(tree)(tree.name, transformTree(tree.rhs)(owner))
          case tree: ClassDef =>
            val constructor @ DefDef(_, _, _, _) = transformStatement(tree.constructor)(tree.symbol): @unchecked
            val parents = tree.parents.map(transformTree(_)(tree.symbol))
            val self = tree.self.map { slf =>
              transformStatement(slf)(tree.symbol) match
                case self: ValDef => self
            }
            val body = tree.body.map(transformStatement(_)(tree.symbol))
            ClassDef.copy(tree)(tree.name, constructor.asInstanceOf[DefDef], parents, self, body) // cast as workaround for lampepfl/dotty#14821. TODO remove when referenceVersion >= 3.2.0-RC1
          case tree: Import =>
            Import.copy(tree)(transformTerm(tree.expr)(owner), tree.selectors)
          case tree: Export =>
            tree
          case _ =>
            throw MatchError(tree.show(using Printer.TreeStructure))
        }
      }

      def transformTerm(tree: Term)(owner: Symbol): Term = {
        tree match {
          case Ident(name) =>
            tree
          case Select(qualifier, name) =>
            Select.copy(tree)(transformTerm(qualifier)(owner), name)
          case This(qual) =>
            tree
          case Super(qual, mix) =>
            Super.copy(tree)(transformTerm(qual)(owner), mix)
          case Apply(fun, args) =>
            Apply.copy(tree)(transformTerm(fun)(owner), transformTerms(args)(owner))
          case TypeApply(fun, args) =>
            TypeApply.copy(tree)(transformTerm(fun)(owner), transformTypeTrees(args)(owner))
          case Literal(const) =>
            tree
          case New(tpt) =>
            New.copy(tree)(transformTypeTree(tpt)(owner))
          case Typed(expr, tpt) =>
            Typed.copy(tree)(transformTerm(expr)(owner), transformTypeTree(tpt)(owner))
          case tree: NamedArg =>
            NamedArg.copy(tree)(tree.name, transformTerm(tree.value)(owner))
          case Assign(lhs, rhs) =>
            Assign.copy(tree)(transformTerm(lhs)(owner), transformTerm(rhs)(owner))
          case Block(stats, expr) =>
            Block.copy(tree)(transformStats(stats)(owner), transformTerm(expr)(owner))
          case If(cond, thenp, elsep) =>
            If.copy(tree)(transformTerm(cond)(owner), transformTerm(thenp)(owner), transformTerm(elsep)(owner))
          case Closure(meth, tpt) =>
            Closure.copy(tree)(transformTerm(meth)(owner), tpt)
          case Match(selector, cases) =>
            Match.copy(tree)(transformTerm(selector)(owner), transformCaseDefs(cases)(owner))
          case Return(expr, from) =>
            Return.copy(tree)(transformTerm(expr)(owner), from)
          case While(cond, body) =>
            While.copy(tree)(transformTerm(cond)(owner), transformTerm(body)(owner))
          case Try(block, cases, finalizer) =>
            Try.copy(tree)(transformTerm(block)(owner), transformCaseDefs(cases)(owner), finalizer.map(x => transformTerm(x)(owner)))
          case Repeated(elems, elemtpt) =>
            Repeated.copy(tree)(transformTerms(elems)(owner), transformTypeTree(elemtpt)(owner))
          case Inlined(call, bindings, expansion) =>
            Inlined.copy(tree)(call, transformSubTrees(bindings)(owner), transformTerm(expansion)(owner))
          case SummonFrom(cases) =>
            SummonFrom.copy(tree)(transformCaseDefs(cases)(owner))
          case _ =>
            throw MatchError(tree.show(using Printer.TreeStructure))
        }
      }

      def transformTypeTree(tree: TypeTree)(owner: Symbol): TypeTree = tree match {
        case Inferred() => tree
        case tree: TypeIdent => tree
        case tree: TypeSelect =>
          TypeSelect.copy(tree)(tree.qualifier, tree.name)
        case tree: TypeProjection =>
          TypeProjection.copy(tree)(tree.qualifier, tree.name)
        case tree: Annotated =>
          Annotated.copy(tree)(tree.arg, tree.annotation)
        case tree: Singleton =>
          Singleton.copy(tree)(transformTerm(tree.ref)(owner))
        case tree: Refined =>
          Refined.copy(tree)(transformTypeTree(tree.tpt)(owner), transformTrees(tree.refinements)(owner).asInstanceOf[List[Definition]])
        case tree: Applied =>
          Applied.copy(tree)(transformTypeTree(tree.tpt)(owner), transformTrees(tree.args)(owner))
        case tree: MatchTypeTree =>
          MatchTypeTree.copy(tree)(tree.bound.map(b => transformTypeTree(b)(owner)), transformTypeTree(tree.selector)(owner), transformTypeCaseDefs(tree.cases)(owner))
        case tree: ByName =>
          ByName.copy(tree)(transformTypeTree(tree.result)(owner))
        case tree: LambdaTypeTree =>
          LambdaTypeTree.copy(tree)(transformSubTrees(tree.tparams)(owner), transformTree(tree.body)(owner))
        case tree: TypeBind =>
          TypeBind.copy(tree)(tree.name, tree.body)
        case tree: TypeBlock =>
          TypeBlock.copy(tree)(tree.aliases, tree.tpt)
        case _ =>
          throw MatchError(tree.show(using Printer.TreeStructure))
      }

      def transformCaseDef(tree: CaseDef)(owner: Symbol): CaseDef = {
        CaseDef.copy(tree)(transformTree(tree.pattern)(owner), tree.guard.map(x => transformTerm(x)(owner)), transformTerm(tree.rhs)(owner))
      }

      def transformTypeCaseDef(tree: TypeCaseDef)(owner: Symbol): TypeCaseDef = {
        TypeCaseDef.copy(tree)(transformTypeTree(tree.pattern)(owner), transformTypeTree(tree.rhs)(owner))
      }

      def transformStats(trees: List[Statement])(owner: Symbol): List[Statement] =
        trees mapConserve (x => transformStatement(x)(owner))

      def transformTrees(trees: List[Tree])(owner: Symbol): List[Tree] =
        trees mapConserve (x => transformTree(x)(owner))

      def transformTerms(trees: List[Term])(owner: Symbol): List[Term] =
        trees mapConserve (x => transformTerm(x)(owner))

      def transformTypeTrees(trees: List[TypeTree])(owner: Symbol): List[TypeTree] =
        trees mapConserve (x => transformTypeTree(x)(owner))

      def transformCaseDefs(trees: List[CaseDef])(owner: Symbol): List[CaseDef] =
        trees mapConserve (x => transformCaseDef(x)(owner))

      def transformTypeCaseDefs(trees: List[TypeCaseDef])(owner: Symbol): List[TypeCaseDef] =
        trees mapConserve (x => transformTypeCaseDef(x)(owner))

      def transformSubTrees[Tr <: Tree](trees: List[Tr])(owner: Symbol): List[Tr] =
        transformTrees(trees)(owner).asInstanceOf[List[Tr]]

    end TreeMap

*/