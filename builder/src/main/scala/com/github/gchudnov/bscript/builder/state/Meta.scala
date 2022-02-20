package com.github.gchudnov.bscript.builder.state

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.lang.symbols.{ Named, SBlock, SBuiltInType, SMethod, SStruct, SVar, Scope, ScopeStateException, Symbol, Type, TypeRef }
import com.github.gchudnov.bscript.lang.util.{ Show, Transform }
import com.github.gchudnov.bscript.builder.util.EqWrap
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.types.Types
import com.github.gchudnov.bscript.lang.util.Show

/**
 * Metadata - Scope & Symbol State
 *
 * NOTE: we're not storing symbols in Block, Method, Struct explicitly, since it is causing huge AST-rewrites on updates.
 *
 * To avoid rewrites, it is possible to use mutable Block, Method, Struct, but we want to avoid it to enable better traceability of mutations.
 *
 * @param scopeTree
 *   Scope tree where a child Scope points to a parent Scope (child -> parent)
 * @param scopeSymbols
 *   All symbols that belong to a scope; { Scope -> Set[Symbol] }
 * @param symbolScopes
 *   Scope a Symbol belongs to; { Symbol -> Scope }
 * @param methodArgs
 *   Arguments that belong to a method; { Method -> List[Var] }
 * @param methodRetTypes
 *   Return Type, associated with a method; { Method -> Type }
 * @param methodAsts
 *   AST, a Symbol references; { Symbol -> AST }
 * @param astScopes
 *   Scope, AST refers to; { AST -> Scope } -- parent scope for AST
 * @param varTypes
 *   Type, assigned to a variable { Var -> Type }. A Type for the variable might be changed, e.g. `auto` -> `long`.
 */
final case class Meta(
  scopeTree: ScopeTree,
  scopeSymbols: Map[EqWrap[Scope], Set[Symbol]], // Pass #1
  symbolScopes: Map[EqWrap[Symbol], Scope],      // Pass #1
  methodArgs: Map[EqWrap[SMethod], List[SVar]],  // Pass #1
  methodRetTypes: Map[EqWrap[SMethod], Type],    // Pass   #2
  methodAsts: Map[EqWrap[SMethod], AST],         // Pass   #2
  astScopes: Map[EqWrap[AST], Scope],            // Pass #1
  varTypes: Map[EqWrap[SVar], Type]              // Pass   #2
):

  /**
   * Resolve a symbol in the scope recursively up to the root
   */
  def resolve(name: String, in: Scope): Either[ScopeStateException, Symbol] =
    maybeResolve(name, in)
      .toRight(new ScopeStateException(s"Cannot find a Symbol '${name}' starting from Scope '${in.name}'"))

  private def maybeResolve(name: String, in: Scope): Option[Symbol] =
    scopeSymbols
      .get(EqWrap(in))
      .flatMap(_.find(_.name == name))
      .orElse(scopeTree.parent(in).flatMap(parentScope => maybeResolve(name, parentScope)))

  /**
   * Resolves a member of a scope by Name
   */
  def resolveMember(name: String, in: Scope): Either[ScopeStateException, Symbol] =
    maybeResolveMember(name, in)
      .toRight(new ScopeStateException(s"Cannot find a Symbol '${name}' in Scope '${in.name}'"))

  private def maybeResolveMember(name: String, in: Scope): Option[Symbol] =
    scopeSymbols
      .get(EqWrap(in))
      .flatMap(_.find(_.name == name))

  /**
   * Defines a root Scope, not connected to any parent Scope
   *
   * NOTE: it could be multiple Scopes, not connected to the parent, but the use-case for that is not clear.
   */
  def defineBlock(block: SBlock): Meta =
    this.copy(scopeTree = scopeTree.add(block))

  /**
   * Defines a Block in the given Scope
   *
   * NOTE: Block is not a symbol
   */
  def defineBlock(block: SBlock, in: Scope): Meta =
    this.copy(scopeTree = scopeTree.link(block, in))

  /**
   * Defines a Method in the given Scope
   *
   * NOTE: a Method is a symbol as well.
   */
  def defineMethod(method: SMethod, in: Scope): Meta =
    defineSymbolScopeInScope(method, in)

  /**
   * Defines a Struct in the given Scope
   *
   * NOTE: a Struct is a symbol as well.
   */
  def defineStruct(struct: SStruct, in: Scope): Meta =
    defineSymbolScopeInScope(struct, in)

  /**
   * Defines a BuiltInType
   *
   * NOTE: a BuiltInType might be defined *only* in Block-scope.
   */
  def defineBuiltInType(t: SBuiltInType, in: SBlock): Meta =
    assert(scopeTree.parent(in).isEmpty, "A Built-in Symbols should be defined in The root scope (without parent scope)")
    defineSymbolInScope(t, in)

  /**
   * Defines a field in the Struct
   */
  def defineStructField(ss: SStruct, field: SVar): Meta =
    defineSymbolInScope(field, ss)

  /**
   * Defines a method argument (used to have an ordered list of arguments)
   */
  def defineMethodArg(ms: SMethod, arg: SVar): Meta =
    val (newScopeSymbols, newSymbolScopes) = addScopeSymbol(arg, ms)
    val newMethodArgs                      = addMethodArg(ms, arg)
    this.copy(scopeSymbols = newScopeSymbols, symbolScopes = newSymbolScopes, methodArgs = newMethodArgs)

  /**
   * Defines a method type
   */
  def defineMethodRetType(sm: SMethod, retType: Type): Meta =
    val newMethodRetTypes = addMethodRetType(sm, retType)
    this.copy(methodRetTypes = newMethodRetTypes)

  /**
   * Defines a method AST
   */
  def defineMethodAST(sm: SMethod, ast: AST): Meta =
    val newMethodAsts = addMethodAST(sm, ast)
    this.copy(methodAsts = newMethodAsts)

  /**
   * Defines a scope for an AST
   */
  def defineASTScope(ast: AST, scope: Scope): Meta =
    val newAstScopes = addASTScope(ast, scope)
    this.copy(astScopes = newAstScopes)

  /**
   * Redefine AST in { AST -> Scope } bindings
   */
  def redefineASTScope(oldAst: AST, newAst: AST): Meta =
    val newAstScopes = replaceASTScope(oldAst, newAst)
    this.copy(astScopes = newAstScopes)

  def defineVar(v: SVar, in: SBlock): Meta =
    defineSymbolInScope(v, in)

  /**
   * Defines a Variable Symbol with the given Type. If the variable is already exists, redefines it.
   */
  def defineVarType(v: SVar, t: Type): Meta =
    val newVarTypes = addVarType(t, v)
    this.copy(varTypes = newVarTypes)

  /**
   * Gets the identifier of the symbol
   */
  def id(symbol: Symbol): Either[ScopeStateException, String] =
    Right(EqWrap(symbol).hashCode().toString)

  /**
   * Get a Scope for the given AST
   */
  def scopeFor(ast: AST): Either[ScopeStateException, Scope] =
    astScopes
      .get(EqWrap(ast))
      .toRight(new ScopeStateException(s"Scope for AST '${ast}' is not found."))

  /**
   * Get a Scope for the given Symbol
   */
  def scopeFor(symbol: Symbol): Either[ScopeStateException, Scope] =
    symbolScopes
      .get(EqWrap(symbol))
      .toRight(new ScopeStateException(s"Scope for Symbol '${symbol.name}' is not found."))

  /**
   * Get a Type for the given Variable Symbol (SVar)
   */
  def typeFor(v: SVar): Either[ScopeStateException, Type] =
    varTypes
      .get(EqWrap(v))
      .toRight(new ScopeStateException(s"Type for SVar '${v.name}' is not found."))

  /**
   * Get a Type for the given Method Symbol (SMethod)
   */
  def retTypeFor(m: SMethod): Either[ScopeStateException, Type] =
    methodRetTypes
      .get(EqWrap(m))
      .toRight(new ScopeStateException(s"Type for SMethod '${m.name}' is not found."))

  /**
   * Gets all symbols in the given scope
   */
  def symbolsFor(s: Scope): Set[Symbol] =
    scopeSymbols.getOrElse(EqWrap(s), Set.empty[Symbol])

  /**
   * Get Method Arguments Symbols
   */
  def methodArgSVars(m: SMethod): Either[Throwable, List[SVar]] =
    for
      _ <- Either.cond(
             symbolScopes.contains(EqWrap(m.asInstanceOf[Symbol])),
             (),
             new ScopeStateException(s"Cannot get argument symbols for SMethod: '${m.name}', method not found")
           )
      sVars = methodArgs.getOrElse(EqWrap(m), List.empty[SVar])
    yield sVars

  /**
   * Get Method Arguments Types
   */
  def methodArgTypes(m: SMethod): Either[Throwable, List[Type]] =
    for
      sVars <- methodArgSVars(m)
      types <- Transform.sequence(sVars.map(sVar => varTypes.get(EqWrap(sVar)).toRight(new ScopeStateException(s"Type for SVar ${sVar.name} is not found")))).map(_.toList)
    yield types

  /**
   * Gets a Map { SVar -> Type} for the arguments of a Method
   */
  def methodArgTypeMap(m: SMethod): Either[Throwable, Map[SVar, Type]] =
    for
      sVars <- methodArgSVars(m)
      types <- Transform
                 .sequence(
                   sVars.map { sVar =>
                     varTypes
                       .get(EqWrap(sVar))
                       .map(t => (sVar, t))
                       .toRight(new ScopeStateException(s"Type for SVar ${sVar.name} is not found"))
                   }
                 )
                 .map(_.toMap)
    yield types

  /**
   * Gets AST for the given method
   */
  def methodAst(m: SMethod): Either[Throwable, AST] =
    methodAsts.get(EqWrap(m)).toRight(new ScopeStateException(s"AST for the SMethod ${m.name} is not found"))

  /**
   * Returns types for the fields of the given struct.
   */
  def structTypes(sStruct: SStruct): Either[Throwable, Map[String, Type]] =
    Transform
      .sequence(symbolsFor(sStruct).toList.map { case (fs) =>
        fs match
          case x: SVar =>
            typeFor(x).left
              .map(ex => new ScopeStateException(s"Cannot find the type of the struct's field '${sStruct.name}.${x.name}'", ex))
              .map(t => x.name -> t)
          case other =>
            Left(new ScopeStateException(s"Expected struct field '${sStruct.name}.${fs.name}' to be SVar, got: '${other}'"))
      })
      .map(_.toMap)

  /**
   * Defines a Symbol in the given Scope
   */
  private def defineSymbolInScope(symbol: Symbol, scope: Scope): Meta =
    val (newScopeSymbols, newSymbolScopes) = addScopeSymbol(symbol, scope)
    this.copy(scopeSymbols = newScopeSymbols, symbolScopes = newSymbolScopes)

  private def defineSymbolScopeInScope(symbolWithScope: Symbol with Scope, scope: Scope): Meta =
    val (newScopeSymbols, newSymbolScopes) = addScopeSymbol(symbolWithScope, scope)
    this.copy(scopeTree = scopeTree.link(symbolWithScope, scope), scopeSymbols = newScopeSymbols, symbolScopes = newSymbolScopes)

  /**
   * Adds a Symbol to the provided Scope
   */
  private def addScopeSymbol(symbol: Symbol, scope: Scope): (Map[EqWrap[Scope], Set[Symbol]], Map[EqWrap[Symbol], Scope]) =
    val ss = scopeSymbols.getOrElse(EqWrap(scope), Set.empty[Symbol])

    val newScopeSymbols = scopeSymbols + (EqWrap(scope)  -> (ss + symbol))
    val newSymbolScopes = symbolScopes + (EqWrap(symbol) -> scope)

    (newScopeSymbols, newSymbolScopes)

  /**
   * Add a Var to the provided Method
   */
  private def addMethodArg(sMethod: SMethod, arg: SVar) =
    val args          = methodArgs.getOrElse(EqWrap(sMethod), List.empty[SVar])
    val newMethodArgs = methodArgs + (EqWrap(sMethod) -> (args :+ arg))
    newMethodArgs

  /**
   * Add a Type to the provided Method to specify the return type.
   */
  private def addMethodRetType(sMethod: SMethod, t: Type) =
    val newMethodRetType = methodRetTypes + (EqWrap(sMethod) -> t)
    newMethodRetType

  /**
   * Binds an AST to a Method. If a binding already exists, it will be overwritten
   */
  private def addMethodAST(sMethod: SMethod, ast: AST) =
    val newSymbolAsts = methodAsts + (EqWrap(sMethod) -> ast)
    newSymbolAsts

  /**
   * Binds a Scope to an AST
   */
  private def addASTScope(ast: AST, scope: Scope): Map[EqWrap[AST], Scope] =
    val newAstScopes = astScopes + (EqWrap(ast) -> scope)
    newAstScopes

  /**
   * Replace AST in { AST -> Scope } binding
   */
  private def replaceASTScope(oldAst: AST, newAst: AST): Map[EqWrap[AST], Scope] =
    val maybeScope = astScopes.get(EqWrap(oldAst))
    assert(maybeScope.isDefined, "Cannot replace AST in { AST -> Scope } binding. AST to replace is not found.")
    maybeScope.fold(astScopes) { scope =>
      val newAstScopes = astScopes - EqWrap(oldAst) + (EqWrap(newAst) -> scope)
      newAstScopes
    }

  /**
   * Bind Type to a variable
   */
  private def addVarType(t: Type, v: SVar): Map[EqWrap[SVar], Type] =
    val newVarTypes = varTypes + (EqWrap(v) -> t)
    newVarTypes

object Meta:

  val empty: Meta =
    Meta(
      scopeTree = ScopeTree.empty,
      scopeSymbols = Map.empty[EqWrap[Scope], Set[Symbol]],
      symbolScopes = Map.empty[EqWrap[Symbol], Scope],
      methodArgs = Map.empty[EqWrap[SMethod], List[SVar]], // NOTE: must be ordered, cannot use Set here
      methodRetTypes = Map.empty[EqWrap[SMethod], Type],
      methodAsts = Map.empty[EqWrap[SMethod], AST],
      astScopes = Map.empty[EqWrap[AST], Scope],
      varTypes = Map.empty[EqWrap[SVar], Type]
    )

  def init(types: Types): Meta =
    val g = SBlock("#global")
    val m = Meta.empty
      .defineBlock(g)
      .defineBuiltInType(types.autoType, g)
      .defineBuiltInType(types.nothingType, g)
      .defineBuiltInType(types.voidType, g)
      .defineBuiltInType(types.boolType, g)
      .defineBuiltInType(types.i32Type, g)
      .defineBuiltInType(types.i64Type, g)
      .defineBuiltInType(types.f32Type, g)
      .defineBuiltInType(types.f64Type, g)
      .defineBuiltInType(types.decType, g)
      .defineBuiltInType(types.strType, g)
      .defineBuiltInType(types.dateType, g)
      .defineBuiltInType(types.datetimeType, g)
    m

  given Show[Meta] with
    import ScopeTree.{ given, * }
    import com.github.gchudnov.bscript.lang.util.LineOps.*

    extension (a: Meta)
      def show: String =
        val sb = new StringBuilder
        sb.append("{\n")

        val treeStr           = tabTail(1, a.scopeTree.show)
        val scopeSymbolsStr   = tabTail(1, scopeSymbolsToMapStr(1)(a.scopeSymbols))
        val symbolScopesStr   = tabTail(1, symbolScopesToMapStr(1)(a.symbolScopes))
        val methodArgsStr     = tabTail(1, methodArgsToMapStr(1)(a.methodArgs))
        val methodRetTypesStr = tabTail(1, methodRetTypesToMapStr(1)(a.methodRetTypes))
        val methodAstsStr     = tabTail(1, methodAstsToMapStr(1)(a.methodAsts))
        val varTypesStr       = tabTail(1, varTypesToMapStr(1)(a.varTypes))
        val methodsStr        = tabTail(1, methodsToMapStr(1))

        sb.append(tabLine(1, s""""scopeTree": ${treeStr},\n"""))
        sb.append(tabLine(1, s""""scopeSymbols": ${scopeSymbolsStr},\n"""))
        sb.append(tabLine(1, s""""symbolScopes": ${symbolScopesStr},\n"""))
        sb.append(tabLine(1, s""""methodArgs": ${methodArgsStr},\n"""))
        sb.append(tabLine(1, s""""methodRetTypes": ${methodRetTypesStr},\n"""))
        sb.append(tabLine(1, s""""methodAsts": ${methodAstsStr},\n"""))
        sb.append(tabLine(1, s""""varTypes": ${varTypesStr},\n"""))
        sb.append(tabLine(1, s""""methods": ${methodsStr}\n"""))

        sb.append("}")
        sb.toString()

      private def scopeSymbolsToMapStr(depth: Int)(m: Map[EqWrap[Scope], Set[Symbol]]): String =
        namedTuplesToMapStr(depth)(m.toList.map { case (k, vs) =>
          (k.value.asInstanceOf[Named], vs.map(_.asInstanceOf[Named]))
        })

      private def methodArgsToMapStr(depth: Int)(m: Map[EqWrap[SMethod], List[SVar]]): String =
        namedTuplesToMapStr(depth)(m.toList.map { case (k, vs) =>
          (k.value.asInstanceOf[Named], vs.map(_.asInstanceOf[Named]))
        })

      private def symbolScopesToMapStr(depth: Int)(m: Map[EqWrap[Symbol], Scope]): String =
        namedTuples1ToMapStr(depth)(m.toList.map { case (k, v) =>
          (k.value.asInstanceOf[Named], v.asInstanceOf[Named])
        })

      private def methodRetTypesToMapStr(depth: Int)(m: Map[EqWrap[SMethod], Type]): String =
        namedTuples1ToMapStr(depth)(m.toList.map { case (k, v) =>
          (k.value.asInstanceOf[Named], v.asInstanceOf[Named])
        })

      private def varTypesToMapStr(depth: Int)(m: Map[EqWrap[SVar], Type]): String =
        namedTuples1ToMapStr(depth)(m.toList.map { case (k, v) =>
          (k.value.asInstanceOf[Named], v.asInstanceOf[Named])
        })

      private def methodAstsToMapStr(depth: Int)(m: Map[EqWrap[SMethod], AST]): String =
        namedTuples1ToMapStr(depth)(m.toList.map { case (k, v) =>
          (
            k.value.asInstanceOf[Named],
            new Named:
              val name: String = "(omitted)"
          )
        })

      private def namedTuples1ToMapStr(depth: Int)(xs: List[(Named, Named)]): String =
        val sb = new StringBuilder
        sb.append(s"{\n")

        val lines = xs
          .sortBy(_._1.name)
          .map { case (k, v) =>
            tabLine(depth, s"""${quote(k.name)}: ${quote(v.name)}""")
          }

        val body = lines.mkString(",\n")
        sb.append(body + (if body.nonEmpty then "\n" else ""))

        sb.append(s"}")
        sb.toString()

      private def namedTuplesToMapStr(depth: Int)(xs: List[(Named, Iterable[Named])]): String =
        val sb = new StringBuilder
        sb.append(s"{\n")

        val lines = xs
          .sortBy(_._1.name)
          .map { case (k, vs) =>
            tabLine(depth, s"""${quote(k.name)}: ${listStr(asList(vs.toList.sortBy(_.name)).map(quote))}""")
          }

        val body = lines.mkString(",\n")
        sb.append(body + (if body.nonEmpty then "\n" else ""))

        sb.append(s"}")
        sb.toString()

      private def methodsToMapStr(depth: Int): String =
        val sb = new StringBuilder
        sb.append(s"[\n")

        val ms = a.scopeTree.vertices
          .filter(_.value.isInstanceOf[SMethod])
          .map(it => EqWrap(it.value.asInstanceOf[SMethod]))
          .toList

        val lines = ms
          .sortBy(_.value.name)
          .map { m =>
            val retTypeStr = a.methodRetTypes.get(m).map(_.name).getOrElse("?")
            val args       = a.methodArgs.getOrElse(m, List.empty[SVar])
            val argsWithTypes = args.map { arg =>
              val retType = a.varTypes.getOrElse(EqWrap(arg), TypeRef("?"))
              s"${arg.name}: ${retType.name}"
            }

            tabLine(depth, quote(s"""fn ${m.value.name}(${argsWithTypes.mkString(", ")}) -> ${retTypeStr}"""))
          }

        val body = lines.mkString(",\n")
        sb.append(body + (if body.nonEmpty then "\n" else ""))

        sb.append(s"]")
        sb.toString()

      private def asList(ss: Iterable[Named]): List[String] =
        ss.map(_.name).toList

      private def listStr(xs: List[String]): String =
        xs.mkString("[", ",", "]")
