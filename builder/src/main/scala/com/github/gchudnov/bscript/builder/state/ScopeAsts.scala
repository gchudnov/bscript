package com.github.gchudnov.bscript.builder.state

import com.github.gchudnov.bscript.builder.util.BiDict
import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.lang.ast.decls.Decl
import com.github.gchudnov.bscript.builder.util.Ptr
import com.github.gchudnov.bscript.lang.util.Show
import ScopeAsts.given

/**
 * A Dictionary of Scope -> AST Interface
 *
 *   - a scope can have multiple ASTs
 *   - an AST can belong to only one scope
 *
 * NOTE: ASTs are not unique, so we use Ptr[AST] instead of AST
 */
sealed trait ScopeAsts:
  def isEmpty: Boolean
  def size: Int

  def link(scope: Scope, ast: AST): ScopeAsts

  def hasLink(scope: Scope, ast: AST): Boolean

  def scope(ast: AST): Option[Scope]
  def asts(scope: Scope): List[AST]

  def findDecl(name: String, scope: Scope): List[Decl]

  def asString: String

object ScopeAsts:
  lazy val empty: ScopeAsts =
    BasicScopeAsts(keyValues = Map.empty[Scope, Set[Ptr[AST]]], valueKey = Map.empty[Ptr[AST], Scope])

  given showAst: Show[AST] = new Show[AST]:
    override def show(a: AST): String =
      s"ast(${a.toString})"

  given showPtrAst: Show[Ptr[AST]] = new Show[Ptr[AST]]:
    override def show(a: Ptr[AST]): String =
      s"ptr(${showAst.show(a.value)})"

/**
 * A Dictionary of Scope -> AST Implementation
 */
private[state] final case class BasicScopeAsts(keyValues: Map[Scope, Set[Ptr[AST]]], valueKey: Map[Ptr[AST], Scope]) extends BiDict[Scope, Ptr[AST], BasicScopeAsts] with ScopeAsts:

  override def isEmpty: Boolean = 
    keyValues.isEmpty

  override def size: Int =
    keyValues.size

  override protected def clone(keyValues: Map[Scope, Set[Ptr[AST]]], valueKey: Map[Ptr[AST], Scope]): BasicScopeAsts =
    BasicScopeAsts(keyValues = keyValues, valueKey = valueKey)

  override def link(scope: Scope, ast: AST): ScopeAsts =
    set(scope, Ptr(ast))

  /**
   * Checks whether the given scope contains the given AST.
   */
  override def hasLink(scope: Scope, ast: AST): Boolean =
    contains(scope, Ptr(ast))

  override def scope(ast: AST): Option[Scope] =
    key(Ptr(ast))

  override def asts(scope: Scope): List[AST] =
    values(scope).map(_.value)

  /**
   * Find a declaration by name in the given scope
   *
   * @param name
   *   name of the declaration
   * @param scope
   *   scope
   * @return
   *   declaration
   */
  override def findDecl(name: String, scope: Scope): List[Decl] =
    asts(scope).collect {
      case x: Decl if x.name == name => x
    }

  override def asString: String =
    show
