package com.github.gchudnov.bscript.builder.pass

import com.github.gchudnov.bscript.lang.func.ASTFolder
import com.github.gchudnov.bscript.builder.state.*
import com.github.gchudnov.bscript.builder.env.*
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.decls.*
import com.github.gchudnov.bscript.lang.ast.lit.*
import com.github.gchudnov.bscript.lang.ast.types.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.builder.BuilderException
import com.github.gchudnov.bscript.lang.ast.refs.Access
import com.github.gchudnov.bscript.lang.ast.refs.Id
import com.github.gchudnov.bscript.lang.const.Const
import com.github.gchudnov.bscript.lang.types.TypeName

/**
 * #4 - Type Check Pass
 *
 *   - Checks the types of the AST nodes to see if they are matching.
 */
final class TypeCheckPass extends Pass[HasAST & HasReadEvalTypes, Unit]:

  override def run(in: HasAST & HasReadEvalTypes): Unit =
    val state0 = TypeCheckState.from(in.evalTypes)
    val ast0   = in.ast

    val folder = new TypeCheckFolder()

    val state1 = folder.foldAST(state0, ast0)

    val out = ()

    out

/**
 * Type Check Folder
 */
private final class TypeCheckFolder() extends ASTFolder[TypeCheckState]:

  override def foldAST(s: TypeCheckState, ast: AST): TypeCheckState =
    ast match
      case x: Access =>
        foldOverAST(s, x)

      case other =>
        foldOverAST(s, other)

      // case other =>
      //   throw new MatchError(s"Unsupported AST type in TypeResolveFolder: ${other}")

private final case class TypeCheckState(evalTypes: ReadEvalTypes) {
  
}

private object TypeCheckState {
  def from(evalTypes: ReadEvalTypes): TypeCheckState =
  TypeCheckState(
    evalTypes = evalTypes,
  )
}
