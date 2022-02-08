package com.github.gchudnov.bscript.builder

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.builder.internal.ScopeBuildVisitor
import com.github.gchudnov.bscript.builder.internal.ScopeResolveVisitor
import com.github.gchudnov.bscript.builder.internal.TypeCheckVisitor
import com.github.gchudnov.bscript.builder.internal.ScopeBuildVisitor.ScopeBuildState
import com.github.gchudnov.bscript.builder.internal.ScopeResolveVisitor.ScopeResolveState
import com.github.gchudnov.bscript.builder.internal.TypeCheckVisitor.TypeCheckState
import com.github.gchudnov.bscript.builder.internal.RootScope
import com.github.gchudnov.bscript.lang.util.Gen
import com.github.gchudnov.bscript.lang.types.{ TypeNames, Types }

sealed trait Builder:

  def build(ast0: AST, typeNames: TypeNames, typeCheckLaws: TypeCheckLaws): Either[Throwable, AstMeta] =
    val globalScopeName = "#global"
    val meta0           = RootScope.make(globalScopeName, typeNames)

    for
      scope0             <- meta0.scopeTree.get(globalScopeName).toRight(new Exception(s"Root scope '${globalScopeName}' not found"))
      scopeBuildVisitor   = ScopeBuildVisitor.make()
      scopeBuildState0    = ScopeBuildState.make(ast0, meta0, scope0, Gen.empty)
      scopeBuildState1   <- ast0.visit(scopeBuildState0, scopeBuildVisitor)
      scopeResolveVisitor = ScopeResolveVisitor.make(typeNames)
      (ast1, meta1)       = (scopeBuildState1.ast, scopeBuildState1.meta)
      scopeResolveState0  = ScopeResolveState.make(ast1, meta1)
      scopeResolveState1 <- ast1.visit(scopeResolveState0, scopeResolveVisitor)
      (ast2, meta2)       = (scopeResolveState1.ast, scopeResolveState1.meta)
      types              <- Types.make(meta2, typeNames)
      typeCheckVisitor    = TypeCheckVisitor.make(types, typeCheckLaws)
      typeCheckState0     = TypeCheckState.make(ast2, meta2)
      typeCheckState1    <- ast2.visit(typeCheckState0, typeCheckVisitor)
      (ast3, meta3)       = (typeCheckState1.ast, typeCheckState1.meta)
    yield AstMeta(meta = meta3, ast = ast3)

object Builder extends Builder
