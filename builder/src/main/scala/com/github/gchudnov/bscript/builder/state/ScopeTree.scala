package com.github.gchudnov.bscript.builder.state

import com.github.gchudnov.bscript.builder.util.Tree
import com.github.gchudnov.bscript.builder.util.ReadTree
import com.github.gchudnov.bscript.builder.util.WriteTree

type ReadScopeTree  = ReadTree[Scope]
type WriteScopeTree = WriteTree[Scope]

type ScopeTree = Tree[Scope]
