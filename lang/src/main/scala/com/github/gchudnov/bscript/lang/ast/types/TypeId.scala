package com.github.gchudnov.bscript.lang.ast.types

/**
 * Reference to a type definition with a given name
 * 
 * It could be either a MethodDecl, StructDecl, or TypeDecl
 */
final case class TypeId(name: String) extends TypeAST