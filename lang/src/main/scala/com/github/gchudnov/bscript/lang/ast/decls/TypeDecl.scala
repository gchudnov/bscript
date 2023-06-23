package com.github.gchudnov.bscript.lang.ast.decls

/**
 * Type Definition, generics or built-in types
 *
 * {{{
 *   T in def func[T](...)`
 *   Int
 *   Float
 *   String
 * }}}
 */
final case class TypeDecl(name: String) extends Decl:
  override def symbolName: String = 
    s"${name}@type"
