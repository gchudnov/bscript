package com.github.gchudnov.bscript.lang.ast.types

/**
 * Type Application in the AST
 *
 * Given type T and a list of types T1,T2..Tn, constructs T[T1,T2,...Tn]
 */
final case class Applied(aType: TypeAST, args: List[TypeAST]) extends TypeAST

/*
Map[Int, String]
Applied(TypeIdent("Map"), List(TypeIdent("Int"), TypeIdent("String")))

List[List[Int]]
Applied(TypeIdent("List"), List(Applied(TypeIdent("List"), List(TypeIdent("Int")))))

List[Int]
Applied(TypeIdent("List"), List(TypeIdent("Int")))
 */
