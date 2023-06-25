package com.github.gchudnov.bscript.lang.types


/**
 * Given type T and a list of types T1,T2..Tn, constructs T[T1,T2,...Tn]
 *
 * @param t
 *   type T
 * @param args
 *   type arguments
 */
final case class TApplied(t: TType, args: List[TType]) extends TType

/*
Map[Int, String]
Applied(TypeIdent("Map"), List(TypeIdent("Int"), TypeIdent("String")))

List[List[Int]]
Applied(TypeIdent("List"), List(Applied(TypeIdent("List"), List(TypeIdent("Int")))))

List[Int]
Applied(TypeIdent("List"), List(TypeIdent("Int")))
 */
