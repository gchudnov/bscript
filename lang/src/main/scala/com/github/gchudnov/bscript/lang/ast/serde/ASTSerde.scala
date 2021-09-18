package com.github.gchudnov.bscript.lang.ast.serde

trait ASTSerde {

//  def problem: Serde[Any, Problem] =
//    Serde(new ProblemDeserializer())(new ProblemSerializer())

}

object ASTSerde extends ASTSerde
