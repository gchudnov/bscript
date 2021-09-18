package com.github.gchudnov.bscript.lang.ast.serde

private[serde] trait Serdes:
  lazy val string: Serde[Throwable, String] = Serde.from(identity)(identity)
