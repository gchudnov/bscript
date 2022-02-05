package com.github.gchudnov.bscript.serde

private[serde] trait Serdes:
  lazy val string: Serde[Throwable, String] = Serde.from(identity)(identity)
