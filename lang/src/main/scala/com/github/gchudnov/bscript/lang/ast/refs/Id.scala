package com.github.gchudnov.bscript.lang.ast.refs

final case class Id(name: String) extends Ref:
  /**
   * Path to the Id
   *
   * @return
   *   path to the identifier
   */
  override def path: List[String] =
    List(name)
