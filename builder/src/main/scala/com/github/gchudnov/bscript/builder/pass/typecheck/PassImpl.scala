package com.github.gchudnov.bscript.builder.pass.typecheck

import com.github.gchudnov.bscript.builder.pass.Pass

/**
 * (3-PASS)
 *
 * Checks Type Safety and promotes data types.
 *
 * Languages typically have lots and lots of semantic rules. Some rules are runtime constraints (dynamic semantics), and some are compile-time constraints (static semantics).
 * Dynamic semantic rules enforce things like “no division by zero” and “no array index out of bounds.” Depending on the language, we can enforce some rules statically such as “no
 * multiplication of incompatible types.”
 *
 * We use general three-pass strategy:
 *
 * 1) In the first pass, the parser builds an AST.
 *
 * 2) In the second pass, a tree walker builds a scope tree and populates a symbol table.
 *
 * 3) I the he third pass over the AST - computes the type of each expression, promotes arithmetic values as necessary.
 */
private[builder] final class PassImpl() extends Pass:

  type In = InState

  type Out = OutState

  override def run(in: InState): OutState =
    val folder = Folder.make()

    val state0         = PassState.from(in)
    val state1         = folder.foldAST(state0, in.ast)

    val out = PassState.into(state1, in.ast)

    out
