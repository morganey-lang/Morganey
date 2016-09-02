package me.rexim.morganey.ast.error

/**
  * Error that may happen during applying bindings to a term
  */
sealed trait BindingError

/**
  * Binding doesn't exist
  * @param name the name of non-existing binding
  */
case class NonExistingBinding(name: String) extends BindingError

/**
  * Bindings formed a loop. Such bindings are not allowed in Morganey.
  * If recursion is required the Y-combinator should be used.
  * @param loop list of binding names that form a loop in a corresponding order
  */
case class BindingLoop(loop: List[String]) extends BindingError
