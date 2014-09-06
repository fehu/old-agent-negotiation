package feh.tec.agents.impl

import feh.tec.agents.macros.ExtendedConstraint
import feh.tec.agents.macros.ExtendedConstraint.CW
import scala.language.experimental.macros

object ExtendedConstraintBuilder{
  trait CBuilderKey

  object proposal extends CBuilderKey
  object value extends CBuilderKey
  object my{
    def current[T](v: value.type)(implicit wrap: CW[T]) = wrap(v)
  }

  /**
   * @return (proposal, value) => bool
   */
  def build[T](withWrapper: CW[T] => Boolean): (T, T) => Boolean = macro ExtendedConstraint.impl[T]
}

