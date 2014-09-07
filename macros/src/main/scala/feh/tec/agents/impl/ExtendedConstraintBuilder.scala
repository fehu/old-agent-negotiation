package feh.tec.agents.impl

import feh.tec.agents.macros.ExtendedConstraint
import feh.tec.agents.macros.ExtendedConstraint.CW
import scala.language.experimental.macros
import scala.reflect.runtime.{universe => ru}

object ExtendedConstraintBuilder{
  trait CBuilderKey

  object proposed extends CBuilderKey
  object value extends CBuilderKey
  object my{
    def current[T](v: value.type)(implicit wrap: CW[T]) = wrap(v)
  }

  trait BProvider[T]{
    /**
     * @return (`buildingFor`, ((proposal, value) => bool))
     */
    def build(withWrapper: CW[T] => Boolean): (T, T) => Boolean = macro ExtendedConstraint.impl[T]
  }


  trait Env{
    def proposed  = ExtendedConstraintBuilder.proposed
    def value     = ExtendedConstraintBuilder.value
    def my        = ExtendedConstraintBuilder.my
  }
}

