package feh.tec.agents

import org.specs2.Specification
import feh.tec.agents.impl.ExtendedConstraintBuilder
import ExtendedConstraintBuilder._

class ExtendedConstraintBuilderSpec extends Specification { def is =
s2"""
${"ExtendedConstraintBuilder".title}

`ExtendedConstraintBuilder` allows building constraint functions in terms of `proposed` and `value`.

```
import ExtendedConstraintBuilder._

build[Any]{ implicit cw =>
  proposal == value
}

build[String]( implicit cw =>
  proposal.indexOf(value) >= 3
)
```
**< basic functionality >** ${test1 and test2}

there are cases when compiler cannot determine which overloaded method to use for wrapped value

```
build[Int]{ implicit cw =>
  proposal > (my current value)
}
```
**< primitive types support >** $test3
"""

  def test1 = {

    val f = build[Any]{ implicit cw =>
      proposal == ExtendedConstraintBuilder.value
    }

    (f("a", "a") must beTrue) and (f("a", "b") must beFalse)
  }
  
  def test2 = {

    val f = build[String]( implicit cw =>
      proposal.indexOf(ExtendedConstraintBuilder.value) >= 3
    )
    
    (f("qwerty", "q") must beFalse) and
      (f("qwerty", "e") must beFalse) and
      (f("qwerty", "r") must beTrue) and
      (f("qwerty", "y") must beTrue)
  }

  def test3 = {
    val f = build[Int]{ implicit cw =>
      proposal > (my current ExtendedConstraintBuilder.value)
    }

    f(1, 2) === false and f(2, 2) === false and f(2, 1) === true
  }
}
