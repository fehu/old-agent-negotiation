## ExtendedConstraintBuilder



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
**< basic functionality > (+)  **
 
 there are cases when compiler cannot determine which overloaded method to use for wrapped value
 
 ```
 build[Int]{ implicit cw =>
   proposal > (my current value)
 }
 ```
 **< primitive types support > (+)  **
 
| ExtendedConstraintBuilder |
| Finished in 8 ms |
| 2 examples, 0 failure, 0 error |