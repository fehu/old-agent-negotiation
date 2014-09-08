Negotiating Agents
===
A Thesis Project in ITESM

### Negotiation Specification
[Example](misc/src/main/scala/feh/tec/agents/NQueen.scala):
```
class NQueenSpecification(boardSize: Int) extends impl.NegotiationSpecificationDSL{
  
  val x = variable `with` domain (1 to boardSize)
  val y = variable `with` domain (1 to boardSize)

  val `queen's position` = negotiation over (x, y)

  val Queen = agent withRole "chess queen" that(
    negotiates the `queen's position` `with` the.others and
      hasConstraints(
        "direct-line sight" |{
          proposed(x) != valueOf(x) && proposed(y) != valueOf(y)
        },
        "diagonal-line sight" |{
          proposed(x) != valueOf(y) && proposed(y) != valueOf(x)
        }
      )
    )

  spawn agents(
    Queen -> boardSize
    )

  configure(
    timeout.creation            <= 100.millis,
    timeout.`resolve conflict`  <= 100.millis
    )
}
```

see also [ExtendedConstraintBuilder](macros/test-reports/feh.tec.agents.ExtendedConstraintBuilderSpec.md)

##### todo:
  * [coloring](coloring/todo.md)
  * [comm](comm/todo.md)

####Running the queens' negotiation
 
 ```
 user:project_root> sbt
 > project misc
 > run
 
 ```
