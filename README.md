Negotiating Agents
===
A Thesis Project in ITESM

### Negotiation Specification
[Example](misc/src/main/scala/feh/tec/agents/NQueen.scala):
```
class NQueenSpecification(boardSize: Int) extends impl.NegotiationSpecificationDSL{
  
  val x = variable `with` domain (1 to boardSize)
  val y = variable `with` domain (1 to boardSize)

  define negotiation "queen's position" over (x, y)

  val Queen = agent withRole "chess queen" that(
    negotiates the "queen's position" `with` the.others and
      hasConstraints.over(
        x >> { _ => proposed != value },
        y >> { _ => proposed != value }
      )
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
