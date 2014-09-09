Negotiating Agents
===
A Thesis Project in ITESM

### Negotiation Specification
[Example](misc/src/main/scala/feh/tec/agents/NQueen.scala):
```
object NQueen{
  def spec(boardSize: Int) = dsl.spec( 
    new dsl.Negotiation {
    
      var x = variable `with` domain (1 to boardSize)
      var y = variable `with` domain (1 to boardSize)
  
      def `queen's position` = negotiation over (x, y)
  
      def Queen = agent withRole "chess queen" that(
        negotiates the `queen's position` `with` the.others and
          hasConstraints(
            "direct-line sight" |{
              proposed(x) != valueOf(x) && proposed(y) != valueOf(y)
            },
            "diagonal-line sight" |{
              proposed(x) - valueOf(x) != proposed(y) - valueOf(y)
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
  })
}
```

[API](http://fehu.github.io/agent-negotiation/unidoc/package.html)

##### todo:
  * [coloring](coloring/todo.md)
  * [comm](comm/todo.md)

####Running the queens' negotiation
 
 ```
 user:project_root> sbt
 > project misc
 > run
 
 ```
