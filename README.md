Negotiating Agents
===
A Thesis Project in ITESM

### Negotiation Specification
[Example](misc/src/main/scala/feh/tec/agents/NQueen.scala):
```
class NQueenSpecification(boardSize: Int) extends impl.NegotiationSpecification{
  
  define variable "x" `with` domain.range(1 to boardSize)
  define variable "y" `with` domain.range(1 to boardSize)

  define negotiation "queen's position" over ("x", "y")

  define agent "Queen" withRole "chess queen" that(
    negotiates the "queen's position" `with` other("chess queen") and 
      hasConstraints.over(
        "x" -> (proposed mustNot equal),
        "y" -> (proposed mustNot equal)
      ) 
    )
  
  spawn agents(
    "Queen" -> boardSize
    )
}
```
