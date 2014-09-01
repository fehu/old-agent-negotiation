Negotiating Agents
===
A Thesis Project in ITESM

### Negotiation Specification
[Example](comm/src/main/scala/feh/tec/agents/impl/NegotiationSpecification.scala):
```
  define variable "v1" `with` domain.range(1 to 10)
  define variable "v2" `with` domain.set('A', 'B', 'C')

  define negotiation "neg-1" over ("v1", "v2", "v5")

  define agent "ag-1" withRole "does something" that (
    negotiates the "neg-1" `with` ("role-1", "role-2") and
      hasConstraints.over(
        "v1" -> (proposed mustNot equal)
      )
    )
```