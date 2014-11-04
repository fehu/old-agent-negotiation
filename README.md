Negotiating Agents
===
A Thesis Project at *Monterrey Institute of Technology and Higher Education* ([ITESM](http:/itesm.mx/))

-[Project API](http://fehu.github.io/agent-negotiation/unidoc/package.html)

##### todo:
  * [comm](comm/todo.md)
  * [web-frontend](web/frontend/todo.md)

### Queens' negotiation (obsolete - needs update)

The negotiation is run by [GenericNegotiatingAgents](misc/src/main/scala/feh/tec/agents/GenericNegotiatingAgent.scala),
that is implemented upon [akka](http://akka.io) actors.

It is described by

#### Agent Specification (light)
Defines agent behaviour.

The `feh.tec.agents.light.impl.agent.create` object provides some specifications bases with very simple or stubbed behaviour.
The specification defines the aspects of agent's behavior as [ExtendableDefinition](comm-light/src/main/scala/feh/tec/agents/light/spec/ExtendableDefinition.scala),
    which can be extended/overridden using implicit methods from [create.Extendable***DefinitionWrapper](comm-light/src/main/scala/feh/tec/agents/light/impl/agent/create.scala).

Most of definitions are instances of *MonoDefinition* class and support following extension methods:
- **<:=** ― override main extension  point, provides a reference to the agent
- **:=**  ― override main extension  point, does NOT provide a reference to the agent
- **andThen** ― aggregate an expression to the main extension point
      
Some of the definitions extend also *ExtendableDefinition.BeforeAndAfter* trait, that provides two extra extension points.
  They can be overridden with methods **before** and **after**.

([source](misc/src/main/scala/feh/tec/agents/light/QueenSpec.scala)):
```scala
import feh.tec.agents.light.impl.agent.create

object QueenSpec extends create.PPI.AllVarsSpec{
  initialize after {
    ag => _ =>
      ag.negotiations.foreach(_.currentPriority update new Priority(1))
      ag.log.info("initialized")
  }
  
  start andThen {
    ag =>
      import ag._
      negotiations.foreach {
        neg =>
          if(neg.currentIterator.raw.isEmpty) neg.currentIterator update ag.newIterator(neg.id)
          log.info("Iterating the domain with " + neg.currentIterator())
          ag.setNextProposal(neg.id)
          //            ag.nextValues()
          //            val proposal = neg.currentProposal.getOrElse {
          //              Message.Proposal(Message.ProposalId.rand, neg.id, neg.currentPriority(), neg.currentValues())
          //            }
          neg.currentState update NegotiationState.Negotiating
          log.info(s"negotiation ${neg.id} started, scope = ${neg.scope()}")
          sendToAll(neg.currentProposal())
      }
  }

  def respondToProposal(msg: Message.Proposal)(implicit ag: create.PPI.Ag) = {
    import ag._
    msg match{
      case Message.Proposal(propId, negId, _, values) =>
        val response =
          if(msg.satisfiesConstraints) Message.Accepted(negId, propId, get(negId).currentPriority(), get(negId).currentValues())
          else Message.Rejected(negId, propId, get(negId).currentPriority(), get(negId).currentValues())
        priorities += msg.sender -> msg.priority
        respond(response)
    }
  }

  onProposal <:= {
    implicit ag =>{
      case msg if ag.myPriority isHigherThenOf msg => respondToProposal(msg)
      case msg if ag.myPriority isLowerThenOf  msg => respondToProposal(msg)
      case samePriority => ag.requestPriorityRaise(samePriority.negotiation)
    }
  }  
}
```

#### Negotiation Specification  (light)
Defines variables and negotiations, references *Agent Specification*, defines the number of agents to create.  

([source](misc/src/main/scala/feh/tec/agents/light/QueenNegotiationApp.scala)):
```scala

import feh.tec.agents.light.spec.dsl._
import impl.agent._
import scala.concurrent.duration._

def negController(boardSize: Int) = controller {
  new Negotiation {

    var x = variable `with` domain(1 to boardSize)
    var y = variable `with` domain(1 to boardSize)

    def `queen's position` = negotiation over(x, y)

    def Queen = agent withRole "chess queen" definedBy QueenSpec that (
      negotiates the `queen's position` `with` the.others reportingTo reporter.default and
        hasConstraints(
          "direct-line sight" | {
            /* Constraints that can be run independently for vars should be separated by && or ||, or defined separately */
            proposed(x) != valueOf(x) && proposed(y) != valueOf(y)
          },
          "diagonal-line sight" | {
            (proposed(x) - valueOf(x)).abs != (proposed(y) - valueOf(y)).abs
          }
        )
      )

    spawn agents (
      Queen -> boardSize
      )

    configure(
      timeout.creation <= 100.millis,
      timeout.`resolve conflict` <= 100.millis
    )
  }
}
```

#### Running the Negotiation

in sbt execute:
```
> project misc
> run
-- choose the app to run --
```

### Visualisation
 
The n-queen negotiation counts with web visualisation. The [frontend](web/frontend) is written in [scala-js](http://scala-js.org) and represents an html page with the generated javascript. It communicates with the [backend](web/backend), which is written using [spray](http://spray.io), by means of a websocket. 

The **backend** support is integrated in the current n-queen negotiation implementation.

The [common](web/common) sub-project contains the web-socket [configuration](web/common/src/main/resources/websocket.conf).
```
websocket.n-queen{
  secure: false                 // wss enabled
  front{
    host:   www.example.com/    // host for forntend to connect
    port:   80                  // port 
    path:   /ws/n-queen         // request path
  }
  back{
    host:   localhost           // backend host
    port:   8081                // backend port
  }
}
```

It is used in **backend** for configuring the port to listen and generating [nginx](http://nginx.org/) configuration.

To do the last one, execute `gen-nginx-ws` in `project web-backend`, it will create file `web/backend/nginx.conf-n-queen.ws`, that contains the websocket redirection configuration fragment for nginx.

In the **frontend**, the web-socket configuration file is used on template generation (the url is embedded in html and read by the script on startup).

For local development set 
```
  front{
    host:   localhost
    port:   8081                // as in `back`
    path:   /                   // no path
  }
```
This way the frontend web-socket will connect directly to the project's [spray-can](https://github.com/spray/spray-can) server, with no need for *nginx* setup. 

#### Visualisation Verification

To verify the accordance of the reports received by the front-end, run the following commands in *[javascript console](http://webmasters.stackexchange.com/questions/8525/how-to-open-the-javascript-console-in-different-browsers) of your browser*: 
 * table rows cache consistency: `QueensCommunications().diffArchiveAndCache(queens_array)`, where `queens_array` is an array with queen numbers, for example `[1, 2, 3, 4]`
 * current communication visualisation consistency: `QueensCommunications().diffArchiveAndTheTable(q1, q2)`, where `q1` and `q2` are the numbers of the two queens, whose communications are currently shown 

#### Frontend Generation

The front-end consists of
* javascript, [generated](web/frontend/src/main/scala/feh/tec/web/NQueen.scala) by scala-js
* it's dependencies
* [generated](web/frontend/src/main/scala/feh/tec/web/gen/NQueenTemplate.scala) template
* the associated [css](web/frontend/styles/n-queen)

Both [generation and packing](web/frontend/src/main/scala/feh/tec/web/util/PackTemplates.scala) are done by `pack-templates` sbt task in `project web-frontend`, the output directory is defined by `Web.packDir` setting key 
(`web/packed` by default). To package fully optimized js, execute `pack-templates-opt`. There is also `clean-templates` task defined.

