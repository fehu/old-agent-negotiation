Negotiating Agents
===
A Thesis Project in ITESM

-[Project API](http://fehu.github.io/agent-negotiation/unidoc/package.html)-
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
-[Building Project](README.md#compiling-the-project)-

##### todo:
  * [coloring](coloring/todo.md)
  * [comm](comm/todo.md)
  * [web-frontend](web/frontend/todo.md)

### Coloring Negotiation (old)
*based on old __oldcomm__ package*

Agent negotiation over graph node colors, in order to run it, execute
```
user:project_root> sbt
> project coloring
> run
```

### Queens' negotiation

The negotiation is run by [GenericNegotiatingAgents](misc/src/main/scala/feh/tec/agents/GenericNegotiatingAgent.scala),
that is implemented upon [akka](http://akka.io) actors.

It is described by

#### Negotiation Specification
([source](misc/src/main/scala/feh/tec/agents/NQueen.scala)):
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
              (proposed(x) - valueOf(x)).abs != (proposed(y) - valueOf(y)).abs
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

#### Running the Negotiation

in sbt execute:
```
> project misc
> run
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

#### Frontend Generation

The front-end consists of
* javascript, [generated](web/frontend/src/main/scala/feh/tec/web/NQueen.scala) by scala-js
* it's dependencies
* [generated](web/frontend/src/main/scala/feh/tec/web/gen/NQueenTemplate.scala) template
* the associated [css](web/frontend/styles/n-queen)

Both [generation and packing](web/frontend/src/main/scala/feh/tec/web/util/PackTemplates.scala) are done by `pack-templates` sbt task in `project web-frontend`, the output directory is defined by `Web.packDir` setting key 
(`web/packed` by default).

There is also `clean-templates` task defined.

## Compiling the Project

The project depends on two of projects of mine:
* [util](https://github.com/fehu/util)
* [swing-dsl](https://github.com/fehu/swing-dsl) (for **coloring** only)

They are published in no public repository, so it’s needed to clone them and run the `publish-local` sbt task.
