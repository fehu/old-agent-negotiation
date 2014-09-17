package feh.tec.agents

import akka.actor.{Props, ActorSystem}
import akka.util.Timeout
import feh.tec.agents.impl._
import akka.pattern.ask
import feh.tec.agents.spec.dsl
import feh.tec.web.common.WebsocketConf
import scala.concurrent.duration._

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

object NQueenApp extends App{
  implicit val acSys = ActorSystem.create("NQueenApp")

  val spec = NQueen.spec(4)

  acSys.actorOf(Props(classOf[NQueenUserAgent], spec), "NQueenUserAgent")
}

class NQueenUserAgent(nspec: spec.NegotiationSpecification) extends UserAgent with WebsocketConf{
  def name = "user"

  implicit def acSys = context.system
  implicit def exContext = context.dispatcher

  println("spec.variables = " + nspec.variables)
  println("spec.negotiations = " + nspec.negotiations)
  println("spec.agents = " + nspec.agents)
  println("spec.config = " + nspec.config)

  val builder: ControllerBuilder[_] = new ControllerBuilder[GenericNegotiatingAgent](WebSocketInterface(web.server))

  if(wsConf.secure("n-queen")) sys.error("wss not supported")

  lazy val web: NQueenWebSocketPushServerBuilder = new NQueenWebSocketPushServerBuilder(
    wsConf.back.host("n-queen"),
    wsConf.back.port("n-queen"),
    negotiationId = builder.negotiations.head._2._1
  )
  val controller = builder(nspec)
  
  println("builder.vars = " + builder.vars)
  println("builder.negotiations = " + builder.negotiations)
  println("builder.agents = " + builder.agents)
  println("builder.agentsCount = " + builder.agentsCount)
  
  println("starting")
  controller ! SystemMessage.Start()

  implicit def timeout = Timeout(300 millis)

  def agFuture = (controller ? impl.NegotiationController.Request.AgentRefs()).mapTo[Seq[AgentRef]]

  agFuture.map{
    _ => {}
  }

}
