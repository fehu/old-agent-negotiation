package feh.tec.agents

import feh.tec.agents.impl._

import scala.concurrent.duration._

object NQueen{
//  val Size = 4

  def spec(boardSize: Int) = impl.NegotiationSpecification.build( new NegotiationSpecificationDSL {
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
  })
}

object NQueenApp extends App{

  val spec = NQueen.spec(4)

  println(spec)

//  implicit val acSys = ActorSystem.create("NQueenApp")
//  acSys.actorOf(Props(classOf[NQueenUserAgent]), "NQueenUserAgent")
}

/*
class NQueenUserAgent extends UserAgent{
  def name = "user"

  implicit def acSys = context.system
  implicit def exContext = context.dispatcher

  val spec = new NQueenSpecification(4)

  println("spec.variables = " + spec.variables)
  println("spec.negotiations = " + spec.negotiations)
  println("spec.agents = " + spec.agents)
  println("spec.config = " + spec.config)

  val builder = new ControllerBuilder[GenericNegotiatingAgentImpl]
  val controller = builder(spec)

  println("builder.vars = " + builder.vars)
  println("builder.negotiations = " + builder.negotiations)
  println("builder.agents = " + builder.agents)
  println("builder.agentsCount = " + builder.agentsCount)

  println("starting")
  controller ! SystemMessage.Start()

  implicit def timeout = Timeout(300 millis)

  def agFuture = (controller ? impl.NegotiationController.Request.AgentRefs()).mapTo[Seq[AgentRef]]

  agFuture.map{
    agents => controller ! Controller.ShowReportsGui(agents, silence = true, updateFreq = 200 millis)
  }

}*/
