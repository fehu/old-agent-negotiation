package feh.tec.agents.impl

import feh.tec.agents.{NegotiationSpecification => ANegotiationSpecification}

import scala.concurrent.duration.FiniteDuration
import scala.language.experimental.macros
import scala.reflect.runtime.{universe => ru}


class NegotiationSpecificationAdapter extends ANegotiationSpecification {
  import NegotiationSpecification._
  
  type Config = {
    def agentSpawn: Seq[SpawnDef]
    def configs: Seq[ConfigureDef]
  }

  type VarDef = this.type

  def negotiations = ???

  def config = ???

  def agents = ???

  def variables = ???

  type NegotiationDef = this.type
  type AgentDef = this.type
}


class NegotiationSpecificationExample extends NegotiationSpecificationDSL{

  define variable "v1" `with` domain (1 to 10)
  define variable "v2" `with` domain ( Set('A', 'B', 'C') )

  define negotiation "neg-1" over ("v1", "v2", "v5")

  define agent "ag-1" withRole "does something" that (
    negotiates the "neg-1" `with` ("role-1", "role-2")
    )
//  spawn agents(
//    "ag-1" -> 10
//    )
  
}

class NegotiationSpecificationExample2 extends NegotiationSpecificationDSL{


  val v1 = variable `with` domain (1 to 10)
  val v2 = variable `with` domain ( Set('A', 'B', 'C') )

  val neg1 = negotiation over (v1, v2)

  val ag1 = agent withRole "does something" that (
    negotiates the "neg-1" `with` neighbours and
      hasConstraints(
        "constraint 1" |{
          proposed(v1) / 2 != valueOf(v2)
        }
      )
    )


//  spawn agents(
//    "ag-1" -> 10
//    )

}


object Tst extends App{

  val spec = new NegotiationSpecificationExample2

//  spec.agents.foreach(println)

}
