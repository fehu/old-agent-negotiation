package feh.tec.agents.coloring.visual

import scala.concurrent.{ExecutionContext, Future}
import scala.xml.NodeSeq
import feh.tec.agents.comm.coloring.OneTryColoring.{StateInfo, GetStateInfo}
import feh.tec.agents.comm.coloring.GraphColoring
import feh.tec.agents.coloring.util.Name
import akka.util.Timeout
import feh.util._

class OneTryColoringStateInfoExtractor(env: GraphColoring, nodeUpdateResponseTimeout: Timeout)(implicit context: ExecutionContext) {

  def nodeAgentInfo(nameOpt: Option[Name]): Future[NodeSeq] = nameOpt map {
    name => env.agentController.askAny(name, GetStateInfo)(nodeUpdateResponseTimeout) map {
      case StateInfo(`name`, isActive, color, tries, proposal, acceptance, freeColors, neighboursColors) =>
        val (accepted, pending) = acceptance.span(_._2)
        <html>
          <table>
            <tr><td>name</td><td>{name.name}</td></tr>
            <tr><td>isActive</td><td>{isActive.toString}</td></tr>
            <tr><td>color</td><td>{color.map(_.stringRGB) getOrElse "None"}</td></tr>
            <tr><td>tries</td><td>{tries.map(_.stringRGB).mkString(", ")}</td></tr>
            <tr><td>proposal</td><td>{Option(proposal).map(_.stringRGB) getOrElse "None"}</td></tr>
            <tr><td>accepted</td><td>{accepted.keys.mkString(", ")}</td></tr>
            <tr><td>pending</td><td>{pending.keys.mkString(", ")}</td></tr>
            <tr><td>freeColors</td><td>{freeColors.map(_.stringRGB).mkString(", ")}</td></tr>
            <tr><td>neighbours</td><td>
              <table>
                {
                neighboursColors.map{case (n, cOpt) => <tr><td>{n.name}</td><td>{cOpt.map(_.stringRGB) getOrElse "None"}</td></tr>}
                }
              </table></td></tr>
          </table>
        </html>
    }
    //<tr><td></td><td>{}</td></tr>
  } getOrElse Future.successful(<html>No info</html>)

}
