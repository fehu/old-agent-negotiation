package feh.tec.agents.interactive

import feh.util._
import feh.util.scutil._
import feh.util.swing.completion.{CompletionListEditorPane, TextCompletion}
import scala.swing.{TextComponent, EditorPane}
import scala.tools.nsc.interpreter.Results

trait ScalaConsole{

  def executor: IMainRuTreesExecutor
  def completion: CompletionProvider
  def input: TextComponent
  def output: TextComponent
  
}

object ScalaConsole{
  class InOutEditPanes extends ScalaConsole with DefaultIMain{
    console =>

    lazy val executor = new DefaultIMainRuTreesExecutor {
      protected val iMain = console.iMain
    }
    lazy val completion = new ScalaCompletionProvider(iMain)

    import ConsoleCompletionEditor._

    lazy val input = new ConsoleCompletionEditor(completion)

    def output = new EditorPane // todo
  }

  implicit class ScalaConsoleOps(console: ScalaConsole){
    def executeInput(then: Results.Result => Unit) = console.executor.exec(console.input.text) |> then
  }

}


object ConsoleCompletionEditor{
  implicit def completionProviderToTextCompletion(peer: CompletionProvider): TextCompletion = new TextCompletion {
    def complete(context: String, pos: Int, verbosity: Option[Int]) = peer.complete(context, pos, verbosity)
  }
}

class ConsoleCompletionEditor[C <% TextCompletion](c: C) extends CompletionListEditorPane(c: TextCompletion)

trait ScalaConsoleProvider{
  def console: ScalaConsole
}

object ScalaConsoleProvider{
  trait InOutEditPanes extends ScalaConsoleProvider{
    lazy val console = new ScalaConsole.InOutEditPanes
  }
}