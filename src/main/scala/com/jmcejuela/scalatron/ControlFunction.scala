package com.jmcejuela.scalatron

object MasterBot extends BotRespond {
  def apply(input: String) = ""
}

object CommandParser {
  def apply(command: String) = {
    def splitParam(param: String) = {
      val segments = param.split('=')
      if (segments.length != 2)
        throw new IllegalStateException("invalid key/value pair: "+param)
      (segments(0), segments(1))
    }

    val segments = command.split('(')
    if (segments.length != 2)
      throw new IllegalStateException("invalid command: "+command)

    val params = segments(1).dropRight(1).split(',')
    val keyValuePairs = params.map(splitParam).toMap
    (segments(0), keyValuePairs)
  }
}

trait Bot

trait BotRespond extends Function1[String, String]

class ControlFunctionFactory {
  def create = MasterBot//new ControlFunction().respond _
}

trait Action {
  override def toString: String
}

object Action {
  def apply(action: Action*) = action.mkString("|")
}