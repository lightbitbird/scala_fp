package parsers

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Random
import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers
import java.time.{LocalDateTime, ZoneId}

object ChatBotMain extends App {
  val rc = ReplyCommand(".*".r, List("testing..."))
  rc.exec("hoge")

  val text = Source.fromFile("./src/main/scala/parsers/chatbot.txt").mkString
  val chatBot: ChatBot = ChatBotTextParser(text) match {
    case ChatBotTextParser.Success(result, _) => result
    case failure: ChatBotTextParser.NoSuccess => scala.sys.error(failure.toString)
  }
  println(s"chatBot: $chatBot")
  println(s"ChatBot booted.")

  @tailrec
  def checkInput(): Unit = {
    val input = scala.io.StdIn.readLine(">> ")
    if (input.startsWith("exit")) System.exit(0)

    @tailrec
    def execute(input: String, commands: List[Command]): Unit = {
      if (commands.nonEmpty && !commands.head.exec(input)) {
        execute(input, commands.tail)
      }
    }

    execute(input, chatBot.commands)
    checkInput()
  }

  checkInput()
}

case class ChatBot(commands: List[Command])

sealed trait Command {
  def exec(input: String): Boolean
}

case class ReplyCommand(regex: Regex, replies: List[String]) extends Command {
  override def exec(input: String): Boolean = {
    regex.findFirstIn(input) match {
      case Some(_) =>
        println(Random.shuffle(replies).head)
        true
      case None => false
    }
  }
}

case class TimeCommand(regex: Regex, start: Int, end: Int, zone: String, replies: List[String]) extends Command {
  override def exec(input: String): Boolean = {
    val currentHour = LocalDateTime.now().atZone(ZoneId.of(zone)).getHour
    val isInTime = start <= currentHour && currentHour <= end
    regex.findFirstIn(input) match {
      case Some(_) if isInTime =>
        println(Random.shuffle(replies).head)
        true
      case _ => false
    }
  }
}

/**
 * <chat-bot> ::= "(" "chatbot" <command-list> ")"
 * <command-list> ::= { <command> }
 * <command> ::= <reply-command>
 * <reply-command> ::= "(" "reply" <string-literal> <reply-list> ")"
 * <reply-list> ::= "(" { <string-literal> } ")"<chat-bot> ::= "(" "chatbot" <command-list> ")"
 * <command-list> ::= { <command> }
 * <command> ::= <reply-command>
 * <reply-command> ::= "(" "reply" <string-literal> <reply-list> ")"
 * <time-command> ::= "(" "time" <string-literal> <digits> <digits> <reply-list> ")"
 * <digits> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
 * <reply-list> ::= "(" { <string-literal> } ")"
 */
object ChatBotTextParser extends JavaTokenParsers {

  def apply(input: String): ParseResult[ChatBot] = parseAll(chatBot, input)

  def chatBot: Parser[ChatBot] = "(" ~ "chatbot" ~ commandList ~ ")" ^^ { t => ChatBot(t._1._2) }

  def commandList: Parser[List[Command]] = rep(command)

  def command: Parser[Command] = replyCommand | timeCommand

  def replyCommand: Parser[ReplyCommand] =
    "(" ~ "reply" ~ string ~ replyList ~ ")" ^^ {
      t => ReplyCommand(t._1._1._2.r, t._1._2)
    }

  def timeCommand: Parser[TimeCommand] =
    "(" ~ "time" ~ string ~ digits ~ digits ~ string ~ replyList ~ ")" ^^ { t =>
      TimeCommand(
        t._1._1._1._1._1._2.r,
        t._1._1._1._1._2.toInt,
        t._1._1._1._2.toInt,
        t._1._1._2,
        t._1._2)
    }

  def digits: Parser[String] = "[0-9]+".r

  def replyList: Parser[List[String]] = "(" ~ rep(string) ~ ")" ^^ { t => t._1._2 }

  def string: Parser[String] = stringLiteral ^^ { s => s.substring(1, s.length - 1) }
}

