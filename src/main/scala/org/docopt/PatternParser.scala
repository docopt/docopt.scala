package org.docopt

import scala.util.matching.Regex
import scala.util.matching.Regex.Match
import scala.{Option => SOption}

import org.docopt.pattern._
import org.docopt.utils._

object PatternParser {
  type SeqOpt = List[Option]
  type Tokens = List[String]
  type ParseRet = (Tokens, SeqOpt, SeqPat)

  def parseDefault(default: String): Value = {
    val defaultPattern = """\[(?i)default: (.*)\]""".r
    defaultPattern.findFirstMatchIn(default) match {
      case Some(defaultPattern(v)) => StringValue(v)
      case None => NullValue()
    }
  }

  def parseArgument(argument: String): SOption[Argument] =
    """(<\S*?>)""".r.findFirstIn(argument).map(name => Argument(name, parseDefault(argument)))

  def parseOption(optionStr: String): SOption[Option] = {
    stringStrip(optionStr).split("  ").filter(_ != "") match {
      case Array(option, optDescription) => {
        val option_ = option.replace(",", " ").replace("=", " ").split(" ").filter(_ != "")
        val (short, long, argCount) = option_.foldLeft(("","",0)) {
          case (tup, token) => {
            token match {
              case tok if tok startsWith "--" => (tup._1, tok, tup._3)
              case tok if tok startsWith "-" => (tok, tup._2, tup._3)
              case _ => (tup._1, tup._2, 1)
            }
          }
        }
        val optionValue = if (argCount > 0) parseDefault(optDescription) else BooleanValue(value = false)
        Some(Option(short, long, argCount, optionValue))
      }
      // TODO(fsaintjacques): remove this lazy hack
      case Array(option) => parseOption(option + "  EmptyDescription")
      // TODO(fsaintjacques): verify if we shouldn't throw an exception
      case _ => None
    }
  }

  def parsePattern(source: String, options: SeqOpt, argv: Boolean = false): (SeqOpt, Pattern) = {
    val tokenizeRegex = new Regex("""([\[\]\(\)\|]|\.\.\.)""", "delim")
    val tokens = tokenStream(tokenizeRegex replaceAllIn
      (source, (m: Match) => " %s ".format(m.group("delim"))))
    val (tokens_, options_, results) = parseExpr(tokens, options, argv)
    if (tokens_.length > 0) throw new UnconsumedTokensException(tokens_)
    (options_, Required(results:_*))
  }

  // TODO(fsaintjacques): there is probably a more clean way of using
  // recursion
  def parseExpr(tokens: Tokens, options: SeqOpt, argv: Boolean): ParseRet = {
    def parseExprRecursive(toks: Tokens, opts: SeqOpt, res: SeqPat): ParseRet =
      toks match {
        case "|" :: tail =>
          val (toks_, opts_, seq_) = parseSeq(tail, opts, argv)
          parseExprRecursive(toks_, opts_, res ++ (if (seq_.length > 1) List(Required((seq_):_*)) else seq_))
        case _ =>
          (toks, opts, if (res.length > 1) List(Either(res:_*)) else res)
      }
    val ret@(tokens_, opts, seq) = parseSeq(tokens, options, argv)
    tokens_ match {
      case Nil => ret
      case head :: _ if head != "|" => ret
      case head :: tail => parseExprRecursive(tokens_, opts, if (seq.length > 1) List(Required(seq:_*)) else seq)
    }
  }

  val reservedTokens = Set("]", ")", "|")
  def parseSeq(tokens: Tokens, options: SeqOpt, argv: Boolean, results: SeqPat = Nil): ParseRet =
    tokens match {
      case head :: _ if (!(reservedTokens contains head)) =>
        val (tokens_, options_, atoms) = parseAtom(tokens, options, argv)
        tokens_ match {
          case "..." :: tail => parseSeq(tail, options_, argv, results ++ List(OneOrMore(atoms:_*)))
          case _ => parseSeq(tokens_, options_, argv, results ++ atoms)
        }
      case _ => (tokens, options, results)
    }

  def isBracketArgument(arg: String) =
   (arg.startsWith("<") && arg.endsWith(">")) || arg.forall(_.isUpper)

  def parseAtom(tokens: Tokens, options: SeqOpt, argv: Boolean): ParseRet =
    tokens match {
      case "(" :: tail =>
        val (tokens_, options_, expr) = parseExpr(tail, options, argv)
        tokens_ match {
          case ")" :: t => (t, options_, List(Required(expr:_*)))
          case _ => throw new MissingEnclosureException(")")
        }
      case "[" :: tail =>
        val (tokens_, options_, expr) = parseExpr(tail, options, argv)
        tokens_ match {
          case "]" :: t => (t, options_, List(Optional(expr:_*)))
          case _ => throw new MissingEnclosureException("]")
        }
      case "options" :: tail => (tail, options, List(AnyOptions()))
      case "--" :: tail => (tail, options, List(Command("--")))
      case "-" :: tail => (tail, options, List(Command("-")))
      case head :: tail if (head.startsWith("--")) => parseLongOption(tokens, options, argv)
      case head :: tail if (head.startsWith("-")) => parseShortOption(tokens, options, argv)
      case head :: tail if (isBracketArgument(head)) => (tail, options, List(Argument(head)))
      case head :: tail => (tail, options, List(Command(head)))
    }

  // TODO(fsaintjacques): change signature to modify options
  def parseLongOption(tokens: Tokens, options: SeqOpt, argv: Boolean = false): ParseRet =
    tokens match {
      case longToken :: tail => {
        val (long, valueName) = extractLongOptionValue(longToken)
        options.filter(_.long == long) match {
          case Nil =>
            val argcount = valueName match { case None => 0 case Some(_) => 1 }
            val o = Option("", long, argcount, if (argcount > 0) NullValue(null) else BooleanValue(false))
            val o_ = Option("", long, argcount, if (argcount > 0) StringValue(valueName.get) else BooleanValue(true))
            (tail, options ++ List(o), List(if (argv == true) o_ else o))
          case head :: Nil => {
            val o@Option(oLong, oShort, oArgcount, _) = head
            val (consumed, value) = oArgcount match {
              case 0 if valueName.isDefined => throw new UnexpectedArgumentException(longToken)
              case 1 if (valueName.isEmpty  && tail.isEmpty) => throw new MissingArgumentException(longToken)
              case 1 if valueName.isEmpty  => (true, tail.head)
              case _ => (false, valueName.getOrElse(""))
            }
            val value_ = if (argv == true && value == "") BooleanValue(value = true) else StringValue(value)
            val o_ = Option(oLong, oShort, oArgcount, value_)
            (if (consumed) tail.tail else tail, options, List(if (argv == true) o_ else o))
          }
          case _ =>
            throw new RuntimeException("option %s is not unique: %s".format(long, options))
        }
      }
      case _ =>
        throw new RuntimeException("parseLongOption requires at least one token")
    }

  def parseShortOption(tokens: Tokens, options: SeqOpt, argv: Boolean = false): ParseRet = {
    def parseShortOptionRecursive(tok: Seq[Char], toks: Tokens, opts: SeqOpt, ret: List[Pattern]): ParseRet =
      tok match {
        case Nil => (toks, opts, ret.reverse)
        case Seq(t, ok@_*) => {
          val short = "-%s".format(t)
          options.filter(short == _.short) match {
            case Nil =>
              val o = Option(short, "", 0)
              val o_ = Option(short, "", 0, BooleanValue(value = true))
              parseShortOptionRecursive(ok, toks, opts ++ List(o), (if (argv == true) o_ else o) :: ret)
            case Option(s, l, c, v) :: Nil => {
              c match {
                case 0 => parseShortOptionRecursive(ok, toks, options, Option(s,l,c,BooleanValue(value = if (argv == true) true else false)) :: ret)
                case 1 if (ok == Nil && toks.isEmpty) => throw new MissingArgumentException(short)
                case 1 if (ok == Nil) => parseShortOptionRecursive(ok, toks.tail, options, Option(s,l,c, if (argv) StringValue(toks.head) else v) :: ret)
                case 1 => (toks, options, (Option(s,l,c, if (argv) StringValue(ok.mkString) else v) :: ret).reverse)
              }
            }
            case head :: tail => throw new UnparsableOptionException(short)
          }
        }
      }
    parseShortOptionRecursive(tokens.head.substring(1), tokens.tail, options, Nil)
  }

  private def extractLongOptionValue(longOption: String) =
    if (longOption.exists(_ == '=')) {
      val Splitter = """^(.*?)=(.*)$""".r
      try {
        val Splitter(long, value) = longOption //val Array(long, value) = longOption.split("=")
        (long, Some(value))
      } catch {
        case _:Throwable => throw new UnparsableOptionException(longOption)
      }

    } else (longOption, None)


  private def expandPartialLongOption(longOption: String, options: SeqOpt): (String,  SOption[String]) = {
    val (long, value) = extractLongOptionValue(longOption)
    if (options.exists(_.long == long))
      (long, value)
    else
      options.filter(_.long.startsWith(long)) match {
        case Nil => (long, value)
        case option :: Nil => (option.long, value)
        case option :: tail => throw new RuntimeException("option %s is not unique: %s".format (longOption, options))
      }
  }

  private def isLongOption(long: String) = long match {
    case "--" => false
    case x if x.startsWith("--") => true
    case _ => false
  }

  private def clarifyLongOptionAmbiguities(argv: Tokens, options: SeqOpt): Tokens = argv match {
    case Nil => Nil
    case "--" :: tail => argv
    case head :: tail if isLongOption(head) =>
      val (long, value) = expandPartialLongOption(head, options)
      options.filter(_.long == long) match {
        case Nil => head :: clarifyLongOptionAmbiguities(tail, options)
        case Option(_, l, c, v) :: _ => c match {
          case 0 if value.isDefined => throw new UnexpectedArgumentException(long)
          case 0 => long :: clarifyLongOptionAmbiguities(tail, options)
          case 1 if value.isDefined => (long + "=" + value.get) :: clarifyLongOptionAmbiguities(tail, options)
          case 1 if tail.isEmpty => throw new MissingArgumentException(long)
          case 1 => (long + "=" + tail.head.mkString("")) :: clarifyLongOptionAmbiguities(tail.drop(1), options)
        }
      }
    case head :: tail => head :: clarifyLongOptionAmbiguities(tail, options)
  }

  //def parseArgv(argv: String, options: SeqOpt, optionFirst:Boolean = false) : (SeqOpt, SeqPat) = parseArgv(argv.split("""\s+"""), options, optionFirst)
  def parseArgv(argv: Array[String], options: SeqOpt, optionFirst:Boolean = false) : (SeqOpt, SeqPat) =
    parseArgvRecursive(clarifyLongOptionAmbiguities(argv.toList, options), options, optionFirst)

  private def parseArgvRecursive(tokens: Tokens, options: SeqOpt, optionFirst: Boolean, ret: List[Pattern] = Nil): (SeqOpt, SeqPat) =
    tokens match {
      case Nil => (options, ret.reverse)
      case "--" :: _ => (options, ret.reverse ++ tokens.map(t => Argument("", StringValue(t))))
      case head :: _ if head.startsWith("--") =>
        val (tokens_, options_, longs) = parseLongOption(tokens, options, argv = true)
        parseArgvRecursive(tokens_, options_, optionFirst, longs.toList ++ ret)
      case head :: _ if head.startsWith("-") && head != "-" =>
        val (tokens_, options_, shorts) = parseShortOption(tokens, options, argv = true)
        parseArgvRecursive(tokens_, options_, optionFirst, shorts.toList ++ ret)
      case head :: _ if optionFirst == true => (options, ret.reverse ++ tokens.map(t => Argument("", StringValue(t))))
      case head :: tail =>
        parseArgvRecursive(tail, options, optionFirst, Argument("", StringValue(head)) :: ret)
    }

  def parseOptionDescriptions(doc: String): List[Option] =
    (for (optionMatch <- """\n[\t ]*(-\S+[^\n]*)""".r .findAllIn(doc).matchData;
          option <- parseOption(optionMatch.group(1))) yield option).toList


  private def tokenStream(source: String, split: Boolean = true): Tokens =
    source.split("""\s+""").filter(_ != "").toList

  // keep only the Usage: part, remove everything after
  def printableUsage(doc: String): String = {
    val usages = """((?i)usage:)""".r.split(doc)
    usages.length match {
      case n if n < 2 => throw new DocoptLanguageException("'usage:' (case-insensitive) not found.")
      case n if n > 2 => throw new DocoptLanguageException("More than one 'usage:' (case-insensitive).")
      case _ => stringStrip(("""\n\s*\n""".r.split(usages(1)))(0))
    }
  }

  def formalUsage(doc: String): String = {
    val words = doc.split("\\s+")
    val programName = words.head
    "( " + words.tail.map(x => if (x == programName) ") | (" else x).mkString(" ") + " )"
  }

  def docopt(doc: String, argv: Array[String] = Array[String](), help: Boolean = true, version: String = "", optionsFirst: Boolean = false): SeqPat = {
    val usage = formalUsage(printableUsage(doc))
    val options = parseOptionDescriptions(doc)
    val (options_, pattern) = parsePattern(usage, options)
    val pattern_ = fixPattern(pattern, options_)
    val (options__, args) = parseArgv(argv, options_, optionsFirst)

    PatternMatcher.matchPattern(pattern_, args) match {
      case None => throw new DocoptExitException("pattern not matched")
      case Some((Nil, collected)) => flattenPattern(fixPattern(pattern, options__)) ++ collected
      case Some((left, collected)) => throw new UnconsumedTokensException(left.map(_.toString))
    }
  }
}
