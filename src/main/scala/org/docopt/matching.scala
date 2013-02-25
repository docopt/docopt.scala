package org.docopt.matching

import org.docopt.patterns._

object PatternMatcher {
  type SPat = Seq[Pattern]
  type MaybeMatch = scala.Option[Tuple2[SPat, SPat]]
  type MaybeChild = scala.Option[Tuple2[Int, Pattern]]

  def matchPattern(matcher: Pattern, left: SPat, collected: SPat = Nil): MaybeMatch = matcher match {
    case child:Argument => matchChildPattern(child, left, collected)
    case child:Command => matchChildPattern(child, left, collected)
    case child:CmdOption => matchChildPattern(child, left, collected)
    case req:Required => matchRequired(req, left, collected)
    case opt:Optional => matchOptional(opt, left, collected)
  }

  private def matchChildPattern(child: Pattern, left: SPat, collected: SPat): MaybeMatch = {
    val matched = child match {
      case a:Argument => matchArgument(a, left)
      case c:Command => matchCommand(c, left)
      case o:CmdOption => matchCmdOption(o, left)
    }
    matched match {
      case Some((pos, matched)) => {
        val left_ = left.slice(0, pos) ++ left.slice(pos+1, left.length)
        // TODO(fsaintjacques) fix this
        //val sameName = (for (a <- collected) yield a).toList

        Some((left_, collected ++ List(matched)))
      }
      case _ => None
    }
  }

  private def matchArgument(arg: Argument, left: SPat, index: Int = 0): MaybeChild =
    left match {
      case Nil => None
      case head :: tail => head match {
        case Argument(n, v) => Some(index, Argument(arg.name, v))
        case _ => matchArgument(arg, tail, index+1)
      }
    }

  private def matchCommand(cmd: Command, left: SPat, index: Int = 0): MaybeChild =
    left match {
      case Nil => None
      case head :: tail => head match {
        case Argument(n, StringValue(v)) if v == cmd.name =>
          Some(index, Command(cmd.name, BooleanValue(true)))
        case a:Argument => None
        case _ => matchCommand(cmd, tail, index+1)
      }
    }

  private def matchCmdOption(opt: CmdOption, left: SPat, index: Int = 0): MaybeChild =
    left match {
      case Nil => None
      case head :: tail => head match {
        case o:CmdOption if o.name == opt.name => Some(index, o)
        case _ => matchCmdOption(opt, tail, index+1)
      }
    }

  private def matchRequired(req: Required, left: SPat, collected: SPat): MaybeMatch = {
    val first:MaybeMatch = Some(left, collected)
    req.children.foldLeft(first) {
      case (Some((l, c)), child) => matchPattern(child, l, c)
      case (None, child) => None
    }
  }

  private def matchOptional(opt: Optional, left: SPat, collected: SPat): MaybeMatch = {
    val first:MaybeMatch = Some(left, collected)
    opt.children.foldLeft(first) {
      case (Some((l, c)), child) => matchPattern(child, l, c) match {
        // if not matched, still pass the previous (left, collected)
        case None => Some(l, c)
        case Some((l_, c_)) => Some(l_, c_)
      }
    }
  }
}
