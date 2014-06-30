package org.docopt

import scala.annotation.tailrec

object ExprMatcher {

  type MaybeMatch = scala.Option[(List[Expr],List[Expr])]
  type MaybeChild = scala.Option[(Int, Expr)]

  def matchExpr(matcher: Expr,
                left: List[Expr],
                collected: List[Expr] = Nil): MaybeMatch = matcher match {
    case arg:Argument=> matchArgument(arg, left, collected)
    case cmd:Command => matchCommand(cmd, left, collected)
    case opt:Option => matchOption(opt, left, collected)
    case req:Required => matchRequired(req, left, collected)
    case opt:Optional => matchOptional(opt, left, collected)
    case any:AnyOptions => matchOptional(Optional(any.children), left, collected)
    case eit:Either => matchEither(eit, left, collected)
    case orm:OneOrMore => matchOneOrMore(orm, left, collected)
    case _ => None
  }

  /* helper for tests */
  def matchExpr(matcher: Expr,
                left: Expr): MaybeMatch = matchExpr(matcher, left :: Nil)

  @tailrec
  private def matchArgument(arg: Argument,
                            left: List[Expr],
                            collected: List[Expr],
                            unmatched: List[Expr] = Nil): MaybeMatch =
    left match {
      case Nil => None
      case Command(n, _) :: tail =>
        Some(unmatched.reverse ::: tail, Argument(arg.name, n) :: collected)
      case head :: tail => matchArgument(arg, tail, collected, head :: unmatched)
    }

  @tailrec
  private def matchCommand(cmd: Command,
                           left: List[Expr],
                           collected: List[Expr],
                           unmatched: List[Expr] = Nil): MaybeMatch =
    left match {
      case Nil => None
      case Command(n, _) :: tail if n == cmd.name =>
        Some(unmatched.reverse ::: tail, Command(cmd.name, Value(value = true)) :: collected)
      case head :: tail => matchCommand(cmd, tail, collected, head :: unmatched)
    }

  @tailrec
  private def matchOption(opt: Option,
                          left: List[Expr],
                          collected: List[Expr],
                          unmatched: List[Expr] = Nil): MaybeMatch =
    left match {
      case Nil => None
      case (o:Option) :: tail if o.name == opt.name =>
        Some(unmatched.reverse ::: tail, o :: collected)
      case head :: tail => matchOption(opt, tail, collected, head :: unmatched)
    }

  private def matchRequired(req: Required,
                            left: List[Expr],
                            collected: List[Expr]): MaybeMatch =
    req.children.foldLeft(Some(left, collected):MaybeMatch) {
      case (Some((l, c)), child) => matchExpr(child, l, c)
      case (None, child) => None
    }

  private def matchOptional(opt: Optional,
                            left: List[Expr],
                            collected: List[Expr]): MaybeMatch =
    opt.children.foldLeft(Some(left, collected):MaybeMatch) {
      // this case should never happend, but we need to clear the warning
      case (None, child) => None
      case (Some((l, c)), child) => matchExpr(child, l, c) match {
      // if not matched, still pass the previous (left, collected)
      case None => Some(l, c)
      case Some((l_, c_)) => Some(l_, c_)
      }
    }

  private def matchOneOrMore(one: OneOrMore,
                             left: List[Expr],
                             collected: List[Expr]): MaybeMatch = {
    require(one.children.size == 1)
    val child = one.children(0)
    def whileMatch(p: Expr, l: List[Expr], c: List[Expr]): MaybeMatch =
      matchExpr(p, l, c) match {
        // first call and did not match anything
        case None if l.size == left.size => None
        // we matched previously but not anymore
        case None => Some(l, c)
        // Optional never returns None, stop looping if we don't collect
        case Some((l_, c_)) if l.size == l_.size => Some(l_, c_)
        case Some((l_, c_)) => whileMatch(p, l_, c_)
      }
    whileMatch(child, left, collected)
  }

  private def matchEither(either: Either,
                          left: List[Expr],
                          collected: List[Expr]): MaybeMatch =
    either.children.foldLeft(None:MaybeMatch) {
      case (maybe, child) =>
      matchExpr(child, left, collected) match {
        // this child doesn't match, keep passing the value
        case None => maybe
        // we match, still have work to do
        case newMatch:MaybeMatch => maybe match {
          // if previous child did not match, we're the new matcher
          case None => newMatch
          // an older child matched, but did it collect more element?
          case oldMatch:MaybeMatch =>
            if (newMatch.get._1.size < oldMatch.get._1.size) newMatch else oldMatch
        }
      }
    }

}
