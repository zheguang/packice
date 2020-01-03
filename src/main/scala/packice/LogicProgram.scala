package packice

case class LogicProgram(exprs: Seq[LogicExpr]) {

  def prove(q: Query, initGc: GlobalContext): (LocalContext, List[GlobalContext]) = {
    val (renamedQuery, queryLc, queryGc) = q.rename(LocalContext(), initGc)

    val solutions: List[GlobalContext] = renamedQuery.cnf.foldRight(List[List[GlobalContext]](List(queryGc))) {
      case (renamedQueryTerm, accumulateSolutions) =>
        val partialSolutions: List[GlobalContext] = (for {
          gc1 <- accumulateSolutions.head
          expr <- exprs
          (renamedExpr, _, gc2) = expr.rename(LocalContext(), gc1)
          (unifiedQueryTerm, gc3) = renamedExpr.unify(renamedQueryTerm , gc2) // unify direction: expr -> query, to simplify propagation
          if unifiedQueryTerm.isDefined
        } yield {
          renamedExpr match {
            case Fact(_) => List(gc3)
            case Rule(_, right) =>
              val (_, gc4s) = prove(Query(right), gc3)
              gc4s
            case Query(cnf) => ???
          }
        }).flatten

        partialSolutions :: accumulateSolutions
    }.head

    // merge same solutions.  Same solutions may arise when having a rule f(X) :- g(X, Y), z(Y) where z(Y) can be a
    // rule too, and a query f(X).
    // There may be multiple valid Y that correspond to the same X.  But only X is in the original query.
    // todo: optimization: only find one qualified (X, Y), because the query does not depend on Y.
    val uniqueSolutions = solutions.toSet.toList
    (queryLc, uniqueSolutions)
  }

  def mkString: String =
    s"""
       |${exprs.map(_.mkString).mkString("\n")}
     """.stripMargin
}

object LogicProgram {
  def from(graphs: Seq[UniversalDependencyGraph]): LogicProgram = {

    case class GraphState(exprs: Seq[LogicExpr])

    val gss = graphs.foldLeft(Seq[GraphState]()) {
      case (graphStates, graph) =>
        val graphState = graph.edges.foldLeft(GraphState(Seq())) {
          case (state, Edge(depType, src, dst)) =>
            depType match {
              case NSubj =>
                GraphState(state.exprs ++ Seq(
                  Fact(Function(src.token, Seq(Constant(dst.token)))),
                  Fact(Function(dst.token, Seq(Constant(src.token))))
                ))
              case Cop =>
                require(dst.pos == VBZ)
                state
              case Punct =>
                require(dst.token == ".")
                state
            }
        }
        graphStates :+ graphState
    }

    val exprs = gss.flatMap(_.exprs)

    LogicProgram(exprs)
  }

  def queriesFrom(graphs: Seq[UniversalDependencyGraph]): Seq[Query] = {

    case class GraphState(exprs: Seq[Query])

    val gss = graphs.foldLeft(Seq[GraphState]()) {
      case (graphStates, graph) =>
        val graphState = graph.edges.foldLeft(GraphState(Seq())) {
          case (state, Edge(depType, src, dst)) =>
            depType match {
              case NSubj =>
                val q = (src.pos, dst.pos) match {
                  case (WP, _) => Query(Seq(Function(dst.token, Seq(LocalVariable("X")))))
                  case (_, WP) => Query(Seq(Function(src.token, Seq(LocalVariable("X")))))
                  case _ => ???
                }
                GraphState(state.exprs :+ q)
              case Cop =>
                require(dst.pos == VBZ)
                state
              case Punct =>
                require(dst.token == "?")
                state
            }
        }
        graphStates :+ graphState
    }

    val queries = gss.flatMap(_.exprs)

    queries
  }
}

case class LocalContext(localToGlobal: Map[LocalVariable, GlobalVariable]) {
  def add(v: LocalVariable, gc: GlobalContext): (GlobalVariable, LocalContext, GlobalContext) = {
    if (localToGlobal.contains(v)) {
      (localToGlobal(v), this, gc)
    } else {
      (gc.nextVariableId,
        LocalContext(localToGlobal + (v -> gc.nextVariableId)),
        gc + (gc.nextVariableId -> None))
    }
  }
}

object LocalContext {
  def apply(): LocalContext = LocalContext(Map())
}

case class GlobalContext(globalToTerm: Map[GlobalVariable, Option[Term]]) {
  def +(varAssignment: (GlobalVariable, Option[Term])): GlobalContext = {
    GlobalContext(globalToTerm + varAssignment)
  }

  val nextVariableId: GlobalVariable = GlobalVariable(s"""_G${globalToTerm.size}""")
}

object GlobalContext {
  def apply(): GlobalContext = GlobalContext(Map())
}

sealed trait Term {
  def unify(t2: Term, context: GlobalContext): (Option[Term], GlobalContext)
  def rename(lc: LocalContext, gc: GlobalContext): (Term, LocalContext, GlobalContext)
  def mkString: String
}

object Term {
  def renameAll(terms: Seq[Term], lc: LocalContext, gc: GlobalContext): (Seq[Term], LocalContext, GlobalContext) = {
    val (renamedTerms, renamedLc, renamedGc) = terms.foldRight(List[Term](), lc, gc) {
      case (t, (currTs, currLc, currGc)) =>
        val (renamedTerm, nextLc, nextGc) = t.rename(currLc, currGc)
        (renamedTerm :: currTs, nextLc, nextGc)
    }

    (renamedTerms, renamedLc, renamedGc)
  }
}

case class LocalVariable(name: String) extends Term {
  override def unify(t2: Term, context: GlobalContext): (Option[Term], GlobalContext) =
    throw LogicProgramError("should transform to var id first")

  override def rename(lc: LocalContext, gc: GlobalContext): (GlobalVariable, LocalContext, GlobalContext) = lc.add(this, gc)

  override def mkString: String = name
}

case class GlobalVariable(id: String) extends Term {
  override def unify(t2: Term, context: GlobalContext): (Option[Term], GlobalContext) = {
    assignment(context) match {
      case Some(x) =>
        require(x match {
          case GlobalVariable(id2) => id != id2
          case _ => true
        })
        unify(x, context)
      case None => t2 match {
        case v: LocalVariable => ???
        case v: GlobalVariable => (Some(this), context + (v -> Some(this)))
        case f: Function => (Some(f), context + (this -> Some(f)))
        case c: Constant => (Some(c), context + (this -> Some(c)))
      }
    }
  }

  override def rename(lc: LocalContext, gc: GlobalContext): (GlobalVariable, LocalContext, GlobalContext) = (this, lc, gc)

  def assignment(context: GlobalContext): Option[Term] = context.globalToTerm(this)

  override def mkString: String = id
}

case class Function(functor: String, args: Seq[Term]) extends Term {
  override def unify(t2: Term, context: GlobalContext): (Option[Term], GlobalContext) = t2 match {
    case LocalVariable(name) => ???
    case v: GlobalVariable => v.assignment(context) match {
      case Some(x) => unify(x, context)
      case None => (Some(this), context + (v -> Some(this)))
    }
    case Function(functor2, args2) =>
      if (functor == functor2) {
        val (unifiedArgs, unifiedContext) = args.zip(args2).foldRight((List[Option[Term]](), context)) {
          case ((arg, arg2), (currentArgs, currentContext)) =>
            val (newArg, nextContext) = arg.unify(arg2, currentContext)
            (newArg :: currentArgs, nextContext)
        }
        if (unifiedArgs.contains(None))
          (None, context)
        else
          (Some(Function(functor, unifiedArgs.flatten)), unifiedContext)
      } else
        (None, context)
    case Constant(value) => (None, context)
  }

  override def rename(lc: LocalContext, gc: GlobalContext): (Function, LocalContext, GlobalContext) = {
    val (renamedArgs, newLc, newGc) = args.foldRight((List[Term](), lc, gc)) {
      case (arg, (currentArgs, currentLc, currentGc)) =>
        val (newArg, nextLc, nextGc) = arg.rename(currentLc, currentGc)
        (newArg :: currentArgs, nextLc, nextGc)
    }
    (Function(functor, renamedArgs), newLc, newGc)
  }

  override def mkString: String = s"""$functor(${args.map(_.mkString).mkString(", ")})"""
}

case class Constant(value: String) extends Term {
  override def unify(t2: Term, context: GlobalContext): (Option[Term], GlobalContext) = t2 match {
    case LocalVariable(name) => ???
    case v: GlobalVariable =>
      v.assignment(context) match {
      case Some(x) => unify(x, context)
      case None => (Some(this), context + (v -> Some(this)))
    }
    case Function(functor, args) => (None, context)
    case Constant(value2) =>
      if (value == value2)
        (Some(this), context)
      else
        (None, context)
  }

  override def rename(lc: LocalContext, gc: GlobalContext): (Constant, LocalContext, GlobalContext) = (this, lc, gc)

  override def mkString: String = value
}

sealed trait LogicExpr {
  def rename(lc: LocalContext, gc: GlobalContext): (LogicExpr, LocalContext, GlobalContext)
  def unify(queryTerm: Term, gc: GlobalContext): (Option[Term], GlobalContext)
  def mkString: String
}

case class Fact(f: Term) extends LogicExpr {
  override def rename(lc: LocalContext, gc: GlobalContext): (Fact, LocalContext, GlobalContext) = {
    val (renamedTerm, renamedLc, renamedGc) = f.rename(lc, gc)
    (Fact(renamedTerm), renamedLc, renamedGc)
  }

  override def unify(queryTerm: Term, gc: GlobalContext): (Option[Term], GlobalContext) = f.unify(queryTerm, gc)

  override def mkString: String = f.mkString
}

case class Rule(left: Term, right: Seq[Term]) extends LogicExpr {
  override def rename(lc: LocalContext, gc: GlobalContext): (Rule, LocalContext, GlobalContext) = {
    val terms = left +: right
    val (renamedTerms, renamedLc, renamedGc) = Term.renameAll(terms, lc, gc)
    (Rule(renamedTerms.head, renamedTerms.tail), renamedLc, renamedGc)
  }

  override def unify(queryTerm: Term, gc: GlobalContext): (Option[Term], GlobalContext) = left.unify(queryTerm, gc)

  override def mkString: String = s"""$left :- ${right.map(_.mkString).mkString(", ")}"""
}

case class Query(cnf: Seq[Term]) extends LogicExpr {
  override def rename(lc: LocalContext, gc: GlobalContext): (Query, LocalContext, GlobalContext) = {
    val (renamedTerms, renamedLc, renamedGc) = Term.renameAll(cnf, lc, gc)
    (Query(renamedTerms), renamedLc, renamedGc)
  }

  override def unify(queryTerm: Term, gc: GlobalContext): (Option[Term], GlobalContext) = ???

  override def mkString: String = s"""?- ${cnf.map(_.mkString).mkString(", ")}"""
}

sealed trait Answer {
  def mkString: String
  def query: Query
}
case class Proof(query: Query, lc: LocalContext, gcs: List[GlobalContext]) extends Answer {
  require(gcs.nonEmpty)

  override def mkString: String = {
    val assignments = gcs.map {
      gc =>
        val localToTerm = lc.localToGlobal.mapValues {
          global => gc.globalToTerm(global)
        }

        val localToTermStrs = localToTerm.map {
          case (l, Some(t)) => l.mkString -> t.mkString
          case (l, None) => l.mkString -> None.mkString
        }

        s"""${localToTermStrs.mkString(", ")}"""
    }

    assignments.mkString("\n")
  }
}

case class Failure(query: Query) extends Answer {
  override def mkString: String = "False"
}

object Answer {
  def from(query: Query, lc: LocalContext, gcs: List[GlobalContext]): Answer = gcs match {
    case Nil => Failure(query)
    case _ => Proof(query, lc, gcs)
  }
}

case class LogicProgramError(msg: String) extends Exception
