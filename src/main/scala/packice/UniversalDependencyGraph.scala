package packice

import edu.stanford.nlp.ling.CoreAnnotations
import edu.stanford.nlp.ling.CoreAnnotations.{PartOfSpeechAnnotation, TokensAnnotation}
import edu.stanford.nlp.pipeline.Annotation
import edu.stanford.nlp.semgraph.SemanticGraphCoreAnnotations

import scala.collection.JavaConverters._
import scala.util.matching.Regex
import scala.util.parsing.combinator.{PackratParsers, RegexParsers}
import scala.util.parsing.input.CharSequenceReader

case class UniversalDependencyGraph(nodes: Set[Node], edges: Set[Edge])
object UniversalDependencyGraph {
  def apply(edges: Set[Edge]): UniversalDependencyGraph = UniversalDependencyGraph(
    edges.flatMap(e => Set(e.src, e.dst)),
    edges
  )

  def from(document: String): Seq[UniversalDependencyGraph] = {
    val sentencesAnnotation = new Annotation(document)

    Pipeline.pipeline.annotate(sentencesAnnotation)

    val sentences = sentencesAnnotation.get(classOf[CoreAnnotations.SentencesAnnotation]).asScala
    //val sentence = sentences.get(0)

    //val out = new PrintWriter(System.out)
    //pipeline.prettyPrint(annotation, out)

    val sentenceGraphs = for {
      sentence <- sentences
    } yield {
      //val tree = sentence.get(classOf[TreeCoreAnnotations.TreeAnnotation])
      //tree.pennPrint()

      val tokens = sentence.get(classOf[TokensAnnotation]).asScala

      val pos = tokens.map(token => token.getString(classOf[PartOfSpeechAnnotation]))
      //println(pos.mkString(" "))

      val values = tokens.map(token => token.value())
      //println(values.mkString(" "))

      val deps = sentence.get(classOf[SemanticGraphCoreAnnotations.BasicDependenciesAnnotation])
      //println(deps.toDotFormat())
      //println(deps.toPOSList)
      //println(deps.toList)

      val sentenceGraph = deps.toPOSList.trim

      println(sentenceGraph)
      val udg = UniversalDependencyGraphParser(sentenceGraph)
      println(udg.toString)
      udg
    }

    sentenceGraphs
  }
}

sealed abstract class DepType(val name: String)
case object NSubj extends DepType("nsubj")
case object Cop extends DepType("cop")
case object Punct extends DepType("punct")

object DepType {
  val types = Seq(NSubj, Cop, Punct)
  val typeByName: Map[String, DepType] = types.map(t => (t.name, t)).toMap
}

case class Node(token: String, pos: PartOfSpeech)
case class Edge(depType: DepType, src: Node, dst: Node)

object UniversalDependencyGraphParser extends RegexParsers with PackratParsers {
  case class ParserError(msg: String) extends Exception

  def apply(input: String): UniversalDependencyGraph =
    parse(graph, new PackratReader(new CharSequenceReader(input.trim))) match {
      case Success(result, _) => result
      case NoSuccess(msg, _) =>
        println(msg)
        throw ParserError(msg)
    }

  def graph: Parser[UniversalDependencyGraph] = rep(edge) ^^ { es => UniversalDependencyGraph(es.toSet) }

  def edge: Parser[Edge] = depType ~ "(" ~ node ~ "," ~ node ~ ")" ^^ { case d ~ _ ~ n1 ~ _ ~ n2 ~ _ => Edge(d, n1, n2) }

  def depType: Parser[DepType] = word ^^ { name => DepType.typeByName(name) }

  val word: Regex = """\w+""".r

  val punctuation: Regex = """[\.\?]""".r

  def partOfSpeech: Parser[PartOfSpeech] = ("""[A-Z]+""".r | punctuation) ^^ { name => PartOfSpeech.tagByName(name) }

  def node: Parser[Node] = (word | punctuation) ~ "/" ~ partOfSpeech ^^ {
    case w ~ _ ~ pos => Node(w, pos)
  }
}
