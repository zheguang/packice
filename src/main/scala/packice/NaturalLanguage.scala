package packice

import java.util.Properties
import edu.stanford.nlp.pipeline.StanfordCoreNLP

sealed abstract class PartOfSpeech(val name: String)
case object NN extends PartOfSpeech("NN")
case object NNP extends PartOfSpeech("NNP")
case object VBZ extends PartOfSpeech("VBZ")
case object JJ extends PartOfSpeech("JJ")
case object EOS extends PartOfSpeech(".")
case object WP extends PartOfSpeech("WP")

object PartOfSpeech {
  val tags = Seq(NN, NNP, VBZ, JJ, EOS, WP)
  val tagByName: Map[String, PartOfSpeech] = tags.map(t => (t.name, t)).toMap
}

object Pipeline {
  val props = new Properties()
  props.setProperty("annotators", "tokenize, ssplit, pos, lemma, ner, parse, depparse, mention, coref")
  val pipeline = new StanfordCoreNLP(props)
}
