import edu.stanford.nlp.pipeline.Annotation
import packice.{Answer, GlobalContext, LogicProgram, UniversalDependencyGraph}

/**
  * Created by zheguang on 7/5/17.
  */
object Main extends App {

  /*val text =
    """
      |A disk has width.
      |There are three disks.
      |The disk "d1" is 10 units wide.
      |The disk "d2" is 20 units wide.
      |The disk "d3" is 30 units wide.
      |There are three poles.
      |The pole "p1" is at the first position.
      |The pole "p2" is at the second position.
      |The pole "p3" is at the third position.
      |Initially, all disks are on the first pole, ordered from top to bottom as "d1", "d2", and "d3".
      |We can move a disk from a pole to another pole if the disk is on the top of the pole.
    """.stripMargin*/

  /*val text =
    """
      |"d1" is a disk of 10 units wide.
      |The disk "d1" is 10 units wide.
      |How many units wide is the disk "d1"?
      |The pole "p1" is at the first position.
      |At what position is the pole "p1"?
      |The disk "d1", disk "d2" and disk "d3" are on the pole "p1" and are ordered as "d1", "d2" and "d3".
      |A disk can move from a pole to another pole if it is the first disk on the pole.
    """.stripMargin*/

  /*val text =
    """
      |Pam is female.
      |Bob is male.
      |Liz is female.
      |Pam is a parent of Bob.
      |Bob is a parent of Liz.
      |Pam does not have a daughter.
      |Bob has a daughter.
      |Who has a daughter?
    """.stripMargin*/

  val descriptions =
    """
      |Pam is female.
      |Bob is male.
    """.stripMargin
  println(
    s"""
       |descriptions:
       |$descriptions
     """.stripMargin)

  val questions =
    """
      |Who is Tom?
    """.stripMargin
  println(
    s"""
       |questions:
       |$questions
     """.stripMargin)


  val descGraphs = UniversalDependencyGraph.from(descriptions)

  val prog = LogicProgram.from(descGraphs)
  println(
    s"""
      |logic program:
      |${prog.mkString}
    """.stripMargin
  )

  val questsGraphs = UniversalDependencyGraph.from(questions)
  val queries = LogicProgram.queriesFrom(questsGraphs)
  println(
    s"""
      |queries:
      |${queries.map(_.mkString).mkString("\n")}
    """.stripMargin
  )

  val answers = queries.map {
    query =>
      val (lc, gcs) = prog.prove(query, GlobalContext())
      Answer.from(query, lc, gcs)
  }
  println(
    s"""
       |answers:
       |${answers.map(a =>
         s"""
           |${a.query.mkString}
           |${a.mkString}
         """.stripMargin).mkString("\n")}
     """.stripMargin
  )
}


