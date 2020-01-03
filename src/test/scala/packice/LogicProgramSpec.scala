package packice

import org.scalatest.{FlatSpec, Matchers}

class LogicProgramSpec extends FlatSpec with Matchers {

  "LogicProgram" should "support simple fact" in {
    val prog = LogicProgram(Seq(
      Fact(Function("male", Seq(Constant("tom")))),
      Fact(Function("female", Seq(Constant("pam")))),
      Fact(Function("female", Seq(Constant("liz"))))
    ))

    val query = Query(Seq(Function("female", Seq(LocalVariable("X")))))
    val (lc, gc) = prog.prove(query, GlobalContext())

    assert(lc === LocalContext(Map(LocalVariable("X") -> GlobalVariable("_G0"))))
    assert(gc === List(GlobalContext(Map(GlobalVariable("_G0") -> Some(Constant("pam")))), GlobalContext(Map(GlobalVariable("_G0") -> Some(Constant("liz"))))))
  }

  it should "support simple rule" in {
    val prog = LogicProgram(Seq(
      Fact(Function("f", Seq(Constant("a")))),
      Fact(Function("f", Seq(Constant("b")))),

      Fact(Function("g", Seq(Constant("a")))),
      Fact(Function("g", Seq(Constant("b")))),

      Fact(Function("h", Seq(Constant("b")))),

      Rule(Function("k", Seq(LocalVariable("X"))),
        Seq(Function("f", Seq(LocalVariable("X"))), Function("g", Seq(LocalVariable("X"))), Function("h", Seq(LocalVariable("X"))))
      )
    ))

    val query = Query(Seq(Function("k", Seq(LocalVariable("Y")))))

    val (lc, gc) = prog.prove(query, GlobalContext())

    assert(lc === LocalContext(Map(LocalVariable("Y") -> GlobalVariable("_G0"))))
    assert(gc === List(GlobalContext(Map(GlobalVariable("_G0") -> Some(GlobalVariable("_G1")), GlobalVariable("_G1") -> Some(Constant("b"))))))
  }

  it should "support multiple solutions in rule" in {
    val prog = LogicProgram(Seq(
      Fact(Function("loves", Seq(Constant("bob"), Constant("mia")))),
      Fact(Function("loves", Seq(Constant("sam"), Constant("mia")))),
      Rule(Function("jealous", Seq(LocalVariable("A"), LocalVariable("B"))),
        Seq(Function("loves", Seq(LocalVariable("A"), LocalVariable("C"))),
          Function("loves", Seq(LocalVariable("B"), LocalVariable("C")))
        ))
    ))

    val query = Query(Seq(Function("jealous", Seq(LocalVariable("X"), LocalVariable("Y")))))

    val (lc, gc) = prog.prove(query, GlobalContext())

    assert(lc === LocalContext(Map(LocalVariable("Y") -> GlobalVariable("_G0"), LocalVariable("X") -> GlobalVariable("_G1"))))
    assert(gc === List(
      GlobalContext(Map(GlobalVariable("_G3") -> Some(Constant("bob")), GlobalVariable("_G4") -> Some(Constant("bob")), GlobalVariable("_G0") -> Some(GlobalVariable("_G3")), GlobalVariable("_G2") -> Some(Constant("mia")), GlobalVariable("_G1") -> Some(GlobalVariable("_G4")))),
      GlobalContext(Map(GlobalVariable("_G3") -> Some(Constant("bob")), GlobalVariable("_G4") -> Some(Constant("sam")), GlobalVariable("_G0") -> Some(GlobalVariable("_G3")), GlobalVariable("_G2") -> Some(Constant("mia")), GlobalVariable("_G1") -> Some(GlobalVariable("_G4")))),
      GlobalContext(Map(GlobalVariable("_G3") -> Some(Constant("sam")), GlobalVariable("_G4") -> Some(Constant("bob")), GlobalVariable("_G0") -> Some(GlobalVariable("_G3")), GlobalVariable("_G2") -> Some(Constant("mia")), GlobalVariable("_G1") -> Some(GlobalVariable("_G4")))),
      GlobalContext(Map(GlobalVariable("_G3") -> Some(Constant("sam")), GlobalVariable("_G4") -> Some(Constant("sam")), GlobalVariable("_G0") -> Some(GlobalVariable("_G3")), GlobalVariable("_G2") -> Some(Constant("mia")), GlobalVariable("_G1") -> Some(GlobalVariable("_G4"))))
    ))
  }
}
