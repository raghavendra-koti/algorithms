package self.koti.algorithms

trait Entity {
  def apply(text: String): Set[String]
}

//"a"
case class Lit(s: String) extends Entity {
  def apply(text: String): Set[String] = if (text.startsWith(s)) Set(text.stripPrefix(s)) else Set.empty
}

//ab
case class Seq(a: Entity, b: Entity) extends Entity {
  def apply(text: String): Set[String] = b(a(text).head)
}

//a|b
case class Alt(a: Entity, b: Entity) extends Entity {
  def apply(text: String): Set[String] = a(text).union(b(text))
}

//a*
case class Star(a: Entity) extends Entity {
  def apply(text: String) = {
    val a1 = for (t1: String <- a(text) if !t1.equals(text);
                  t2: String <- Star(a)(t1))
      yield t2
    Set[String](text).union(a1)
  }
}

//a+
case class Plus(a: Entity) extends Entity {
  def apply(text: String) = {
    Seq(a, Star(a))(text)
  }
}

//a?
case class opt(a: Entity) extends Entity {
  def apply(text: String) = {
    Alt(Lit(""), a)(text)
  }
}

//.
case class Dot() extends Entity {
  def apply(text: String) = {
    if (text != null && text.trim() != "") {
      Set(text.substring(1))
    }
    else {
      Set.empty
    }
  }
}

//oneof
case class OneOf(chars: String) extends Entity {
  def apply(text: String) = {
    if (text != null && chars.contains(text(0))) {
      Set(text.substring(1))
    }
    else {
      Set.empty
    }
  }
}


object RegexSupport extends App {
  def matchString(entity: Entity, text: String): String = {
    val remainders = entity(text)
    if (remainders.nonEmpty) {
      val minRemainder = remainders.min(new Ordering[String] {
        override def compare(x: String, y: String): Int = {
          if (x.length == y.length)
            0
          else if (x.length > y.length)
            1
          else
            -1
        }
      })
      text.substring(0, text.length - minRemainder.length)
    }
    else {
      ""
    }
  }

  def search(entity: Entity, text: String): String = {
    val results = (for (i <- 0 to text.length)
      yield matchString(entity, text.substring(i))
      ).dropWhile(_ == "")

    if (results.isEmpty) "" else results.head
  }

  assert(Lit("wow")("wowme") == Set("me"))
  assert(Lit("wo")("woo") == Set("o"))

  println(Seq(Lit("hi "), Lit("there "))("hi there nice to meet you"))
  assert(Seq(Lit("hi "), Lit("there "))("hi there nice to meet you") == Set("nice to meet you"))
  assert(Alt(Lit("b"), Lit("a"))("ab") == Set("b"))
  assert(Star(Lit("hey"))("heyhey!") == Set("!", "heyhey!", "hey!"))
  assert(Star(Alt(Lit("a"), Lit("b")))("ab") == Set("ab", "b", ""))
  assert(Plus(Lit("hey"))("heyhey!") == Set("!", "hey!"))
  assert(opt(Lit("hey"))("heyhey!") == Set("hey!", "heyhey!"))
  assert(matchString(Star(Lit("a")), "aaaaabbbaa") == "aaaaa")
  assert(matchString(Lit("hello"), "hello how are you?") == "hello")
  assert(matchString(Lit("x"), "hello how are you?") == "")
  assert(matchString(OneOf("xyz"), "x**2 + y**2 = r**2") == "x")
  assert(matchString(OneOf("xyz"), "   x is here!") == "")
  assert(matchString(Star(Lit("a")), "aaabcd") == "aaa")
  assert(matchString(Lit("abc"), "abc") == "abc")
  assert(matchString(Alt(Lit("b"), Lit("c")), "ab") == "")
  assert(matchString(Alt(Lit("b"), Lit("a")), "ab") == "a")
  assert(Lit("abc")("abcdef") == Set("def"))
  assert(Seq(Lit("hi "), Lit("there "))("hi there nice to meet you") == Set("nice to meet you"))
  assert(Alt(Lit("dog"), Lit("cat"))("dog and cat") == Set(" and cat"))
  assert(Dot()("am i missing something?") == Set("m i missing something?"))
  assert(Dot()("") == Set.empty)
  assert(OneOf("a")("aabc123") == Set("abc123"))
  assert(OneOf("abc")("babc123") == Set("abc123"))
  assert(OneOf("abc")("dabc123") == Set.empty)
  assert(search(Lit(""), "") == "")
  assert(search(Alt(Lit("b"), Lit("c")), "ab") == "b")
  assert(search(Star(Alt(Lit("a"), Lit("b"))), "ab") == "ab")
  assert(search(Alt(Lit("b"), Lit("c")), "ad") == "")

}
