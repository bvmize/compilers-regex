// A version with simplification of derivatives;
// this keeps the regular expressions small, which
// is good for run-time
 

abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp 
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp 
case class STAR(r: Rexp) extends Rexp 
case class NTIMES(r: Rexp, n: Int) extends Rexp 



// nullable function: tests whether the regular 
// expression can recognise the empty string
def nullable (r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(_) => false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true
  case NTIMES(r, i) => if (i == 0) true else nullable(r)
}

// derivative of a regular expression w.r.t. a character
def der (c: Char, r: Rexp) : Rexp = r match {
  case ZERO => ZERO
  case ONE => ZERO
  case CHAR(d) => if (c == d) ONE else ZERO
  case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
  case SEQ(r1, r2) => 
    if (nullable(r1)) ALT(SEQ(der(c, r1), r2), der(c, r2))
    else SEQ(der(c, r1), r2)
  case STAR(r1) => SEQ(der(c, r1), STAR(r1))
  case NTIMES(r, i) => 
    if (i == 0) ZERO else SEQ(der(c, r), NTIMES(r, i - 1))
}

def simp(r: Rexp) : Rexp = r match {
  case ALT(r1, r2) => (simp(r1), simp(r2)) match {
    case (ZERO, r2s) => r2s
    case (r1s, ZERO) => r1s
    case (r1s, r2s) => if (r1s == r2s) r1s else ALT (r1s, r2s)
  }
  case SEQ(r1, r2) =>  (simp(r1), simp(r2)) match {
    case (ZERO, _) => ZERO
    case (_, ZERO) => ZERO
    case (ONE, r2s) => r2s
    case (r1s, ONE) => r1s
    case (r1s, r2s) => SEQ(r1s, r2s)
  }
  case r => r
}


// derivative w.r.t. a string (iterates der)
def ders (s: List[Char], r: Rexp) : Rexp = s match {
  case Nil => r
  case c::s => ders(s, simp(der(c, r)))
}


// main matcher function
def matcher(r: Rexp, s: String) : Boolean = nullable(ders(s.toList, r))


//one or zero
def OPT(r: Rexp) = ALT(r, ONE)


// Test Cases

//evil regular expressions: (a?){n} a{n}  and (a*)* b
def EVIL1(n: Int) = SEQ(NTIMES(OPT(CHAR('a')), n), NTIMES(CHAR('a'), n))
val EVIL2 = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))


def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start)/(i * 1.0e9)
}


//test: (a?{n}) (a{n})
for (i <- 1 to 7001 by 1000) {
  println(i + " " + "%.5f".format(time_needed(2, matcher(EVIL1(i), "a" * i))))
}

for (i <- 1 to 7001 by 1000) {
  println(i + " " + "%.5f".format(time_needed(2, matcher(EVIL1(i), "a" * i))))
}

//test: (a*)* b
for (i <- 1 to 6000001 by 500000) {
  println(i + " " + "%.5f".format(time_needed(2, matcher(EVIL2, "a" * i))))
}

for (i <- 1 to 6000001 by 500000) {
  println(i + " " + "%.5f".format(time_needed(2, matcher(EVIL2, "a" * i))))
}


// size of a regular expressions - for testing purposes 
def size(r: Rexp) : Int = r match {
  case ZERO => 1
  case ONE => 1
  case CHAR(_) => 1
  case ALT(r1, r2) => 1 + size(r1) + size(r2)
  case SEQ(r1, r2) => 1 + size(r1) + size(r2)
  case STAR(r) => 1 + size(r)
  case NTIMES(r, _) => 1 + size(r)
}


// now the size of the derivatives grows 
// much, much slower

size(ders("".toList, EVIL2))      // 5
size(ders("a".toList, EVIL2))     // 8
size(ders("aa".toList, EVIL2))    // 8
size(ders("aaa".toList, EVIL2))   // 8
size(ders("aaaa".toList, EVIL2))  // 8
size(ders("aaaaa".toList, EVIL2)) // 8





