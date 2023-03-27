package u04lab.code

// Express a second degree polynomial
// Structure: secondDegree * X^2 + firstDegree * X + constant
trait SecondDegreePolynomial:
  def constant: Double
  def firstDegree: Double
  def secondDegree: Double
  def +(polynomial: SecondDegreePolynomial): SecondDegreePolynomial
  def -(polynomial: SecondDegreePolynomial): SecondDegreePolynomial


object SecondDegreePolynomial:
  def apply(secondDegree: Double, firstDegree: Double, constant: Double): SecondDegreePolynomial = 
    SecondDegreePolynomialImpl(secondDegree, firstDegree, constant)

@main def checkComplex(): Unit =
  val simplePolynomial = SecondDegreePolynomial(1.0, 0, 3)
  val anotherPolynomial = SecondDegreePolynomial(0.0, 1, 0.0)
  val fullPolynomial = SecondDegreePolynomial(3.0, 2.0, 5.0)
  val sum = simplePolynomial + anotherPolynomial
  println((sum, sum.secondDegree, sum.firstDegree, sum.constant)) // 1.0 * X^2 + 1.0 * X + 3.0
  val multipleOperations = fullPolynomial - (anotherPolynomial + simplePolynomial)
  println((multipleOperations, multipleOperations.secondDegree, multipleOperations.firstDegree, multipleOperations.constant)) // 2.0 * X^2 + 1.0 * X + 2.0
  //BEfore use case
  println(SecondDegreePolynomial(1.0, 0, 3) == SecondDegreePolynomial(1.0, 0, 3)) // false
  println(SecondDegreePolynomial(1.0, 0, 3) eq simplePolynomial) // false
  println(SecondDegreePolynomial(1.0, 0, 3).toString) // u04lab.code.SecondDegreePolynomialImpl@3796751b
  //After use case
  println(CaseClass(1.0, 0, 3) == CaseClass(1.0, 0, 3)) // true
  println(CaseClass(1.0, 0, 3) eq CaseClass(1.0, 0, 3)) // false
  println(CaseClass(1.0, 0, 3).toString) // CaseClass(1.0, 0, 3)

class SecondDegreePolynomialImpl(override val secondDegree: Double, override val firstDegree: Double,override val constant: Double) extends SecondDegreePolynomial:
  override def +(polynomial: SecondDegreePolynomial): SecondDegreePolynomial =
    SecondDegreePolynomial(this.secondDegree + polynomial.secondDegree, this.firstDegree + polynomial.firstDegree, this.constant + polynomial.constant)
  override def -(polynomial: SecondDegreePolynomial): SecondDegreePolynomial =
    SecondDegreePolynomial(this.secondDegree - polynomial.secondDegree, this.firstDegree - polynomial.firstDegree, this.constant - polynomial.constant)

case class CaseClass(secondDegree: Double, firstDegree: Double, constant: Double)