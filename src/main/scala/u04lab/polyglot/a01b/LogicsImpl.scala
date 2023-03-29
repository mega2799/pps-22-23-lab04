package u04lab.polyglot.a01b

import scala.jdk.javaapi.OptionConverters
import u04lab.polyglot.OptionToOptional
import u04lab.code.Option
import u04lab.code.Stream
import u04lab.code.Stream.*
import u04lab.code.List
import u04lab.code.List.*
import u04lab.code.Option.*

import scala.util.Random
trait Pair:
  def x : Int
  def y : Int

private case class PairImpl(override val x : Int, override val y : Int) extends Pair

object Pair:
  def apply(x : Int, y : Int): Pair =
    PairImpl(x, y)


/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */
class LogicsImpl(private val size: Int, private val mines: Int) extends Logics:
  val rand = new Random()
  val listMines : List[Pair] = toList(Stream.map(Stream.take(iterate(Pair(0,0))((p : Pair)=> p))(mines))((p : Pair) =>  Pair(p.x + rand.nextInt(size), p.y + rand.nextInt(size))))
  var selected: List[Pair] = List.Nil()

//  println(listMines) // check where mines are
  def hit(x: Int, y: Int): java.util.Optional[Integer] =
    selected = append(List.cons(Pair(x, y), Nil()), selected)
    Pair(x, y) match
    case p if contains(listMines, p) eq true => OptionToOptional(None())
    case p => OptionToOptional(Some(neighbours(p)))

  def neighbours(p: Pair) : Int =
    (p.x - 1 to p.x + 1).flatMap(xx =>
      (p.y - 1 to p.y + 1).map(yy => Pair(xx, yy))
    ).count(p => List.contains(listMines, p))
  def won : Boolean = List.length(selected) + List.length(listMines) eq (size * size)
