package core

import scala.util.Random

trait Block {
  val x: Int
  val y: Int
  val occupation: List[(Int, Int)]
  val color: Int
}

class Tetromino(xc: Int, yc: Int, colorc: Int, anglec: Int, spinc: Int => List[(Int, Int)]) extends Block {
  val x: Int = xc
  val y: Int = yc
  val angle: Int = anglec
  val color: Int = colorc
  val spin: Int => List[(Int, Int)] = spinc
  override val occupation: List[(Int, Int)] = spin(angle).map(t => (x + t._1, y + t._2))
}

class MiniBlock(xc: Int, yc: Int, colorc: Int) extends Block {
  val x: Int = xc
  val y: Int = yc
  val color: Int = colorc
  override val occupation: List[(Int, Int)] = List((x, y))
}

trait Grid {
  val w: Int
  val h: Int
  val blocks: List[Block]
  val isValidPlace: Block => Boolean
  val placeBlock: Block => Grid
  val getBlockAt: (Int, Int) => Option[Block]
  val getClearedLines: () => List[Int]
  val clearLines: () => Grid
}

class ClassicGrid(xc: Int, yc: Int, bc: List[Block]) extends Grid {

  override val w: Int = xc
  override val h: Int = yc
  override val blocks: List[Block] = bc
  override val isValidPlace: Block => Boolean = (nb: Block) => {
    val err = nb.occupation.find(t =>
      t._2 < 1 ||
        t._1 < 0 ||
        t._1 > (w - 1))
    if (err.isDefined) false else {
      !nb.occupation.map(t => blocks.filter(b => {
        val old = b.occupation.head
        old == t
      })).exists(l => l.nonEmpty)
    }
  }
  override val placeBlock: Block => Grid = (nb: Block) => {
    val blocksToAdd = nb.occupation.map(t => new MiniBlock(t._1, t._2, nb.color))
    new ClassicGrid(w, h, blocksToAdd ++ blocks)
  }

  override val getBlockAt: (Int, Int) => Option[Block] = (x, y) => bc.find(b => b.x == x && b.y == y)

  override val getClearedLines: () => List[Int] = () => {
    val res2 = List[Int]()
    val i = 1 until (h + 1)
    val xs = 0 until (w + 1)
    val res = i.foldLeft(res2)((acc, y) => {
      val l = xs.map(x => isValidPlace(new MiniBlock(x, y, 8)))
      if (!l.exists(b => b))
        y :: acc
      else
        acc
    })
    res
  }

  def counter(f: () => Int): (Int, () => Int) = (f(), () => {
    1 + f()
  })

  def nextIndex(y: Int, cleared: List[Int]): Option[Int] =
    if (y >= h) None
    else if (cleared.contains(y)) nextIndex(y + 1, cleared)
    else Some(y)

  def shiftLines(y: Int, cleared: List[Int], acc: List[List[Block]]): List[Block] = {
    if (y >= h) acc.foldLeft(List[Block]())((res, l) => l ++ res)
    else if (!cleared.contains(y)) shiftLines(y + 1, cleared, blocks.filter(b => b.y == y) :: acc)
    else {
      val nextInd = nextIndex(y, cleared)
      nextInd match {
        case Some(v) => shiftLines(y + 1, v :: cleared, blocks.filter(b => b.y == v)
          .map(b => new MiniBlock(b.x, y, b.color)) :: acc)
        case None => acc.foldLeft(List[Block]())((res, l) => l ++ res)
      }
    }
  }

  override val clearLines: () => Grid = () => {
    val clearedLines = getClearedLines()
    val newBlocks = shiftLines(1, clearedLines, List[List[Block]]())
    val res = new ClassicGrid(w, h, newBlocks)
    println(res.getClearedLines())
    res
  }

}

object TetrisFuncs {

  var bag = List(Tetrominos.T(5, 20),
    Tetrominos.S(5, 20),
    Tetrominos.Z(5, 20),
    Tetrominos.L(5, 20),
    Tetrominos.J(5, 20),
    Tetrominos.I(5, 20),
    Tetrominos.O(5, 20))
  val r: Random.type = Random

  def takeFromBag(bag: List[Tetromino]): (Tetromino, List[Tetromino]) = {
    if (bag.isEmpty) {
      val nb = List(Tetrominos.T(5, 20),
        Tetrominos.S(5, 20),
        Tetrominos.Z(5, 20),
        Tetrominos.L(5, 20),
        Tetrominos.J(5, 20),
        Tetrominos.I(5, 20),
        Tetrominos.O(5, 20))
      return takeFromBag(nb)
    } else if (bag.size == 1) {
      return (bag.head, List())
    }

    val ind = r.nextInt(bag.size)
    val b = bag.apply(ind)
    val (t1, t2) = bag.splitAt(ind)
    (b, t1 ++ t2.tail)
  }

  def getNextBlock: () => Tetromino = () => {
    val (res, nbag) = takeFromBag(bag)
    bag = nbag
    res
  }

  def block_move(b: Tetromino, xd: Int, yd: Int): Tetromino =
    new Tetromino(b.x + xd, b.y + yd, b.color, b.angle, b.spin)

  def block_spin(b: Tetromino, ad: Int): Tetromino =
    new Tetromino(b.x, b.y, b.color, b.angle + ad, b.spin)


  def spinS(a: Int): List[(Int, Int)] = {
    val am = a % 4
    am match {
      case 0 => List((-1, 0), (0, 0), (0, 1), (1, 1));
      case 1 => List((0, 1), (0, 0), (1, 0), (1, -1));
      case 2 => List((1, 0), (0, 0), (0, -1), (-1, -1));
      case 3 => List((0, -1), (0, 0), (-1, 0), (-1, 1));
    }
  }

  def spinZ(a: Int): List[(Int, Int)] = {
    val am = a % 4
    am match {
      case 0 => List((-1, 1), (0, 0), (0, 1), (1, 0));
      case 1 => List((0, -1), (0, 0), (1, 0), (1, 1));
      case 2 => List((1, -1), (0, 0), (0, -1), (-1, 0));
      case 3 => List((-1, -1), (0, 0), (-1, 0), (0, 1));
    }
  }


  def spinO(a: Int): List[(Int, Int)] = {
    List((1, 1), (0, 0), (1, 0), (0, 1))
  }

  def spinI(a: Int): List[(Int, Int)] = {
    val am = a % 4
    am match {
      case 0 => List((-2, 1), (-1, 1), (0, 1), (1, 1));
      case 1 => List((1, 2), (1, 1), (1, 0), (1, -1));
      case 2 => List((-2, 0), (-1, 0), (0, 0), (1, 0));
      case 3 => List((0, 2), (0, 1), (0, 0), (0, -1));
    }
  }

  def spinJ(a: Int): List[(Int, Int)] = {
    val am = a % 4
    am match {
      case 0 => List((-1, 1), (-1, 0), (0, 0), (1, 0));
      case 1 => List((1, 1), (0, 1), (0, 0), (0, -1));
      case 2 => List((-1, 0), (0, 0), (1, 0), (1, -1));
      case 3 => List((0, 1), (0, 0), (0, -1), (-1, -1));
    }
  }

  def spinL(a: Int): List[(Int, Int)] = {
    val am = a % 4
    am match {
      case 0 => List((-1, 0), (1, 0), (0, 0), (1, 1));
      case 1 => List((1, -1), (0, 1), (0, 0), (0, -1));
      case 2 => List((-1, 0), (0, 0), (1, 0), (-1, -1));
      case 3 => List((0, 1), (0, 0), (0, -1), (-1, 1));
    }
  }

  def spinT(a: Int): List[(Int, Int)] = {
    val am = a % 4
    am match {
      case 0 => List((0, 1), (-1, 0), (0, 0), (1, 0));
      case 1 => List((1, 0), (0, 1), (0, 0), (0, -1));
      case 2 => List((0, -1), (0, 0), (1, 0), (-1, 0));
      case 3 => List((-1, 0), (0, 0), (0, -1), (0, 1));
    }
  }

}

object Tetrominos {
  def S(xc: Int, yc: Int, anglec: Int = 0) = new Tetromino(xc, yc, 1, anglec, TetrisFuncs.spinS)

  def Z(xc: Int, yc: Int, anglec: Int = 0) = new Tetromino(xc, yc, 2, anglec, TetrisFuncs.spinZ)

  def O(xc: Int, yc: Int, anglec: Int = 0) = new Tetromino(xc, yc, 3, anglec, TetrisFuncs.spinO)

  def L(xc: Int, yc: Int, anglec: Int = 0) = new Tetromino(xc, yc, 4, anglec, TetrisFuncs.spinL)

  def J(xc: Int, yc: Int, anglec: Int = 0) = new Tetromino(xc, yc, 5, anglec, TetrisFuncs.spinJ)

  def I(xc: Int, yc: Int, anglec: Int = 0) = new Tetromino(xc, yc, 6, anglec, TetrisFuncs.spinI)

  def T(xc: Int, yc: Int, anglec: Int = 0) = new Tetromino(xc, yc, 7, anglec, TetrisFuncs.spinT)
}