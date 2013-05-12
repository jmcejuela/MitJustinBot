import scala.util.Random
import scala.Array.canBuildFrom
import scala.Option.option2Iterable

class ControlFunctionFactory {
  def create: String => String = input => {
    val (opcode,params) = CommandParser(input)
    if (opcode == "React") {
        (params("generation") match {
                    case "0" => MasterBot(params)
                    case _ => Missile(params)
                }).toString
        
    }
    else "Status(boring!)"
     Log(params.keys.mkString(",")).toString
  }
    
}

object MasterBot extends BotRespond {

  def apply(params:Map[String,String]): Action = {
    import Cell._
    val view = View(params("view"))
  
    val targets = (for {
            target <- List(Plant, Fluppet)
            offset <- view.offsetToNearest(target)
        } yield offset)

        val move = (
        if (targets.isEmpty) Move(XY.random)
        else {
            val nearestTarget = targets.minBy {_.distanceTo(XY.Zero)}
            val pref = nearestTarget.signum
            val dir = view.cellAtRelPos(pref) match {
                case Wall|Enemy|Poison|Snorg => view.randomAdjacent(Empty).getOrElse(XY.random)
                case _ => pref.signum
            }
            Move(dir)
        }
     )
     if (view.nearby(Set(Enemy, Snorg), 2).isEmpty) move
     else MultiAction(move, Spawn(view.randomAdjacent(Empty).getOrElse(XY.Zero), "missile", 100))
  }
}

object Missile extends BotRespond {
    def apply(params:Map[String,String]): Action = Explode(2)
}

object Cell {
  /** Cell whose content is occluded by a wall */
  val Occluded = '?'
  /** empty cell */
  val Empty = '_'
  /** Wall */
  val Wall = 'W'
  /** Bot (=master; yours, always in the center unless seen by a slave) */
  val Me = 'M'
  /** Bot (=master; enemy, not you) */
  val Enemy = 'm'
  /** Mini-bot (=slave, yours) */
  val Child = 'S'
  /** Mini-bot (=slave; enemy's, not yours) */
  val EnemyChild = 's'
  /** Zugar (=good plant, food) */
  val Plant = 'P' 
  /** Toxifera (=bad plant, poisonous) */
  val Poison = 'p' 
  /** Fluppet (=good beast, food animal) */
  val Fluppet = 'B' 
  /** (=bad beast, predator) */
  val Snorg = 'b' // Predator
}

object CommandParser {
  val opcodeCommand = """(\w+)\((.+)\)""".r
  def apply(command: String): (String, Map[String, String]) = {
    val opcodeCommand(opcode, params) = command
    (opcode,
      (for {
        p <- params split ','
        Array(name, value) = p split '='
      } yield (name, value)) toMap)
  }
}

trait Bot

trait BotRespond extends Function1[Map[String,String], Action]

case class Move(direction: XY) extends Action {
    override def toString = "Move(direction="+direction+")"
}

case class Explode(radius:Int = 2) extends Action {
    override def toString = "Explode(size="+radius+")"
}

case class Spawn(direction: XY, name: String, energy: Int) extends Action {
    override def toString = "Spawn(direction="+direction+",name="+name+",energy="+energy+")"
}

case class Log(msg: String) extends Action {
    override def toString = "Log(text="+msg+")"
}

case class Say(msg: String) extends Action {
    override def toString = "Say(text="+msg+")"
}

case class Status(msg: String) extends Action {
  override def toString = "Status(text="+msg+")"
}

case class State()

case class SetState(state: State) extends Action {
  override def toString = "Set()"
}

case class MultiAction(actions: Action*) extends Action {
    override def toString = actions.mkString("|")
}

trait Action {
  override def toString: String
}

//-------------------------------------------------------------------------

case class XY(x: Int, y: Int) {
  override def toString = x+":"+y

  def isNonZero = x != 0 || y != 0
  def isZero = x == 0 && y == 0
  def isNonNegative = x >= 0 && y >= 0

  def updateX(newX: Int) = XY(newX, y)
  def updateY(newY: Int) = XY(x, newY)

  def addToX(dx: Int) = XY(x + dx, y)
  def addToY(dy: Int) = XY(x, y + dy)

  def +(pos: XY) = XY(x + pos.x, y + pos.y)
  def -(pos: XY) = XY(x - pos.x, y - pos.y)
  def *(factor: Double) = XY((x * factor).intValue, (y * factor).intValue)

  def distanceTo(pos: XY): Double = (this - pos).length // Phythagorean
  def length: Double = math.sqrt(x * x + y * y) // Phythagorean

  def stepsTo(pos: XY): Int = (this - pos).stepCount // steps to reach pos: max delta X or Y
  def stepCount: Int = x.abs.max(y.abs) // steps from (0,0) to get here: max X or Y

  def signum = XY(x.signum, y.signum)

  def negate = XY(-x, -y)
  def negateX = XY(-x, y)
  def negateY = XY(x, -y)

  /** Returns the direction index with 'Right' being index 0, then clockwise in 45 degree steps. */
  def toDirection45: Int = {
    val unit = signum
    unit.x match {
      case -1 =>
        unit.y match {
          case -1 =>
            if (x < y * 3) Direction45.Left
            else if (y < x * 3) Direction45.Up
            else Direction45.UpLeft
          case 0 =>
            Direction45.Left
          case 1 =>
            if (-x > y * 3) Direction45.Left
            else if (y > -x * 3) Direction45.Down
            else Direction45.LeftDown
        }
      case 0 =>
        unit.y match {
          case 1  => Direction45.Down
          case 0  => throw new IllegalArgumentException("cannot compute direction index for (0,0)")
          case -1 => Direction45.Up
        }
      case 1 =>
        unit.y match {
          case -1 =>
            if (x > -y * 3) Direction45.Right
            else if (-y > x * 3) Direction45.Up
            else Direction45.RightUp
          case 0 =>
            Direction45.Right
          case 1 =>
            if (x > y * 3) Direction45.Right
            else if (y > x * 3) Direction45.Down
            else Direction45.DownRight
        }
    }
  }

  def rotateCounterClockwise45 = XY.fromDirection45((signum.toDirection45 + 1) % 8)
  def rotateCounterClockwise90 = XY.fromDirection45((signum.toDirection45 + 2) % 8)
  def rotateClockwise45 = XY.fromDirection45((signum.toDirection45 + 7) % 8)
  def rotateClockwise90 = XY.fromDirection45((signum.toDirection45 + 6) % 8)

  def wrap(boardSize: XY) = {
    val fixedX = if (x < 0) boardSize.x + x else if (x >= boardSize.x) x - boardSize.x else x
    val fixedY = if (y < 0) boardSize.y + y else if (y >= boardSize.y) y - boardSize.y else y
    if (fixedX != x || fixedY != y) XY(fixedX, fixedY) else this
  }
}

object XY {
  /** Parse an XY value from XY.toString format, e.g. "2:3". */
  def apply(s: String): XY = { val a = s.split(':'); XY(a(0).toInt, a(1).toInt) }

  val Zero = XY(0, 0)
  val One = XY(1, 1)

  val Right = XY(1, 0)
  val RightUp = XY(1, -1)
  val Up = XY(0, -1)
  val UpLeft = XY(-1, -1)
  val Left = XY(-1, 0)
  val LeftDown = XY(-1, 1)
  val Down = XY(0, 1)
  val DownRight = XY(1, 1)

  val AdjacentMatrix = List(Right, RightUp, Up, UpLeft, Left, LeftDown, Down, DownRight)

  def random: XY = XY(Random.nextInt(3) - 1, Random.nextInt(3) - 1)

  def fromDirection45(index: Int): XY = index match {
    case Direction45.Right     => Right
    case Direction45.RightUp   => RightUp
    case Direction45.Up        => Up
    case Direction45.UpLeft    => UpLeft
    case Direction45.Left      => Left
    case Direction45.LeftDown  => LeftDown
    case Direction45.Down      => Down
    case Direction45.DownRight => DownRight
  }

  def fromDirection90(index: Int): XY = index match {
    case Direction90.Right => Right
    case Direction90.Up    => Up
    case Direction90.Left  => Left
    case Direction90.Down  => Down
  }

  def apply(array: Array[Int]): XY = XY(array(0), array(1))
}

object Direction45 {
  val Right = 0
  val RightUp = 1
  val Up = 2
  val UpLeft = 3
  val Left = 4
  val LeftDown = 5
  val Down = 6
  val DownRight = 7
}

object Direction90 {
  val Right = 0
  val Up = 1
  val Left = 2
  val Down = 3
}

//-------------------------------------------------------------------------

case class View(cells: String) {
  val size = math.sqrt(cells.length).toInt
  val center = XY(size / 2, size / 2)

  def apply(relPos: XY) = cellAtRelPos(relPos)

  def indexFromAbsPos(absPos: XY) = absPos.x + absPos.y * size
  def absPosFromIndex(index: Int) = XY(index % size, index / size)
  def absPosFromRelPos(relPos: XY) = relPos + center
  def cellAtAbsPos(absPos: XY) = cells.charAt(indexFromAbsPos(absPos))

  def indexFromRelPos(relPos: XY): Int = indexFromAbsPos(absPosFromRelPos(relPos))
  def relPosFromAbsPos(absPos: XY) = absPos - center
  def relPosFromIndex(index: Int) = relPosFromAbsPos(absPosFromIndex(index))
  def cellAtRelPos(relPos: XY): Char = cells.charAt(indexFromRelPos(relPos))

  def offsetToNearest(c: Char) = {
    val matchingXY = cells.view.zipWithIndex.filter(_._1 == c)
    if (matchingXY.isEmpty)
      None
    else {
      val nearest = matchingXY.map(p => relPosFromIndex(p._2)).minBy(_.length)
      Some(nearest)
    }
  }

  def adjacent(c: Char): List[XY] =
    for { rel <- XY.AdjacentMatrix if cells(indexFromRelPos(rel)) == c } yield rel

  def randomAdjacent(c:Char): Option[XY] = {
    val adj = adjacent(c)
    if (adj.isEmpty) None
    else Some(adj(Random.nextInt(adj.size)))
  }

  def nearby(c: Set[Char], distance: Int) = {
    for {
        x <- -distance to distance
        y <- -distance to distance
        coord = XY(x,y) if c contains cellAtRelPos(coord)
    } yield coord
  }

}
