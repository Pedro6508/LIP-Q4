object Main {
  def main(args: Array[String]): Unit = {
    val army = (1 to 5).map(JosephusProblem.Soldier).toList
    val k = 3

    JosephusProblem.solve(army, k).foreach {
      sol => println(s"Last soldier alive: ${sol.id}")
    }
  }

  object JosephusProblem {
    case class Soldier(id: Int)
    case class Message(x: Int, alive: List[Soldier], maybeLast: Option[Soldier]) {
      def pass(sol: Soldier)(implicit k: Int) = {
        val log = """[x = %02d] - Soldier[id = %02d] is""".format(x, sol.id)

        lazy val deadSender = {
          println(s"$log dead")
          Message(
            x + 1,
            alive,
            maybeLast
          )
        }
        lazy val aliveSender = {
          println(s"$log alive")

          Message(
            x + 1,
            sol :: alive,
            Some(sol)
          )
        }

        if (x % k == 0) deadSender
        else aliveSender
      }
    }

    // T(n) = T(n - n/k) + (n) + (n - n/k)
    // T(n) = T(n - n/k) + 2n - n/k
    // T(n) = T(n - n/k) + n(1 - 1/k)
    // T(n) = (T(...) + n*(1 - 1/k)^2) + n*(1 - 1/k)
    // T(n) = n*{1/[1 - (1 - 1/k)]}
    // T(n) = n*k
    def iter(list: List[Soldier], x: Int)(implicit k: Int): Option[Soldier] = {
      val message = // O(n) - message.alive.length = n - n/k
        list.foldLeft(Message(x, List(), list.headOption)) {
          (msg, sol) => msg.pass(sol)
        }

      lazy val loop = iter(
        message.alive.reverse, // O(n - n/k)
        message.x
      )
      lazy val result = message.maybeLast

      if (message.alive.isEmpty) result
      else loop // T(n - n/k)
    }

    def solve(army: List[Soldier], k: Int): Option[Soldier] = iter(army, 1)(k)
  }


}