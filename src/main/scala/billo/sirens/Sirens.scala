package billo.sirens

/** Scala implementation of http://corporama.com/carabistouilles/sirens.html */
object Sirens {
  type Sirens = Map[String, Int]

  def parse(ss: List[String]): Sirens = {
    ss.foldLeft(Map.empty: Sirens) { (m, s) =>
      m.updated(s, m.get(s).map(_ + 1).getOrElse(1))
    }
  }

  def main(args: Array[String]) {
    val lines = scala.io.Source.fromURL(args.toList.head).getLines.toList
    val counts = lines.groupBy(l => l).mapValues(_.size)

    println(counts.filter(_._2 == 1).toList.length)
    println(counts.filter(_._2 >= 2).toList.length)
  }
}