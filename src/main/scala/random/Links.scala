package random

object Links {

  def someRandomLinks(u: String): List[String] = {
    val l = List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k")
    val r = scala.util.Random
    if (r.nextInt(3) == 1) List.empty
    else scala.util.Random.shuffle(l).take(2)
  }

  val res: scala.collection.mutable.Set[String] =
    scala.collection.mutable.Set.empty[String]
  def findAllUniqueLinks(url: String): Set[String] = {
    val links = someRandomLinks(url)
    if (links.isEmpty) res.toSet
    else {
      links.flatMap { l =>
        res += l
        findAllUniqueLinks(l)
      }.toSet
    }
  }

  // This should probably be implemented as bfs with graph
  def findAllUniqueLinks1(url: String): Set[String] = {
    def find(urls: List[String], result: Set[String] = Set.empty[String]): Set[String] = {
      if (urls.isEmpty) result
      else urls.foldLeft(result){ (acc, u) =>
        (acc + u) ++ find(someRandomLinks(u))
      }
    }
    find(List(url))
  }

}
