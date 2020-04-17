import fastparse._

package object scoogle {

  def search(q: String): List[Function] = {
    parse(q, QueryParser.query(_)) match {
      case Parsed.Success(query, _) =>
        Database.search(query)
      case Parsed.Failure(msg, _, _) =>
        println(s"Invalid query ($msg)")
        Nil
    }
  }

  def printResults(fs: List[Function]): Unit =
    fs.foreach(f => println(s"* ${f.classFQN}.${f.name}${f.sig}"))

}
