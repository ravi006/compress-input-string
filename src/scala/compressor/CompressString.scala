package scala.compressor

class CompressString {

  def conditionCompressor(firstString: String, secondString: String) =
    if (secondString.toInt - firstString.toInt == 1) secondString else firstString

  def generateMap(list: List[String], map:Map[String, Int]) : Map[String, Int] = list match {
    case x :: y => if(map.keySet.contains(x)) generateMap(y, map ++ Map(x -> (map(x)+1))) else generateMap(y, map ++ Map(x -> 1))
    case Nil => map
  }

  def converter(f1: String, f2: String): String = if (f1 == "0") f2 else f1

  def finalConverter(f1: String, f2: String): String = if (f1 == "") f2 else f1 + " "

  def compress(list: List[String], initialValue: String, prevString: String, forwardString: String, finalString: String): String = list match {
    case head :: tail => if(head.toInt - prevString.toInt == 1) compress(tail, converter(initialValue, head), head, head, finalString) else compress(tail, head, head, head, finalConverter(finalString,"") + (initialValue+"-"+prevString))
    case Nil => finalString + " " + (initialValue+"-"+prevString)
  }


}

object CompressString {
  val comp = new CompressString()
  def main(args: Array[String]): Unit = {

    print(comp.compress(List("1","2","3","4","6","7","8","10","11"),"0","0","0",""))
  }
}
