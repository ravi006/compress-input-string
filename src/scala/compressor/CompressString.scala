package scala.compressor

class CompressString {

  def converter(f1: String, f2: String): String = if (f1 == "0") f2 else f1

  def finalConverter(f1: String, f2: String): String = if (f1 == "") f2 else f1 + " "

  def compress(list: List[String], initialValue: String, prevString: String, forwardString: String, finalString: String): String = list match {
    case t1 :: t2 => if(t1.toInt - prevString.toInt == 1) compress(t2, converter(initialValue, t1), t1, t1, finalString) else compress(t2, t1, t1, t1, finalConverter(finalString,"") + (initialValue+"-"+prevString))
    case Nil => finalString + " " + (initialValue+"-"+prevString)
  }


}

object CompressString {
  val comp = new CompressString()
  def main(args: Array[String]): Unit = {

    print(comp.compress(List("1","2","3","4","6","7","8","10","11"),"0","0","0",""))
  }
}
