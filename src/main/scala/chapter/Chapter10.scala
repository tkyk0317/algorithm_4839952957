package chapters10

/**
 * プロコンのためのアルゴリズムとデータ構造(10章 ヒープ).
 */

object Chapter10 {

  /**
   * Chapter10実施 メイン.
   */
  def process(args: Array[String]) = {
    args(1).toInt match {
      case 2 => {
        val n = io.StdIn.readLine.trim.toInt
        val h = io.StdIn.readLine.trim.split(' ').map(_.toInt).toArray
        process2(h)
      }
      case _ => println("not found command")
    }
  }

   /**
    * 完全二分木.
    *
    * [入力フォーマット]
    *   １行目：ヒープサイズ.
    *   ２行目：ヒープの節点値
    *
    * ＜入力例＞
    *   5
    *   7 8 1 2 3
    */
   private def process2(h: Array[Int]) = {
     // 其々、1オリジン.
     def left(i: Int, l: Int): Option[Int] = if (l < 2 * i) None else Some(2 * i)
     def right(i: Int, l: Int): Option[Int] = if (l < 2 * i + 1) None else Some(2 * i + 1)
     def parent(i: Int): Option[Int] = if (i == 1) None else Some(scala.math.floor(i.toDouble / 2.0).toInt)

     // 節点の情報を出力.
     (1 to h.length).foreach(i => {
       printf("node %s key %s, ", i, h(i - 1))
       if (parent(i).isDefined) printf("parent key %s, ", h(parent(i).get - 1)) // 1オリジンなので、マイナス１しておく.
       if (left(i, h.length).isDefined) printf("left key %s, ", h(left(i, h.length).get - 1)) // 1オリジンなので、マイナス１しておく.
       if (right(i, h.length).isDefined) printf("right key %s, ", h(right(i, h.length).get - 1)) // 1オリジンなので、マイナス１しておく.
       println(" ")
     })
   }
}

