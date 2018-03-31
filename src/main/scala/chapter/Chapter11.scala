package chapters11

/**
 * プロコンのためのアルゴリズムとデータ構造(11章 動的計画法).
 */

/**
 * Chapter11 実装メイン.
 */
object Chapter11 {

  /**
   * Chapter11実施 メイン.
   */
  def process(args: Array[String]) = {
    args(1).toInt match {
      case 2 => {
        // フィボナッチ計算.
        val n = io.StdIn.readLine.trim.toInt
        var memo = new Array[Int](n)
        println(fib(n, memo))
      }
      case 3 => {
        // LCS(最長部分文字列問題.)
        val n = io.StdIn.readLine.trim.toInt
        val s = (1 to n).foldLeft(List[Pair[String, String]]())((acc, x) => {
          val s1: String = io.StdIn.readLine.trim
          val s2: String = io.StdIn.readLine.trim
          acc :+ (s1, s2)
        })

        // 入力文字列のLCSを算出.
        s.foreach({ case (s1, s2) => println(lcs(s1, s2)) })
      }
      case 4 => {
        // 連結行列積.
        val n = io.StdIn.readLine.trim.toInt
        val p = (1 to n).foldLeft(List[Pair[Int, Int]]())((acc, x) => {
          val m = io.StdIn.readLine.trim.split(' ')
          acc :+ (m(0).toInt, m(1).toInt)
        })
        println(mcm(p))
      }
      case _ => {
        println("not find command")
      }
    }
  }

  /**
   * フィボナッチ計算.
   *
   * [入力フォーマット]
   *   1行目：第N項のフィボナッチ数.
   */
  def fib(n: Int, m: Array[Int]): Int = {
    n match {
      case 0 | 1 => 1
      case _ => {
        if(0 == m(n - 2)) m(n - 2) = fib(n - 2, m)
        if(0 == m(n - 1)) m(n - 1) = fib(n - 1, m)
        m(n - 2) + m(n - 1)
      }
    }
  }

  /**
   * 最長部分列算出(Longest Common Subsequence).
   *
   * [入力フォーマット]
   *   １行目：データセットの数.
   *   ２行目以降：LCSを算出する文字列データセット.
   *
   * ＜入力例＞
   *   3
   *   abcbdab
   *   bdcaba
   *   abc
   *   abc
   *   abc
   *   bc
   */
  def lcs(s1: String, s2: String): Int = {
    if (s1.length == 0 || s2.length == 0) 0
    else if (s1.length > 0 && s2.length > 0 && s1(s1.length - 1) == s2(s2.length - 1)) {
      1 + lcs(s1.take(s1.length - 1), s2.take(s2.length - 1))
    }
    else {
      // lcs(i - 1, j)とlcs(i, j - 1)のどちらか大きいほうのLCSを返す.
      scala.math.max(lcs(s1.take(s1.length - 1), s2) , lcs(s1, s2.take(s2.length - 1)))
    }
  }

  /**
   * 連鎖行列積（Matrix Chain Multiplication）.
   *
   * @param Array[Pair[Int, Int]] 各行列の行と列を一次元配列に展開したもの.
   *
   * [入力フォーマット]
   *   １行目：行列の数
   *   ２行目：行列の次元（行、列）.
   *
   * ＜入力例＞
   *   6
   *   30 35
   *   35 15
   *   15 5
   *   5 10
   *   10 20
   *   20 25
   *
   * 15125
   */
  def mcm(p: List[Pair[Int, Int]]): Int = {
    p match {
      case (a: Int, b: Int) :: Nil => 0
      case _ => {
        // 行列の配列を分割しながら算出.
        (1 to p.length - 1).foldLeft(Int.MaxValue)((acc, x) => {
          val (a, b) = p.splitAt(x)
          scala.math.min(acc, mcm(a) + mcm(b) + a.head._1 * a.last._2 * b.last._2)
        })
      }
    }
  }
}

