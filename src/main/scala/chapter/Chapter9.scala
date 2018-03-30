package chapters9

import scala.language.implicitConversions
import tree._

/**
 * プロコンのためのアルゴリズムとデータ構造(9章 二分木)
 *
 * [入力フォーマット]
 * 一行目：命令数.
 * 二行目以降：命令を記載
 *
 * 命令例
 *   insert 10
 *     10の要素をインサート.
 *   delete 1
 *      1の要素を削除.
 *   find 2
 *      2の要素を検索
 *   print
 *      中間順巡回、先行順巡回の結果を出力.
 *
 * ＜入力例＞
 *   18
 *   insert 8
 *   insert 2
 *   insert 3
 *   insert 7
 *   insert 22
 *   insert 1
 *   find 1
 *   find 2
 *   find 3
 *   find 4
 *   find 5
 *   find 6
 *   find 7
 *   find 8
 *   print
 *   delete 3
 *   delete 7
 *   print
 */

/**
 * Chapter9.
 */
object Chapter9 extends Tree {

  /**
   * Chapter9実施 メイン.
   */
  def process(args: Array[String]) = {
    // 接点数分ループし、命令を取得.
    val num = io.StdIn.readLine().trim.toInt

    // 命令を実施.
    var t = new Tree
    def inst(x: String) = {
      val i = x.split(' ')
      // 命令を取得.
      i(0) match {
        case "insert" => t = TreeTrait.insert(i(1).toInt, t)
        case "find" =>  println(if (TreeTrait.search(i(1).toInt, t)) "yes" else "no")
        case "delete" => t = TreeTrait.delete(i(1).toInt, t)
        case "print" => {
          TreeTrait.inorder(t).foreach(printf("%s ", _))
          print("\n")
          TreeTrait.preorder(t).foreach(printf("%s ", _))
          print("\n")
        }
        case _ => printf("%s: commnad not found\n", i)
      }
    }

    // 命令を取得し、実行.
    (1 to num).foldLeft(List[String]())((acc, x) => acc :+ io.StdIn.readLine().trim).foreach(inst)
  }
}


