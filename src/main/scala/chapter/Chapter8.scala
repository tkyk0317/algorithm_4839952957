package chapters8

import scala.language.implicitConversions

/**
 * プロコンのためのアルゴリズムとデータ構造(8章 木)
 *
 * [8-1から8-4] 入力フォーマット.
 *
 * 1行目：接点数
 * 2行目以降：接点番号 接続している接点番号 ...
 *
 * 接点が存在しない場合は、接点数を-1で入力.
 *
 * ＜入力例＞
 *   9
 *   0 1 4
 *   1 2 3
 *   2 -1 -1
 *   3 -1 -1
 *   4 5 8
 *   5 6 7
 *   6 -1 -1
 *   7 -1 -1
 *   8 -1 -1
 *
 * [8-5] 入力フォーマット.
 *
 * 1行目：接点数
 * 2行目：先行順巡回で取得できる接点番号
 * 3行目：中間順巡回で取得できる接点番号
 *
 * ＜入力例＞
 *   5
 *   1 2 3 4 5
 *   3 2 4 1 5
 */

/**
 * 木構造.
 */
sealed case class Node(index: Int, parent: Option[Int], left: Option[Int], right: Option[Int])
trait Tree {
  /**
   * 深さ算出.
   */
  @scala.annotation.tailrec
  final def depth(i: Int, t: Array[Node], m: Int = 0): Int =
    t(i).parent match {
      case None => m
      case _ => depth(t(i).parent.get, t, m + 1)
    }

  /**
   * 高さ算出.
   */
  def height(i: Int, t: Array[Node]): Int = {
    def h(x: Option[Int]): Int = x match { case Some(c) => 1 + height(c, t) case _ => 0 }
    List(t(i).left, t(i).right).map(h).max
  }

  /**
   * 接続数取得.
   */
  def degree(n: Node): Int = {
    List(n.left, n.right).flatten.length
  }

  /**
   * 根、左部分木、右部分木という順序で巡回.
   */
  def preorder(n: Int, t: Array[Node]): Unit = {
    printf("%s, ", n)
    t(n).left match {
      case Some(l) => preorder(l, t)
      case None => return
    }
    t(n).right match {
      case Some(r) => preorder(r, t)
      case None => return
    }
  }

  /**
   * 左部分木、根、右部分木という順序で巡回.
   */
  def inorder(n: Int, t: Array[Node]): Unit = {
    t(n).left match {
      case Some(l) => inorder(l, t)
      case None => {}
    }
    printf("%s, ", n)
    t(n).right match {
      case Some(r) => inorder(r, t)
      case None => {}
    }
  }

  /**
   * 左部分木、右部分木、根という順序で巡回.
   */
  def postorder(n: Int, t: Array[Node]): Unit = {
    t(n).left match {
      case Some(l) => postorder(l, t)
      case None => {}
    }
    t(n).right match {
      case Some(r) => postorder(r, t)
      case None => {}
    }
    printf("%s, ", n)
  }

  /**
   * 親接点番号取得.
   */
  def getParent(i: Int, t: Array[Node]): Option[Int] = {
    if (i == 0) None
    else t.find(n => { List(n.left, n.right).exists(_ == Some(i)) }).map(_.index)
  }

  /**
   * 先行巡回と中間巡回から 後行巡回を復元.
   */
  def restorePostOrder(p: List[Int], i: List[Int]): List[Int] = {
    if (i.length == 1 || p.length == 0)  i
    else {
      // 見つかった接点以外で左右分割.
      val (l, r) = i.splitAt(i.indexOf(p.head))
      restorePostOrder(p.tail, l) ++ // 左部分木.
      restorePostOrder(p.tail, if (r.length > 0) r.tail else List()) ++ // 右部分木.
      List(p.head) // 根.
    }
  }
}

/**
 * Chapter8.
 */
object Chapter8 extends Tree {

  // Listの要素をOptionにくるんで返す.
  implicit class DefaultValue[A](l: List[A]) {
    def someOrNone(i: Int): Option[A] = {
      try {
        if (l(i) == -1) None
        else Some(l(i))
      }
      catch {
        case e: java.lang.IndexOutOfBoundsException => None
      }
    }
  }

  /**
   * 第八章実行.
   */
  def process(args: Array[String]) = {
    // コマンドライン引数により、処理を分ける.
    if (args.length == 2) {
      args(1).toInt match {
        case 1 => process_1_4(args)
        //case 5 => process_5(args)
      }
    }
  }

  /**
   * 8-5の処理.
   */
  def process_5(args: Array[String]) = {
    // 接点数取得.
    val n = io.StdIn.readLine().trim.toInt
    val t = new Array[Node](n);

    // 先行巡回と中間巡回でのオーダーを取得.
    val preorder_node = io.StdIn.readLine().trim.split(' ').map(_.toInt).toList
    val inorder_node = io.StdIn.readLine().trim.split(' ').map(_.toInt).toList

    // 先行巡回の接点番号を一つづつ処理し、左右の部分木に中間巡回接点を分割していく.
    println(restorePostOrder(preorder_node, inorder_node))
  }

  /**
   * 8-1から8-4のまでの処理.
   */
  def process_1_4(args: Array[String]) = {
    // 接点数取得.
    val n = io.StdIn.readLine().trim.toInt

    // 木構造をノードのArrayとして表現.
    val t = new Array[Node](n);

    // 接点数数分、情報を取得.
    (1 to n) foreach(_ => {
      // 接点番号と接続数を取得.
      val i = io.StdIn.readLine().trim.split(' ').map(_.toInt).toList
      val c = i(0)

      // 接続ノード取得.
      val connection_node = i.slice(1, i.length)
      val l = connection_node.someOrNone(0)
      val r = connection_node.someOrNone(1)

      // ノード設定.
      t(c) = new Node(c, getParent(c, t), l, r)
    })

    // 木構造出力.
    t.foreach(x => {
      printf("node num: %s, ", x.index)
      printf("parent: %s, ", x.parent.getOrElse("-"))
      printf("left: %s, ", x.left.getOrElse("-"))
      printf("right: %s, ", x.right.getOrElse("-"))
      printf("degree: %s, ", degree(x))
      printf("height: %s, ", height(x.index, t))
      printf("depth: %s\n", depth(x.index, t))
    })

    // 巡回.
    println("preorder\n")
    preorder(0, t)
    printf("\ninorder\n")
    inorder(0, t)
    printf("\npostorder\n")
    postorder(0, t)
  }
}

