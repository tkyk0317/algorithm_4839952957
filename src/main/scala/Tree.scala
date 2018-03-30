package tree

/**
 * 木構造定義.
 */
class Tree
case class Leaf() extends Tree
case class Branch(value: Int, left: Tree, right: Tree) extends Tree

/**
 * 木構造Trait.
 */
object TreeTrait {

  /**
   * 深さ計算.
   *
   * @param: Tree.
   *
   * @return: 深さ.
   */
  def depth(t: Tree): Int = {
    t match {
      case Branch(_, l, r) => {
        scala.math.max(depth(l), depth(r)) + 1
      }
      case _ => 0
    }
  }

  /**
   * リストから木作成.
   */
  def createFromList(l: List[Int]): Tree = {
    l.foldLeft(new Tree)((acc, x) => { insert(x, acc) })
  }

  /**
   * 要素挿入.
   */
  def insert(v: Int, t: Tree): Tree = {
    t match {
      case Branch(k, l, r) => if (v < k) Branch(k, insert(v, l), r) else Branch(k, l, insert(v, r))
      case _ => Branch(v, Leaf(), Leaf())
    }
  }

  /**
   * 探索.
   *
   * @param k: 探索キー.
   */
  def search(v: Int, t: Tree): Boolean = {
    t match {
      case Branch(k, l, r) => {
        if (v == k) true
        else if(v < k) search(v, l)
        else if(v > k) search(v, r)
        else false
      }
      case _ => false
    }
  }

  /**
   * 先行順巡回.
   */
  def preorder(t: Tree): List[Int] = {
    t match {
      case Branch(v, l, r) => List(v) ++ preorder(l) ++ preorder(r)
      case _ => List[Int]()
    }
  }

  /**
   * 中間順巡回.
   */
  def inorder(t: Tree): List[Int] = {
    t match {
      case Branch(v, l, r) => inorder(l) ++ List(v) ++ inorder(r)
      case _ => List[Int]()
    }
  }

  /**
   * 最小値削除.
   */
  def deleteMin(t: Tree): Tree = {
    t match {
      case Branch(_, Leaf(), Leaf()) => Leaf()
      case Branch(_, l, _) => deleteMin(l)
      case _ => t
    }
  }

  /**
   * 最大値削除.
   */
  def deleteMax(t: Tree): Tree = {
    t match {
      case Branch(_, Leaf(), Leaf()) => Leaf()
      case Branch(_, _, r) => deleteMax(r)
      case _ => t
    }
  }

  /**
   * 要素削除.
   */
  def delete(k: Int, t: Tree): Tree = {
    // 同じ値が見つかった場合.
    def deleteSameVal(t: Tree): Tree = {
      t match {
        case Branch(_, Leaf(), Leaf()) => Leaf()
        case Branch(_, l, Leaf()) => l
        case Branch(_, Leaf(), r) => r
        case Branch(v, l, r) => {
          // 右部分木から最小の値と置き換えて、削除.
          Branch(searchMin(r).get, l, deleteMin(r))
        }
      }
    }

    t match {
      case Branch(v, l, r) => {
        if (k == v) deleteSameVal(t)
        else if(k < v) Branch(v, delete(k, l), r)
        else Branch(v, l, delete(k, r))
      }
      case _ => t
    }
  }

  /**
   * 最小値検索.
   */
  def searchMin(t: Tree): Option[Int] = {
    t match {
      case Leaf() => None
      case Branch(v, Leaf(), _) => Some(v)
      case Branch(_, l, _) => searchMin(l)
    }
  }
}

