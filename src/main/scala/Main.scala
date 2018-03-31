import chapters8._
import chapters9._
import chapters10._
import chapters11._
import scala.language.implicitConversions

/**
 * プロコンのためのアルゴリズムとデータ構造.
 */
object Main extends App {

  // コマンドライン引数により、処理を分ける.
  if (args.length >= 1) {
    args(0).toInt match {
      case 8 => Chapter8.process(args)
      case 9 => Chapter9.process(args)
      case 10 => Chapter10.process(args)
      case 11 => Chapter11.process(args)
    }
  }
  else {
    println("argment is invalid")
  }
}

