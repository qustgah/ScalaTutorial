/**
  * Created by apple on 2017/7/8.
  */
abstract class SeqBuffer extends Buffer{

  type U
  type T <: Seq[U]
  def length = element.length
}
