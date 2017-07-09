import javax.management.Notification

import scala.collection.mutable.ArrayBuffer
import scala.sys.SystemProperties
import scala.util.Random
import scala.util.matching.Regex

/**
  * Created by apple on 2017/7/8.
  */
class Main {

  (x : Int) => x + 1

  val addOne = (x:Int) => x +1

  def add(x:Int,y:Int) = x +y

  def addThenMultiply(x:Int,y:Int)(multiplier : Int) = (x + y) * multiplier

  def getSquareString(input : Double) : String = {
    val square = input * input
    square.toString
  }

  class Greeter(prefix : String,suffix : String){
    def greet(name : String) :Unit = println(prefix + name + suffix)
  }

  case class  Point(x : Int,y:Int){

    override def equals(that: Any): Boolean = true



  }

  val point = Point(1,2)

  object IdFactory{
    private var counter = 0

    def create() : Int = {
      counter += 1
      counter
    }

    def main(args: Array[String]): Unit = {
      val list : List[Any] = List(
        "a String",888,'c',true,()=>"an anonymous function returning a String "
      )

      list.foreach(element => println(element))

      val x:Long = 987654321
      val y:Float = x


      val face : Char = 'c'
      val number :Int = face

      class User
      val user1 = new User

      class Point(var x : Int,var y:Int){
        def move(dx:Int,dy:Int): Unit ={
          x = x + dx
          y = y + dy
        }

        override def toString: String = s"($x,$y)"




      }

      class PriPoint{
        private var _x = 0
        private var _y = 0
        private val bound = 100

        def x = _x
        def x_= (newValue : Int) = {
          if (newValue < bound) _x = newValue else println("error")
        }

        def y = _y
        def y_= (newValue : Int) ={
          if (newValue < bound) _y = newValue else println("error")
        }

        private def printWarning = println("Warning : Out of bounds")
      }

      val priPoint = new PriPoint
      priPoint.x = 99
      priPoint.y = 101

      trait HairColor

      trait Iterator[A]{
        def hasNext : Boolean
        def next() : A
      }

      class IntIterator(to : Int) extends Iterator[Int]{
        private var current = 0
        override def hasNext: Boolean = current < to

        override def next(): Int = {
          if (hasNext){
            val t = current
            current += 1
            t
          }else 0
        }
      }

      val iterator = new IntIterator(10)
      iterator.next()
      iterator.next()

      trait Pet{
        val name : String
      }

      class Cat(val name:String) extends Pet

      class Dog(val name: String) extends Pet

      val dog = new Dog("Harry")
      val cat = new Cat("Sally")

      val animals = ArrayBuffer.empty[Pet]
      animals.append(dog)
      animals.append(cat)
      animals.foreach(pet => println(pet.name))

      abstract class A{
        val message:String
      }

      class B extends A{
        override val message: String = "I'm an instance of class B"
      }

      trait C extends A{
        def loudMessage = message.toUpperCase
      }

      class D extends B with C
      val d = new D
      d.message
      d.loudMessage


      abstract class AbsIterator{
        type T
        def hasNext : Boolean
        def next:T
      }

      class StringIterator(s:String) extends AbsIterator{
        type T = Char
        private var i = 0

        override def hasNext: Boolean = i < s.length

        override def next: Char = {
          val ch = s charAt i
          i += 1
          ch
        }
      }

      trait RichIterator extends AbsIterator{
        def foreach(f:T=>Unit) : Unit = while (hasNext) f(next)
      }

      object StringIteratorTest extends App{
        class Iter extends StringIterator(args(0)) with RichIterator
        val  iter = new Iter
        iter foreach println
      }

      def apply(f:Int => String,v:Int) = f(v)

      class Decorator(left: String,right:String){
        def layout[A](x:A) = left + x.toString + right
      }

      object FunTest extends App{
        def apply(f:Int => String,v:Int) = f(v)
        val decorator = new Decorator("[","]")
        println(apply(decorator.layout,7))
      }

      def factorial(x:Int):Int ={
        def fact(x:Int,accumulator:Int) :Int={
          if (x<= 1 ) accumulator
          else fact(x-1,x*accumulator)
        }
        fact(x,1)
      }

      def filter(xs:List[Int],p:Int => Boolean) :List[Int] = {
        if (xs.isEmpty) xs
        else if (p(xs.head)) xs.head :: filter(xs.tail,p)
        else filter(xs.tail,p)
      }

      def modN(n:Int)(x:Int) = ((x%n) == 0)

      val nums = List(1,2,3,4,5,6,7,8)
      println(filter(nums,modN(2)))

      case class Book(isbn:String)

      val frankenstein = Book("978-0299292922")

      case class Message(sender : String ,recipient: String, body: String)

      val message1 = Message("qust@","kkkk","......")

      println(message1.sender)

      val message4 = Message("","","")
      val message5 = message4.copy(sender = message4.recipient,recipient = "")


      def pattern() = {
        val x : Int = Random.nextInt(10)

        x match{
          case 0 => "zero"
          case _ => "many"
        }
      }

      def matchTest(x:Int) : String = x match {
        case 1 => "one"
        case 2 => "two"
        case 3 => "many"
        case _ => ""
      }

      abstract class Nofification

      case class Email(sender : String,title: String,body: String) extends Nofification

      case class SMS(caller: String,message: String) extends Nofification

      case class VoiceRecording(contactName: String,link : String) extends Nofification

      def showNotification(notification:Nofification):String ={
        notification match {
          case Email(email,title,_) => s"you get an email from $email with title: $title"
          case SMS(number,message) => s"$number,$message"


        }
      }


      def showImportantNotificaion(nofification: Nofification,importantPeopleInfo:Seq[String]) : String ={
        nofification match {
          case Email(email,_,_) if importantPeopleInfo.contains(email) =>
            "You get an email from special someOne"
          case SMS(number,_) if importantPeopleInfo.contains(number) =>
            "you get an SMS from Special someone!"
          case other => showNotification(other)

        }
      }

      abstract class Device
      case class Phone(model:String) extends Device{
        def screenOff = "Turning screen off"

      }

      case class Computer(model:String) extends Device{
        def screenSaverOn = "Turning screen saver on..."
      }

      def goIdle(device:Device) = device match {
        case p:Phone => p.screenOff
        case c:Computer => c.screenSaverOn
      }

      val numberPattern: Regex = "[0-9]".r
      numberPattern.findFirstMatchIn("awesomepassword") match {
        case Some(_) => println("Password ok ")
        case None => println(("password must contain a number"))
      }

      class Stack[A] {
        private var elements: List[A] = Nil

        def push(x: A) {
          elements = x :: elements
        }

        def peek: A = elements.head

        def pop(): A = {
          val currentTop = peek
          elements = elements.tail
          currentTop
        }
      }

      val stack = new Stack[Int]
      stack.push(1)
      stack.push(2)
      println(stack.pop)
      println(stack.pop())

      abstract class Animal{
        def name : String
      }


      trait Node[+B]{
        def prepend[U>: B] (elem:U)
      }

      class Graph{
        class Node{
          var connectedNodes: List[Node] = Nil
          def connectTo(node: Node): Unit ={
            if (connectedNodes.find(node.equals).isEmpty){
              connectedNodes = node::connectedNodes
            }
          }
        }
        var nodes:List[Node] = Nil
        def newNode: Node = {
          val res = new Node
          nodes = res::nodes
          res
        }
      }

      val graph1: Graph = new Graph
      val node1: graph1.Node = graph1.newNode
      val node2: graph1.Node = graph1.newNode
      val node3: graph1.Node = graph1.newNode

      trait Buffer{
        type T
        val element : T
      }

      abstract class SeqBuffer extends Buffer{
        type U
        type T <: Seq[U]
        def length =element.length
      }

      abstract class IntSeqBuffer extends SeqBuffer{
        type U = Int
      }

      def newIntSeqBuf(elem1 : Int,elem2: Int): IntSeqBuffer = new IntSeqBuffer {
        type T = List[U]
        val element = List(elem1,elem2)
      }

      val buf = newIntSeqBuf(7,8)

      abstract class AbBuffer[+T]{
        val element: T

      }

      abstract class AbSeqBuffer[U,+T <: Seq[U]] extends AbBuffer[T]{
        def length = element.length
      }

      def newAbIntSeqBuf(e1:Int,e2:Int):AbSeqBuffer[Int,Seq[Int]] = new AbSeqBuffer[Int,List[Int]] {
        override val element: List[Int] = List(e1,e2)
      }

      val abuf = newAbIntSeqBuf(7,8)

      trait Cloneable extends java.lang.Cloneable{
        override def clone(): Cloneable = {
          super.clone().asInstanceOf[Cloneable]
        }
      }

      trait Resetable{
        def reset: Unit
      }


      def cloneAndReset(obj: Cloneable with Resetable):Cloneable={
        val cloned = obj.clone()
        obj.reset
        cloned
      }

      trait SelfUser{
        def username: String

      }

      trait Tweeter{
        this:SelfUser =>
        def tweet(tweeText:String) = println(s"$username:$tweeText")
      }


      class VerifiedTweeter(val username_ : String) extends Tweeter with SelfUser{
        override def username: String = s"real$username_"
      }

      abstract class SemiGroup[A]{
        def add(x:A,y:A) : A
      }

      abstract class Monoid[A] extends SemiGroup[A]{
        def unit: A
      }

      object ImplicitTest extends App{
        implicit object StringMonoid extends Monoid[String]{
          override def add(x: String, y: String): String = x concat y
          def unit:String = ""

        }

        implicit object IntMonoid extends Monoid[Int]
        {
          override def add(x: Int, y: Int): Int = x + y
          def unit: Int = 0

        }

        def sum[A](xs:List[A])(implicit m:Monoid[A]):A =
          if (xs.isEmpty) m.unit
          else m.add(xs.head,sum(xs.tail))

        println(sum(List(1,2,3)))
        println(sum(List("a","b")))

        implicit def list2ordered[A](x:List[A])(implicit  elem2ordered:A => Ordered[A]) : Ordered[List[A]] =
          (that: List[A]) => 0

        def listOfDuplicates[A](x:A,length:Int):List[A] = {
          if (length < 1){
            Nil
          }else{
            x::listOfDuplicates(x,length-1)

          }
        }

        case class MyPair[A,B](x:A,y:B);

        def id[T](x:T) = x
        val p = MyPair(1,"scala")
        val 1 = id(1)





























      }












































































































































    }

  }

  trait GreeterTrait{

    def greet(name : String) : Unit = {
      println("Hello " + name)
    }

  }









































}
