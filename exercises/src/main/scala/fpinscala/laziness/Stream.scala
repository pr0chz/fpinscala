package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def toList: List[A] = {
    this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // completely lazy as the tail (and recursive call) is part of tail thunk
  def take(n: Int): Stream[A] =
    (this, n) match {
      case (Cons(h, t), x) if x > 0 => Stream.cons(h(), t().take(x - 1))
      case (_, _) => Stream.empty
    }

  // this is strict, all dropped stream items are evaluated, rest of the stream kept lazy
  def drop(n: Int): Stream[A] =
    (this, n) match {
      case (Cons(h, t), x) if x > 0 => t().drop(x - 1)
      case (stream, _) => stream
    }

  // will evaluate first head, otherwise lazy as the tail (and recursive call) is part of tail thunk
  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
      case _ => Stream.empty
    }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((x, acc) => p(x) && acc)

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((x, acc) => if (p(x)) Stream.cons(x, acc) else Stream.empty)


  def headOption: Option[A] = foldRight(Option.empty[A])((x, _) => Some(x))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = foldRight(Stream.empty[B])((x, acc) => Stream.cons(f(x), acc))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((x, acc) => if (p(x)) Stream.cons(x, acc) else acc)

  def append[B >: A](stream2: Stream[B]): Stream[B] =
    foldRight(stream2)((x, acc) => Stream.cons(x, acc))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((x, acc) => f(x).append(acc))

  def mapUnfold[B](f: A => B): Stream[B] = Stream.unfold(this) {
    case Empty => None
    case Cons(h, t) => Some((f(h()), t()))
  }

  def takeUnfold(n: Int): Stream[A] = Stream.unfold((this, n)) {
    case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
    case _ => None
  }

  def takeWhileUnfold(p: A => Boolean): Stream[A] = Stream.unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case Empty => None
  }

  def zipWith[B, C](b: Stream[B])(f: (A, B) => C): Stream[C] = Stream.unfold((this, b)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    case _ => None
  }

  def zipAll[B](b: Stream[B]): Stream[(Option[A], Option[B])] = Stream.unfold((this, b)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
    case (Cons(h1, t1), Empty) => Some(((Some(h1()), None)), (t1(), Stream.empty))
    case (Empty, Cons(h2, t2)) => Some(((None, Some(h2()))), (Stream.empty, t2()))
    case _ => None
  }

  def tails: Stream[Stream[A]] = Stream.unfold(this) {
    case s@Cons(h, t) => Some(s, t())
    case Empty => None
  }.append(Stream(Stream.empty))

  def hasSubsequence[A](subsequence: Stream[A]): Boolean =
    tails.exists(_.startsWith(subsequence))

  def startsWith[B](s: Stream[B]): Boolean = zipAll(s)
    .takeWhile(_._2.nonEmpty)
    .forAll(x => x._1 == x._2)

  def scanRight[B](z: B)(f: (A, B) => B): Stream[B] =
    foldRight(Stream(z))((x, acc) => Stream.cons(f(x, acc.headOption.get), acc))

  def scanRightRaw[B](z: B)(f: (A, B) => B): Stream[B] = this match {
    case Empty => Stream(z)
    case Cons(h, t) => {
      lazy val r = t().scanRightRaw(z)(f)
      Stream.cons(f(h(), r.headOption.get), r)
    }
  }


//  def scanRightViaUnfold[B](z: B)(f: (A, B) => B): Stream[B] =
//    Stream.unfold(this) {
//      case Empty =>
//      case Cons(h, t) => f(h(), )
//    }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def compute(a: Int, b: Int): Stream[Int] = Stream.cons(a, compute(b, a + b))
    compute(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    case None => Stream.empty
  }

  def fibsUnfold: Stream[Int] = unfold((0, 1))(s => Some((s._1, (s._2, s._1 + s._2))))

}