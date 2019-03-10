package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, next) = rng.nextInt
    if (i < 0) (-(i + 1), next) else (i, next)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, next) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), next)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, next) = rng.nextInt
    val (d, next2) = double(rng)
    ((i, d), next2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), next) = intDouble(rng)
    ((d, i), next)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, next1) = double(rng)
    val (d2, next2) = double(rng)
    val (d3, next3) = double(rng)
    ((d1, d2, d3), next3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) (Nil, rng)
    else {
      val (tail, r1) = ints(count - 1)(rng)
      val (i, r2) = r1.nextInt
      (i :: tail, r2)
    }
  }

  def doubleViaMap(rng: Rand[Int]): Rand[Double] = {
    map(rng)(i => i / (Int.MaxValue.toDouble + 1))
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, r1) = ra(rng)
    val (b, r2) = rb(r1)
    (f(a, b), r2)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldLeft(unit(List.empty[A]))(
      (acc, rand) => map2(acc, rand)((a, b) => b :: a)
    )
  }

  def intsViaSequence(count: Int)(rng: RNG): (List[Int], RNG) = {
    sequence(List.fill(count)(int))(rng)
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (v, r1) = f(rng)
    g(v)(r1)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { x =>
      val m = x % n
      if (Int.MaxValue - m >= x) unit(m) else nonNegativeLessThan(n)
    }

  def mapViaFlatMap[A, B](a: Rand[A])(f: A => B): Rand[B] =
    flatMap(a)(x => unit(f(x)))

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => mapViaFlatMap(rb)(b => f(a, b)))


}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))
  def flatMap[B](f: A => State[S, B]): State[S, B] = State[S, B](state => {
      val (a, s2) = run(state)
      f(a).run(s2)
    }
  )

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def output: ((Int, Int), Machine) = ((coins, candies), this)
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A) = State[S, A](s => (a, s))

  def sequence[S, A](states: List[State[S, A]]): State[S, List[A]] = {
    states.foldRight(unit[S, List[A]](List.empty))(
      (s, acc) => acc.map2(s)((l, v) => v :: l)
    )
  }

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def nextState(i: Input)(m: Machine): Machine = (m, i) match {
    case (Machine(true, candies, coins), Coin) if candies > 0 =>
      Machine(false, candies, coins + 1)

    case (Machine(false, candies, coins), Turn) =>
      Machine(true, candies - 1, coins)

    case (m, _) => m
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    for {
      _ <- sequence(inputs map nextState map modify[Machine])
      r <- get
    } yield (r.coins, r.candies)
  }
}
