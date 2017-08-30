trait Functor[F[_]] {
	def map[A,B](fa: F[A])(f: A => B): F[B]
}

trait Monad[F[_]] extends Functor[F] {
	def unit[A](a: => A): F[A]
	def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]

	def map[A,B](ma: F[A])(f: A => B): F[B] =
		flatMap(ma)(a => unit(f(a)))

	def map2[A,B,C](ma: F[A], mb: F[B])(f: (A,B) => C): F[C] =
		flatMap(ma)(a => map(mb)(b => f(a,b)))

	def sequence[A](lma: List[F[A]]): F[List[A]] =
		lma.foldRight(unit(List[A]()))((ma, mla) => map2(ma, mla)(_ :: _))

	def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] =
		la.foldRight(unit(List[B]()))((a, mlb) => map2(f(a), mlb)(_ :: _))
}


object Monad {
	val listMonad = new Monad[List] {
		def unit[A](a: => A) = List(a)
		override def flatMap[A,B](ma: List[A])(f: A => List[B]) = ma flatMap f
	}
}