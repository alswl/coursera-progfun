def streamRange(lo: Int, hi: Int): Stream[Int] = {
  print(lo + " ")
  if (lo >= hi) Stream.empty
  else Stream.cons(lo, streamRange(lo + 1, hi))
}

// notice the `Stream.cons` parameter call by name
streamRange(1, 10)
streamRange(1, 10).take(3)
streamRange(1, 10).take(3).toList