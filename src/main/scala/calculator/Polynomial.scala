package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal {
      b() * b() - 4 * a() * c()
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    if (delta() < 0)
      Signal {Set()}
    else {
      val root1 = (-b() + Math.sqrt(delta())) / (2 * a())
      val root2 = (-b() - Math.sqrt(delta())) / (2 * a())

      Signal {
        Set(root1, root2)
      }
    }
  }
}
