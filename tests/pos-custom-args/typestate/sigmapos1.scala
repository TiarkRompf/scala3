import typestate.*

def bing(x: Int): Sigma { type A = Int; type B = Int } =
  Sigma(x, 10)

def testBing() =
  1 + bing(10) + bing(20) + summon[Int]
  val z = bing(30)
  bing(40)
