object Main:
  class Iota:
    def test1: Int = 10

    def test1(z: String): Int = 10

    def test1(k: Double): Int = 10

  def test() =
    val iota = new Iota
    val k: Int = iota.test1