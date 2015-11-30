import org.scalatest._

class Mixer {
  def whatItSays(): String = "waka-waka"
}

class MixerSpec extends FlatSpec with Matchers {
  "A Mixer" should "say waka-waka" in {
    val mixer = new Mixer()
    mixer.whatItSays() should be ("waka-waka")
  }
}
