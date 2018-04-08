package pattern.mixin

object MinimalCakePattern {

  def main(args: Array[String]): Unit = {
    println(Controller.hogeService.run)

    val something = Service.something
    something.doSomething
  }

  //--------------------------------------------
  trait CarService {
    def run: Unit
  }

  trait AudiCarService extends CarService
  trait NissanCarService extends CarService

  trait UsesCarService {
    def hogeService: CarService
  }

  trait UsesAudiCarService {
    def hogeService: CarService
  }

  trait MixInAudiCarService extends UsesCarService {
    override val hogeService: CarService = new AudiCarService {
      override def run: Unit = println("The Audi car run.")
    }
  }

  trait UsesNissanCarService {
    def hogeService: CarService
  }

  trait MixInCarNissanService extends UsesCarService {
    override val hogeService: CarService = new NissanCarService {
      override def run: Unit = println("The Nissan car run.")
    }
  }

  abstract class Controller
    extends UsesNissanCarService
      with UsesAudiCarService

  // hogeService が被っているが、override が付いているので Scala の継承の仕様によって粛々と後勝ちになる
  object Controller
    extends MixInCarNissanService
      with MixInAudiCarService
  //--------------------------------------------


  //--------------------------------------------
  //MixIn
  object Service extends MixInUsesSomething

  trait Something {
    def doSomething: Unit
  }

  trait MixInSomething extends Something {
    override def doSomething = {
      println(s"Hello from MixinSomething")
    }
  }

  trait UsesSomething {
    def something: Something
  }

//  trait MixInSomething extends UsesSomething {
  trait MixInUsesSomething {
    def something: Something = new MixInSomething {
      override def doSomething: Unit = super.doSomething
    }
  }

  trait Service extends UsesSomething {
//  abstract class Service extends UsesSomething {
    // ...
  }
  //--------------------------------------------

}
