package mixin

object MinimalCakePattern {

  def main(args: Array[String]): Unit = {
    Controller.carService.run

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
    def carService: CarService
  }

  trait UsesAudiCarService {
    def carService: CarService
  }

  trait MixInAudiCarService extends UsesCarService {
    override val carService: CarService = new AudiCarService {
      override def run: Unit = println("The Audi car run.")
    }
  }

  trait UsesNissanCarService {
    def carService: CarService
  }

  trait MixInCarNissanService extends UsesCarService {
    override val carService: CarService = new NissanCarService {
      override def run: Unit = println("The Nissan car run.")
    }
  }

  abstract class Controller
    extends UsesNissanCarService
      with UsesAudiCarService

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
