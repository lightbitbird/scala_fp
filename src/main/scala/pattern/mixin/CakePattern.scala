package pattern.mixin

object CakePattern {

  def main(args: Array[String]): Unit = {
    AirService.airRepository.blow
  }

  trait AirRepositoryComponent {
    val airRepository: AirRepository

    trait AirRepository {
      def blow: Unit

    }
  }

  trait AirRepositoryComponentImpl extends AirRepositoryComponent {
    val airRepository = AirRepositoryImpl

    object AirRepositoryImpl extends AirRepository {
      override def blow: Unit = println(s"The wind blow...")
    }
  }

  trait AirService {
    self: AirRepositoryComponent =>
  }

  object AirService extends AirService with AirRepositoryComponentImpl
}
