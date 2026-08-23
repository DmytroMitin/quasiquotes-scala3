package quasiquotes.types.phase119

class TopLevel
type TopAlias = TopLevel

object OwnerOne:
  class Same
  class Nested

object OwnerTwo:
  class Same

class Holder:
  class Member

object StablePaths:
  val first: Holder = new Holder
  val second: Holder = new Holder

object MutablePaths:
  var current: Holder = new Holder

object UserConstructors:
  class List[A]
  class Option[A]
  class Either[A, B]
  class Box[A]
