package quasiquotes.matching

/** Source-reflected chain fixture; repeating target names never repeats source binders. */
object Q079ChainTarget:
  final class Node:
    val alpha: Node = this
    val beta: Node = this
    val gamma: Node = this
    val `+`: Node = this
    val `type`: Node = this
    val `safe spaced name`: Node = this
  val root: Node = new Node
