package sh.sysl

import scala.collection.mutable

/** An undo log for the analyzer's whole-program tables, so that a speculative walk can be taken
 * back for the cost of what it *did* rather than the cost of what the tables *hold*.
 *
 * The analyzer asks speculative questions constantly — whether a receiver has a member of some
 * name, which of several overloads the arguments fit, whether a name resolves at all — and each one
 * is a real walk whose registrations have to be dropped again (`AnalyzerBase.sandboxed`). Taking a
 * copy of every table before the walk and putting it back after is correct and costs the size of
 * the tables at each of those questions, which is a program's instantiations multiplied by its call
 * sites: measured on a generated program, 38,553 speculative walks over 14,005 lines copied 239
 * million table entries, which is 13.4 GB of the 14.0 GB that analysis allocated in total, and the
 * cost per line grew with the program rather than staying flat.
 *
 * A journal costs the *changes* instead. Each table records how to undo a write while a speculative
 * region is open, and rewinding replays those backwards to the mark — so a question that registers
 * nothing costs nothing, whatever the tables hold.
 *
 * **Nothing is recorded while no region is open**, which is what keeps the log from growing over a
 * whole build: outside a speculative walk there is nobody who could rewind, so there is nothing to
 * remember. The log is emptied again as the outermost region closes.
 */
final class Journal {

  private val undos = mutable.ArrayBuffer.empty[() => Unit]

  private var open = 0

  /** How many undo entries this journal has written over its life, and how long the log ever got.
   *
   * They are what says the rewinding is laid out per *change* rather than per *table*: the first
   * grows with what a walk registers and the second with how deep the speculation nests, and
   * neither with what the program has built so far. `AnalyzerRewindTests` asserts on both, which it
   * can do on a loaded machine as easily as on an idle one because they are counts.
   */
  private[sysl] var recorded: Long = 0

  private[sysl] var deepest: Int = 0

  /** Whether a table has anybody to answer to. A write outside a speculative region is permanent,
   * so recording how to take it back would be remembering something nothing can ask for.
   */
  def recording: Boolean = open > 0

  /** Opens a region, answering the mark to rewind it to. */
  def enter(): Int = {
    open += 1
    undos.length
  }

  /** Closes a region. The log is dropped with the outermost one, since a write made under no region
   * at all can never be taken back.
   */
  def leave(): Unit = {
    open -= 1
    if open == 0 then undos.clear()
  }

  def record(undo: () => Unit): Unit = {
    undos += undo
    recorded += 1
    if undos.length > deepest then deepest = undos.length
  }

  /** Everything written since `mark`, undone newest first — which is the order that matters, since
   * two writes to one key have to be taken back in the order they were made.
   */
  def rewind(mark: Int): Unit = {
    var i = undos.length

    while i > mark do
      i -= 1
      undos(i)()

    undos.remove(mark, undos.length - mark)
  }
}

/** A map that tells a [[Journal]] how to take each write back, holding a `LinkedHashMap` rather
 * than extending one — inheriting from the linked collections is deprecated, and a delegate is what
 * makes the set of ways in **closed**: `get`, `iterator`, `addOne` and `subtractOne` are the whole
 * of what a mutable map is built from, so a write cannot reach the table without passing here.
 *
 * That mattered more than it sounds. `update`, `getOrElseUpdate` and `addOne` are each written
 * directly in `LinkedHashMap` rather than in terms of `put`, so a version of this that overrode
 * `put` alone let `m(k) = v` — which is how the analyzer writes to nearly all of these tables — go
 * in unrecorded. A rewind then put the *sets* back and left the *maps* as they were, which is worse
 * than not rewinding at all: it left a vtable registered whose method had been un-reached, and the
 * emitted module named a function nothing defined.
 *
 * Insertion order survives a rewind. Undoing an insertion removes the key, which leaves the keys
 * before it where they were, and undoing an overwrite writes the old value back to a key the map
 * already holds — which does not move it.
 */
final class JournaledMap[K, V](journal: Journal)
    extends mutable.AbstractMap[K, V] with mutable.SeqMap[K, V] {

  private val under = mutable.LinkedHashMap.empty[K, V]

  def get(key: K): Option[V] = under.get(key)

  def iterator: Iterator[(K, V)] = under.iterator

  override def contains(key: K): Boolean = under.contains(key)

  override def size: Int = under.size

  override def knownSize: Int = under.knownSize

  override def isEmpty: Boolean = under.isEmpty

  def addOne(kv: (K, V)): this.type = {
    write(kv._1, kv._2)
    this
  }

  def subtractOne(key: K): this.type = {
    erase(key)
    this
  }

  override def put(key: K, value: V): Option[V] = {
    val old = under.get(key)
    write(key, value)
    old
  }

  override def update(key: K, value: V): Unit = write(key, value)

  override def remove(key: K): Option[V] = {
    val old = under.get(key)
    erase(key)
    old
  }

  override def getOrElseUpdate(key: K, default: => V): V =
    under.get(key) match
      case Some(v) => v
      case None =>
        val made = default
        write(key, made)
        made

  override def clear(): Unit = {
    if journal.recording then
      val held = under.toList
      journal.record { () =>
        under.clear()
        under ++= held
      }

    under.clear()
  }

  private def write(key: K, value: V): Unit = {
    if journal.recording then
      under.get(key) match
        case Some(old) => journal.record(() => under(key) = old)
        case None      => journal.record(() => under.remove(key))

    under(key) = value
  }

  private def erase(key: K): Unit = {
    if journal.recording then under.get(key).foreach(old => journal.record(() => under(key) = old))

    under.remove(key)
  }
}

/** The same for a set, over a `LinkedHashSet` for the same reason and with the same order. */
final class JournaledSet[A](journal: Journal) extends mutable.AbstractSet[A] {

  private val under = mutable.LinkedHashSet.empty[A]

  def contains(elem: A): Boolean = under.contains(elem)

  def iterator: Iterator[A] = under.iterator

  override def size: Int = under.size

  override def knownSize: Int = under.knownSize

  override def isEmpty: Boolean = under.isEmpty

  def addOne(elem: A): this.type = {
    if journal.recording && !under.contains(elem) then journal.record(() => under.remove(elem))

    under += elem
    this
  }

  def subtractOne(elem: A): this.type = {
    if journal.recording && under.contains(elem) then journal.record(() => under += elem)

    under -= elem
    this
  }

  override def clear(): Unit = {
    if journal.recording then
      val held = under.toList
      journal.record { () =>
        under.clear()
        under ++= held
      }

    under.clear()
  }
}
