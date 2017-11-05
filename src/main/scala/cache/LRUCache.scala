package cache

import java.util.concurrent.ConcurrentHashMap

class Node[V](val value: V) {
  var prev: Node[V] = _
  var next: Node[V] = _
}

object Node {
  def apply[V](value: V): Node[V] = new Node(value)
}

class LRUCache[K, V](val max: Int) {
  private val map: ConcurrentHashMap[K, Node[V]] = new ConcurrentHashMap[K, Node[V]]()
  private var head: Node[V] = _
  private var last: Node[V] = _

  def getHead: Node[V] = head
  def getLast: Node[V] = last

  // 1. This is the n th element in the cache
  // 2. This is the first element in the cache
  // 3. Cache has already reached the max
  def put(key: K, value: V): Unit = {
    // Create the node that needs to be added
    val node: Node[V] = Node(value)
    if (head != null) {
      head.prev = node
      node.next = head
    } else {
      last = node
    }
    head = node
    map.put(key, node)
    if (map.size > max) {
      map.remove(last.value)
      last = last.prev
      last.next = null
    }
  }

  // 1. If key does not exist, return None
  //    i) empty cache
  //    ii) Does not contain key
  // 2. If key exist, return and add to the head
  //    i) Only one entity in the cache
  //    ii) Only 2 entities in the cache (head and last)
  //    iii) multiple entities in the cache
  def get(key: K): Option[V] = {
    Option(map.get(key)).map { n =>
        if(n.prev != null) {
          if (n.next != null)
            n.next.prev = n.prev
          else last = n.prev
          n.prev.next = n.next
          n.prev = null
          n.next = head
          head = n
        }
      n.value
    }
  }
}
