package cache

import org.scalatest.{FlatSpec, Matchers}

class LRUCacheTest extends FlatSpec with Matchers {

  "cache get" should "return None for empty cache" in {
    val cache = new LRUCache[String, String](5)
    cache.get("not-present") shouldBe None
  }

  it should "return the element when only one element is present" in {
    val cache = new LRUCache[String, String](5)
    cache.put("key1", "value1")
    cache.get("key1").get shouldBe "value1"
  }

  it should "return the element when only two elements are present" in {
    val cache = new LRUCache[String, String](5)
    cache.put("key1", "value1")
    cache.getHead.value shouldBe "value1"
    cache.getLast.value shouldBe "value1"
    cache.put("key2", "value2")
    // Make sure that head and last are correct
    cache.getHead.value shouldBe "value2"
    cache.getLast.value shouldBe "value1"

    cache.get("key1").get shouldBe "value1"
    // key 2 should be the head now
    cache.getHead.value shouldBe "value1"
    // key 1 should be the last now
    cache.getLast.value shouldBe "value2"
  }

  it should "return the element when only three elements are present" in {
    val cache = new LRUCache[String, String](5)
    cache.put("key1", "value1")
    cache.put("key2", "value2")
    cache.put("key3", "value2")
    cache.get("key2").get shouldBe "value2"
    // key 2 should be the head now
    cache.getHead.value shouldBe "value2"
    // key 1 should be the last now
    cache.getLast.value shouldBe "value1"
  }

}
