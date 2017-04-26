package main.tweetset


class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
      "Text: " + text + " [" + retweets + "]"
}


abstract class TweetSet {

  def filter(p: Tweet => Boolean): TweetSet = filterAcc(p, new Empty)

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  def union(that: TweetSet): TweetSet

  def mostRetweeted: Tweet

  def descendingByRetweet: TweetList

  def incl(tweet: Tweet): TweetSet

  def remove(tweet: Tweet): TweetSet

  def contains(tweet: Tweet): Boolean

  def foreach(f: Tweet => Unit): Unit
}

class Empty extends TweetSet {
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = new Empty

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()

  def union(that: TweetSet): TweetSet = that

  def mostRetweeted: Tweet = throw new NoSuchElementException()

  def descendingByRetweet: TweetList = Nil
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet =
    if (this.isInstanceOf[NonEmpty] && p(elem)) {
      left.filterAcc(p, acc.incl(elem))
      right.filterAcc(p, acc.incl(elem))
    }
    else acc

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }

  def union(that: TweetSet): TweetSet = left.union(right).union(that).incl(elem)

  def mostRetweeted: Tweet = {
    val most = filter(x => x.retweets > elem.retweets)
    if (most.isInstanceOf[NonEmpty]) most.mostRetweeted
    else elem
  }

  def descendingByRetweet: TweetList = {
    val most = mostRetweeted
    val rest = remove(most)
    new Cons(most, rest.descendingByRetweet)
  }

}

trait TweetList {
  def head: Tweet

  def tail: TweetList

  def isEmpty: Boolean

  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")

  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")

  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}


object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  val all: TweetSet = TweetReader.allTweets

  lazy val googleTweets: TweetSet = all.filter(a => google.exists(b => a.text.contains(b)))
  lazy val appleTweets: TweetSet = all.filter(a => apple.exists(b => a.text.contains(b)))

  lazy val trending: TweetList = googleTweets.union(appleTweets).descendingByRetweet
}

object Main extends App {
  GoogleVsApple.trending foreach println
}
