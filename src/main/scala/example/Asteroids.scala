package example

import org.scalajs.dom
import scala.util.Random

case class Asteroids(bounds: Point, resetGame: () => Unit) extends Game {

  var bullets1 = Seq.empty[Bullet]
  val craft1 = new Craft1(bounds / 2 - Point(bounds.x / 4, 0), Point(0, 0), 0)
  var bullets2 = Seq.empty[Bullet]
  val craft2 = new Craft2(bounds / 2 + Point(bounds.x / 4, 0), Point(0, 0), 0)
  var frameCount = 0
  var asteroids = Seq.fill(10)(
    new Asteroid(3,
      if (Random.nextBoolean()) Point(0, Random.nextInt(bounds.y.toInt))
      else Point(Random.nextInt(bounds.y.toInt), 0),
      Point(Random.nextInt(5), Random.nextInt(5)) - Point(2.5, 2.5)
    )
  )

  def update(keys: Set[Int]) = {
    frameCount += 1


    asteroids.foreach(_.move())
    bullets1.foreach(_.move())
    bullets2.foreach(_.move())
    craft1.move(keys)
    craft2.move(keys)


    // Space 32, '3' 51

    if (keys(51) && bullets1.length < 10 && frameCount % 2 == 0) {
      bullets1 = bullets1 :+ new Bullet(
        craft1.position,
        craft1.momentum + Point(15, 0).rotate(craft1.theta)
      )
    }
    if (keys(32) && bullets2.length < 10 && frameCount % 2 == 0) {
      bullets2 = bullets2 :+ new Bullet(
        craft2.position,
        craft2.momentum + Point(15, 0).rotate(craft2.theta)
      )
    }

    val changes = for {
      b1 <- bullets1
      b2 <- bullets2
      a <- asteroids
      if a.contains(b1.position) || a.contains(b2.position)
    } yield {
      val newAsteroids =
        if (a.level == 1) Nil
        else {
          Seq(30, -30).map(d =>
            new Asteroid(a.level - 1, a.position, a.momentum.rotate(d * Math.PI / 180))
          )
        }
      (Seq(a, b1), newAsteroids)
    }
    val (removed, added) = changes.unzip
    val flatRemoved = removed.flatten
    asteroids = asteroids.filter(!flatRemoved.contains(_)) ++ added.flatten
    bullets1 =
      bullets1
        .filter(!flatRemoved.contains(_))
        .filter(_.position.within(Point(0, 0), bounds))

    if (asteroids.exists(_.contains(craft1.position))) {
      result = Some("Ship 1 hit an asteroid!")
      resetGame()
    } else if (asteroids.exists(_.contains(craft2.position))) {
      result = Some("Ship 2 hit an asteroid!")
      resetGame()
    } else if (asteroids.length == 0) {
      result = Some("You successfully destroyed every asteroid!")
      resetGame()
    }
  }

  def draw(ctx: dom.CanvasRenderingContext2D) = {
    ctx.fillStyle = Color.Black
    ctx.fillRect(0, 0, 800, 800)

    ctx.fillStyle = Color.White
    ctx.strokeStyle = Color.White

    asteroids.foreach(_.draw(ctx))
    bullets1.foreach(_.draw(ctx))
    bullets2.foreach(_.draw(ctx))
    craft1.draw(ctx)
    craft2.draw(ctx)
  }


  class Asteroid(val level: Int, var position: Point, val momentum: Point) {
    def draw(ctx: dom.CanvasRenderingContext2D) = {
      val size = 10 * level
      ctx.fillRect(position.x - size / 2, position.y - size / 2, size, size)
    }

    def move() = {
      position += momentum
      position += bounds
      position %= bounds
    }

    def contains(other: Point) = {
      val min = position - Point(5, 5) * level
      val max = position + Point(5, 5) * level
      other.within(min, max)
    }
  }

  abstract class Craft {

    var position: Point
    var momentum: Point
    var theta: Double

    def draw(ctx: dom.CanvasRenderingContext2D) = {
      ctx.beginPath()
      val pts = Seq(
        Point(15, 0).rotate(theta) + position,
        Point(7, 0).rotate(theta + 127.5 / 180 * Math.PI) + position,
        Point(7, 0).rotate(theta - 127.5 / 180 * Math.PI) + position
      )
      ctx.moveTo(pts.last.x, pts.last.y)
      pts.map(p => ctx.lineTo(p.x, p.y))
      ctx.fill()
    }

    def move(keys: Set[Int])
  }

  class Craft1(var position: Point, var momentum: Point, var theta: Double)
    extends Craft {

    override def move(keys: Set[Int]) = {
      position += momentum
      position += bounds
      position %= bounds

      // A 65, left 37
      // W 87, up 38
      // D 68, right 39
      // S 83, down 40

      if (keys(65)) theta -= 0.05
      if (keys(87)) momentum += Point(0.2, 0).rotate(theta)
      if (keys(68)) theta += 0.05
      if (keys(83)) momentum -= Point(0.2, 0).rotate(theta)
    }
  }

  class Craft2(var position: Point, var momentum: Point, var theta: Double)
    extends Craft {

    override def move(keys: Set[Int]) = {
      position += momentum
      position += bounds
      position %= bounds

      // A 65, left 37
      // W 87, up 38
      // D 68, right 39
      // S 83, down 40

      if (keys(37)) theta -= 0.05
      if (keys(38)) momentum += Point(0.2, 0).rotate(theta)
      if (keys(39)) theta += 0.05
      if (keys(40)) momentum -= Point(0.2, 0).rotate(theta)
    }
  }

  class Bullet(var position: Point, val momentum: Point) {
    def draw(ctx: dom.CanvasRenderingContext2D) = {
      ctx.beginPath()
      ctx.moveTo(position.x, position.y)
      val forward = position + momentum * 5.0 / momentum.length
      ctx.lineTo(forward.x, forward.y)
      ctx.stroke()
    }

    def move() = {
      position += momentum
    }
  }

}