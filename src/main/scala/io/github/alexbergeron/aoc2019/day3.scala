package io.github.alexbergeron.aoc2019

import cats.effect._
import cats.syntax.all._
import java.nio.file.{Path, Paths}
import cats.kernel.Monoid

trait Day3 extends Common {
  case class Point(x: Int, y: Int) {
    def distance: Int = math.abs(x) + math.abs(y)
  }

  case class Line(head: Point, tail: Vector[Point]) {
    lazy val toSet: Set[Point] = tail.toSet + head

    def stepsFor(p: Point): Option[Int] =
      if (toSet.contains(p))
        Some(tail.indexOf(p))
      else None
  }

  object Point {
    implicit val monoid = new Monoid[Point] {
      def empty: Point = Point(0, 0)
      def combine(p1: Point, p2: Point): Point = Point(p1.x + p2.x, p1.y + p2.y)
    }
  }

  def readLine(line: String): Line = {
    val origin = Line(Monoid[Point].empty, Vector(Monoid[Point].empty))
    line.split(",").toList.foldLeft(origin) { (line, instructions) =>
      val direction = instructionToUnit(instructions.head)
      val remaining = instructions.tail.toInt
      stepLine(line, direction, remaining)
    }
  }

  def stepLine(line: Line, direction: Point, remaining: Int): Line =
    if (remaining <= 0) line
    else {
      val next = line.head |+| direction
      stepLine(Line(next, line.tail.appended(next)), direction, remaining - 1)
    }

  def instructionToUnit(direction: Char): Point = direction match {
    case 'R'     => Point(1, 0)
    case 'L'     => Point(-1, 0)
    case 'U'     => Point(0, 1)
    case 'D'     => Point(0, -1)
    case unknown => throw new Exception(s"Unknown instruction $unknown")
  }

  def findClosest(l1: Line, l2: Line): Int =
    l1.toSet.intersect(l2.toSet).map(_.distance).filter(_ > 0).min

  def findShortest(l1: Line, l2: Line): Int =
    l1.toSet
      .intersect(l2.toSet)
      .flatMap(p => l1.stepsFor(p).zip(l2.stepsFor(p)).map(t => t._1 + t._2))
      .filter(_ > 0)
      .min
}

object Day3Part1 extends Day3 with IOApp {
  def run(args: List[String]): IO[cats.effect.ExitCode] = {
    val resourcePath: Path = Paths.get("data/day3/input")

    readLines[IO](resourcePath)
      .filter(_.nonEmpty)
      .map(readLine _)
      .compile
      .toVector
      .map { lines =>
        findClosest(lines(0), lines(1))
      }
      .flatTap(distance => IO { println(distance) })
      .as(ExitCode.Success)
  }
}

object Day3Part2 extends Day3 with IOApp {
  def run(args: List[String]): IO[cats.effect.ExitCode] = {
    val resourcePath: Path = Paths.get("data/day3/input")

    readLines[IO](resourcePath)
      .filter(_.nonEmpty)
      .map(readLine _)
      .compile
      .toVector
      .map { lines =>
        findShortest(lines(0), lines(1))
      }
      .flatTap(steps => IO { println(steps) })
      .as(ExitCode.Success)
  }
}
