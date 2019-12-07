package io.github.alexbergeron.aoc2019

import cats.effect._
import cats.syntax.all._
import fs2.Stream
import java.nio.file.Path
import java.nio.file.Paths

trait Day1 extends Common {
  def fuelForMass(mass: Int): Int = {
    math.floor(mass.toDouble / 3).toInt - 2
  }

  def fuelForFuel[F[_]: Sync](fuel: Int): F[Int] = {
    val requiredFuel = fuelForMass(fuel)
    if (requiredFuel <= 0) {
      Sync[F].pure(0)
    } else {
      Sync[F].suspend(fuelForFuel(requiredFuel).map(_ + requiredFuel))
    }
  }
}

object Day1Part1 extends Day1 with IOApp {
  def run(args: List[String]): IO[cats.effect.ExitCode] = {
    val resourcePath: Path = Paths.get("data/day1/input")

    readLines[IO](resourcePath)
      .filter(_.nonEmpty)
      .map(_.toInt)
      .map(fuelForMass _)
      .compile
      .fold(0)(_ + _)
      .flatMap(response => IO { println(response) })
      .as(ExitCode.Success)
  }
}

object Day1Part2 extends Day1 with IOApp {
  def run(args: List[String]): IO[cats.effect.ExitCode] = {
    val resourcePath: Path = Paths.get("data/day1/input")

    readLines[IO](resourcePath)
      .filter(_.nonEmpty)
      .map(_.toInt)
      .flatMap(mass => Stream.eval(fuelForFuel[IO](mass)))
      .compile
      .fold(0)(_ + _)
      .flatMap(response => IO { println(response) })
      .as(ExitCode.Success)
  }
}
