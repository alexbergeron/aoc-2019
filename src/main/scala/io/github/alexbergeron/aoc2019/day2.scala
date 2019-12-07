package io.github.alexbergeron.aoc2019

import cats.effect._
import cats.syntax.all._
import fs2.Stream
import java.nio.file.Path
import java.nio.file.Paths

trait Day2 extends Common {

  case class Run(program: Vector[Int], operationIndex: Int)

  def readProgram[F[_]: ContextShift: Sync](path: Path): F[Vector[Int]] = {
    readLines[F](path)
      .filter(_.nonEmpty)
      .flatMap(line => Stream.emits(line.split(",")))
      .map(_.toInt)
      .compile
      .toVector
  }

  def executeProgram[F[_]: Sync](program: Vector[Int]): F[Vector[Int]] = {
    stepProgram[F](program, 0)
  }

  def stepProgram[F[_]: Sync](program: Vector[Int], opIndex: Int): F[Vector[Int]] = {
    program(opIndex) match {
      case 1 => Sync[F].suspend { stepProgram[F](add(program, opIndex + 1, opIndex + 2, opIndex + 3), opIndex + 4) }
      case 2 => Sync[F].suspend { stepProgram[F](mult(program, opIndex + 1, opIndex + 2, opIndex + 3), opIndex + 4) }
      case 99 => Sync[F].pure(program)
      case opcode => Sync[F].raiseError(new Exception(s"Unknown opcode: $opcode"))
    }
  }

  def add(program: Vector[Int], first: Int, second: Int, store: Int): Vector[Int] = {
    program.updated(program(store), program(program(first)) + program(program(second)))  
  }

  def mult(program: Vector[Int], first: Int, second: Int, store: Int): Vector[Int] = {
    program.updated(program(store), program(program(first)) * program(program(second)))
  }

  def setInputs(program: Vector[Int], noun: Int, verb: Int): Vector[Int] = {
    program.updated(1, noun).updated(2, verb)
  }
}

object Day2Test extends Day2 with IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    val program = Vector(1,1,1,4,99,5,6,0,99)
    executeProgram[IO](program)
      .flatMap(program => IO { println(program.mkString(",")) })
      .as(ExitCode.Success)
  } 
}

object Day2Part1 extends Day2 with IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    val resourcePath: Path = Paths.get("data/day2/input")

    readProgram(resourcePath)
      .map (setInputs(_, 12, 2))
      .flatMap(executeProgram[IO])
      .flatMap(program => IO { println(program(0)) })
      .as(ExitCode.Success)
  } 
}