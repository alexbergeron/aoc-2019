package io.github.alexbergeron.aoc2019

import cats.effect._
import fs2.{io, text, Stream}
import java.nio.file.Path

trait Common {
  def readLines[F[_]: ContextShift: Sync](path: Path): Stream[F, String] =
    Stream.resource(Blocker[F]).flatMap { blocker =>
      io.file
        .readAll[F](path, blocker, 4096)
        .through(text.utf8Decode)
        .through(text.lines)
    }
}
