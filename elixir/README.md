# Elixir Maces for Programmers

This is my [Elixir][] version of [Maces for Programmers][].

[Elixir]: http://elixir-lang.org/
[Mazes for Programmers]: https://pragprog.com/book/jbmaze/mazes-for-programmers

## Abstract

The application consists of a main supervisor. The main supervisor
spawns a Grid server and a Cell supervisor. The Gridserver uses the
Cell supervisor to spawn and manage the cells needed for the mace.