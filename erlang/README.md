# Mazes for Programmers - Erlang/OTP version

This is my [Erlang/OTP][] version of [Maces for Programmers][].

[Erlang/OTP]: http://erlang.org/
[Mazes for Programmers]: https://pragprog.com/book/jbmaze/mazes-for-programmers

## Abstract

The application consists of a main supervisor. The main supervisor
spawns a Grid server and a Cell supervisor. The Gridserver uses the
Cell supervisor to spawn and manage the cells needed for the mace.