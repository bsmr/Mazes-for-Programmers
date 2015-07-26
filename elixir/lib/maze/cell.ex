defmodule Maze.Cell do
	@moduledoc """
	Handle a cell in a grid of a maze
	"""

	@doc """
	Create a new cell
	"""
	def new(row, column), do: Maze.CellSupervisor.new(row, column)

	@doc """
	Return the row of the cell
	"""
	def row(self), do: Maze.CellServer.row(self)

	@doc """
	Return the column of the cell
	"""
	def column(self), do: Maze.CellServer.column(self)

	@doc """
	Set cell to the north
	"""
	def north(self, cell), do: Maze.CellServer.north(self, cell)

	@doc """
	Set cell to the south
	"""
	def south(self, cell), do: Maze.CellServer.south(self, cell)

	@doc """
	Set cell to the east
	"""
	def east(self, cell), do: Maze.CellServer.east(self, cell)

	@doc """
	Set cell to the west
	"""
	def west(self, cell), do: Maze.CellServer.west(self, cell)

	@doc """
	Get cell to the north
	"""
	def north(self), do: Maze.CellServer.north(self)

	@doc """
	Get cell to the south
	"""
	def south(self), do: Maze.CellServer.south(self)

	@doc """
	Get cell to the east
	"""
	def east(self), do: Maze.CellServer.east(self)

	@doc """
	Get cell to the west
	"""
	def west(self), do: Maze.CellServer.west(self)

	@doc """
	Link to another cell
	"""
	def link(self, cell, bidi \\ true), do: Maze.CellServer.link(self, cell, bidi)

	@doc """
	Unlink from another cell
	"""
	def unlink(self, cell, bidi \\ true), do: Maze.CellServer.unlink(self, cell, bidi)

	@doc """
	Return all links
	"""
	def links(self), do: Maze.CellServer.links(self)

	@doc """
	Does a link to another cell exist?
	"""
	def linked?(self, cell), do: Maze.CellServer.linked?(self, cell)

	@doc """
	Return all neighbours
	"""
	def neighbours(self), do: Maze.CellServer.neighbours(self)

end