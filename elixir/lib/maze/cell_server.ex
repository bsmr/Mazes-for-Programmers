defmodule Maze.CellServer do
	use GenServer
	@moduledoc """
	Manage the state of a Maze.Cell.
	"""

	defstruct row: nil, column: nil, north: nil, south: nil, east: nil, west: nil, links: []

	# === API ============================================================

	def start_link(row, column), do: GenServer.start_link(__MODULE__, [row, column], [])
	
	def row(self), do: GenServer.call(self, :row)
	def column(self), do: GenServer.call(self, :column)

	def north(self, cell), do: GenServer.call(self, {:north, cell})
	def south(self, cell), do: GenServer.call(self, {:south, cell})
	def east(self, cell), do: GenServer.call(self, {:east, cell})
	def west(self, cell), do: GenServer.call(self, {:west, cell})

	def north(self), do: GenServer.call(self, :north)
	def south(self), do: GenServer.call(self, :south)
	def east(self), do: GenServer.call(self, :east)
	def west(self), do: GenServer.call(self, :west)

	def link(self, cell, bidi), do: GenServer.call(self, {:link, cell, bidi})
	def unlink(self, cell, bidi), do: GenServer.call(self, {:unlink, cell, bidi})

	def links(self), do: GenServer.call(self, :links)

	def linked?(self, cell), do: GenServer.call(self, {:is_linked, cell})

	def neighbours(self), do: GenServer.call(self, :neighbours)

	# === callbacks ======================================================
	
	def init([row, column]), do: {:ok, %Maze.CellServer{row: row, column: column}}

	def handle_call(:row, _from, state = %Maze.CellServer{row: row}), do: {:reply, {:ok, row}, state}
	def handle_call(:column, _from, state = %Maze.CellServer{column: column}), do: {:reply, {:ok, column}, state}

	def handle_call(:north, _from, state = %Maze.CellServer{north: north}), do: {:reply, {:ok, north}, state}
	def handle_call(:south, _from, state = %Maze.CellServer{south: south}), do: {:reply, {:ok, south}, state}
	def handle_call(:east,  _from, state = %Maze.CellServer{east:  east }), do: {:reply, {:ok, east},  state}
	def handle_call(:west,  _from, state = %Maze.CellServer{west:  west }), do: {:reply, {:ok, west},  state}

	#def handle_call({:north, cell}, _from, state = %Maze.CellServer{}), do: {:reply, :ok, %Maze.CellServer{state | north: cell}}
	#def handle_call({:south, cell}, _from, state = %Maze.CellServer{}), do: {:reply, :ok, %Maze.CellServer{state | south: cell}}
	#def handle_call({:east,  cell}, _from, state = %Maze.CellServer{}), do: {:reply, :ok, %Maze.CellServer{state | east:  cell}}
	#def handle_call({:west,  cell}, _from, state = %Maze.CellServer{}), do: {:reply, :ok, %Maze.CellServer{state | west:  cell}}

	def handle_call({:north, cell}, _from, state = %Maze.CellServer{}), do: {:reply, :ok, %{state | north: cell}}
	def handle_call({:south, cell}, _from, state = %Maze.CellServer{}), do: {:reply, :ok, %{state | south: cell}}
	def handle_call({:east,  cell}, _from, state = %Maze.CellServer{}), do: {:reply, :ok, %{state | east:  cell}}
	def handle_call({:west,  cell}, _from, state = %Maze.CellServer{}), do: {:reply, :ok, %{state | west:  cell}}

	def handle_call({:link, cell, bidi}, _from, state = %Maze.CellServer{links: links}) do
		if ((bidi == true) and is_pid(cell)), do: Maze.Cell.link(cell, self(), false)
		#{:reply, :ok, %Maze.CellServer{state | links: links ++ [cell]}}
		{:reply, :ok, %{state | links: links ++ [cell]}}
	end

	def handle_call({:unlink, cell, bidi}, _from, state = %Maze.CellServer{links: links}) do
		if ((bidi == true) and is_pid(cell)), do: Maze.Cell.unlink(cell, self(), false)
		#{:reply, :ok, %Maze.CellServer{state | links: links -- [cell]}}
		{:reply, :ok, %{state | links: links -- [cell]}}
	end

	def handle_call(:links, _from, state = %Maze.CellServer{links: links}), do: {:reply, {:ok, links}, state}

	def handle_call({:is_linked, cell}, _from, state = %Maze.CellServer{links: links}) do
		is_linked = Enum.member?(links, cell)
		{:reply, {:ok, is_linked}, state}
	end

	def handle_call(:neighbours, _from, state = %Maze.CellServer{north: north, south: south, east: east, west: west}) do
		neighbours = for n <- [north, south, east, west], is_pid(n), do: n
		{:reply, {:ok, neighbours}, state}
	end
	
end