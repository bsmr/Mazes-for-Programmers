defmodule Maze.CellSupervisor do
	use Supervisor
	@moduledoc """
	Provide a simple_one_for_one manager for Maze.Cells.
	"""

	#def start_link(), do: Supervisor.start_link(__MODULE__, [], [name: __MODULE__])
	def start_link(), do: Supervisor.start_link(__MODULE__, [], [name: __MODULE__])

	def new(row, column), do: Supervisor.start_child(__MODULE__, [row, column])

	def init([]) do
		children = [
			worker(Maze.CellServer, [], restart: :temporary)
		]

		supervise(children, strategy: :simple_one_for_one)
	end
end