defmodule Elmfp.Grid do
	use GenServer

	def start_link(args) do
		GenServer.start_link __MODULE__, args
	end
	
	def start_link(rows, columns) do
		GenServer.start_link __MODULE__, rows, columns
	end
	
	def init(rows, columns) do
		{:stop, :error_not_implemented}
	end
end