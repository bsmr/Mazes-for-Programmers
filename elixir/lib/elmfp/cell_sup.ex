defmodule Elmfp.CellSup do
	use Supervisor

	def start(_type, _args) do
		import Supervisor.Spec, warn: false

		children = []

		opts = [strategy: :simple_one_for_one, name: Elmfp.CellSup]
		Supervisor.start_link(children, opts)
	end
end
