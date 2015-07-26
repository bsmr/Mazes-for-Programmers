defmodule Maze.CellTest do
	use ExUnit.Case

	test "create a new cell" do

		#{:ok, _pid} = Maze.CellSupervisor.start_link
		Maze.CellSupervisor.start_link
		
		cell_row = 2
		cell_column = 3

		{:ok, c} = Maze.Cell.new(cell_row, cell_column)

		assert {:ok, cell_row}    == Maze.Cell.row(c)
		assert {:ok, cell_column} == Maze.Cell.column(c)
		
		assert {:ok, nil} == Maze.Cell.north(c)
		assert {:ok, nil} == Maze.Cell.south(c)
		assert {:ok, nil} == Maze.Cell.east(c)
		assert {:ok, nil} == Maze.Cell.west(c)
		
		assert {:ok, []} == Maze.Cell.neighbours(c)
	end

	test "directions and neighbours" do
		
		#{:ok, _pid} = Maze.CellSupervisor.start_link
		Maze.CellSupervisor.start_link
		
		{:ok, c} = Maze.Cell.new(5, 5)
		{:ok, n} = Maze.Cell.new(6, 5)
		{:ok, s} = Maze.Cell.new(4, 5)
		{:ok, w} = Maze.Cell.new(5, 4)
		{:ok, e} = Maze.Cell.new(5, 6)

		assert Maze.Cell.north(c) == {:ok, nil}
		assert Maze.Cell.south(c) == {:ok, nil}
		assert Maze.Cell.east(c)  == {:ok, nil}
		assert Maze.Cell.west(c)  == {:ok, nil}

		{:ok, neighbours} = Maze.Cell.neighbours(c)
		assert length(neighbours) == 0
		assert Enum.member?(neighbours, n) == false
		assert Enum.member?(neighbours, s) == false
		assert Enum.member?(neighbours, e) == false
		assert Enum.member?(neighbours, w) == false

		:ok = Maze.Cell.north(c, n)

		assert Maze.Cell.north(c) == {:ok, n}
		assert Maze.Cell.south(c) == {:ok, nil}
		assert Maze.Cell.east(c)  == {:ok, nil}
		assert Maze.Cell.west(c)  == {:ok, nil}

		{:ok, neighbours} = Maze.Cell.neighbours(c)
		assert length(neighbours) == 1
		assert Enum.member?(neighbours, n) == true
		assert Enum.member?(neighbours, s) == false
		assert Enum.member?(neighbours, e) == false
		assert Enum.member?(neighbours, w) == false

		:ok = Maze.Cell.south(c, s)

		assert Maze.Cell.north(c) == {:ok, n}
		assert Maze.Cell.south(c) == {:ok, s}
		assert Maze.Cell.east(c)  == {:ok, nil}
		assert Maze.Cell.west(c)  == {:ok, nil}

		{:ok, neighbours} = Maze.Cell.neighbours(c)
		assert length(neighbours) == 2
		assert Enum.member?(neighbours, n) == true
		assert Enum.member?(neighbours, s) == true
		assert Enum.member?(neighbours, e) == false
		assert Enum.member?(neighbours, w) == false

		:ok = Maze.Cell.east(c, e)

		assert Maze.Cell.north(c) == {:ok, n}
		assert Maze.Cell.south(c) == {:ok, s}
		assert Maze.Cell.east(c)  == {:ok, e}
		assert Maze.Cell.west(c)  == {:ok, nil}

		{:ok, neighbours} = Maze.Cell.neighbours(c)
		assert length(neighbours) == 3
		assert Enum.member?(neighbours, n) == true
		assert Enum.member?(neighbours, s) == true
		assert Enum.member?(neighbours, e) == true
		assert Enum.member?(neighbours, w) == false

		:ok = Maze.Cell.west(c, w)

		assert Maze.Cell.north(c) == {:ok, n}
		assert Maze.Cell.south(c) == {:ok, s}
		assert Maze.Cell.east(c)  == {:ok, e}
		assert Maze.Cell.west(c)  == {:ok, w}

		{:ok, neighbours} = Maze.Cell.neighbours(c)
		assert length(neighbours) == 4
		assert Enum.member?(neighbours, n) == true
		assert Enum.member?(neighbours, s) == true
		assert Enum.member?(neighbours, e) == true
		assert Enum.member?(neighbours, w) == true

		:ok = Maze.Cell.east(c, nil)

		assert Maze.Cell.north(c) == {:ok, n}
		assert Maze.Cell.south(c) == {:ok, s}
		assert Maze.Cell.east(c)  == {:ok, nil}
		assert Maze.Cell.west(c)  == {:ok, w}

		{:ok, neighbours} = Maze.Cell.neighbours(c)
		assert length(neighbours) == 3
		assert Enum.member?(neighbours, n) == true
		assert Enum.member?(neighbours, s) == true
		assert Enum.member?(neighbours, e) == false
		assert Enum.member?(neighbours, w) == true
	end

	test "check links" do
		
		#{:ok, _pid} = Maze.CellSupervisor.start_link
		Maze.CellSupervisor.start_link
		
		{:ok, c1} = Maze.Cell.new(11, 12)
		{:ok, c2} = Maze.Cell.new(13, 14)
		{:ok, c3} = Maze.Cell.new(15, 16)

		{:ok, links} = Maze.Cell.links(c1)

		assert Enum.empty?(links) == true
		assert length(links) == 0

		assert :ok == Maze.Cell.link(c1, c2)
		assert {:ok, true} == Maze.Cell.linked?(c1, c2)
		assert {:ok, true} == Maze.Cell.linked?(c2, c1)

		assert :ok == Maze.Cell.link(c1, c3, false)
		assert {:ok, true} == Maze.Cell.linked?(c1, c3)
		assert {:ok, false} == Maze.Cell.linked?(c3, c1)
	end
	
end