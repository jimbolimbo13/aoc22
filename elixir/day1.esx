IO.puts("Starting Advent of Code again...")

defmodule Day10 do
  def load_file(f_name) do
    file_lines = String.split(File.read!(f_name), "\n\n")
    max_cals = Enum.map(file_lines, &sum_strings/1)
    part1 = Enum.max(max_cals)

    part2 = Enum.sort(max_cals)
    |> Enum.reverse()
    |> Enum.slice(0,3)
    |> Enum.sum()

    IO.puts("Day1 part1: #{part1}")
    IO.puts("Day1 part2: #{part2}")
  end

  def sum_elves(this) do
    sum_strings(String.split(this, "\n"))
  end

  def sum_strings([head | tail]) when is_list([head | tail]) do
    if head == "" do
      0
    else
      String.to_integer(head) + sum_strings(tail)
    end
  end

  def sum_strings([]) do
    0
  end

  def sum_strings(this) do
    sum_strings(String.split(this, "\n"))
  end

end

file_name = "ex-input-day1.txt"
#file_name = "ex-test-day1.txt"
Day10.load_file(file_name)
