(* The following program is an implementation of Conway's Game of Life
   by Tomaz Geddes de Almeida *)

(* A cell refers to a position which is a pair of integers. To create a
   set of cells (a board) we need to implement a module Cell that provides a method
   compare. We take the polymorphic compare from Stdlib. A cell has type Cell.t.*)
module Cell = struct
  type t = int * int

  let compare = Stdlib.compare
end

(* Our board inherits from a set of cells, ultimately providing the funciton step
   which computes the next generation of a board. We initialise the unnamed module that our
   Board inherits from using the functor Set.Make. Set.Make(Cell) returns the module with set
   operations corresponding to our Cell. *)
module Board = struct
  include Set.Make (Cell)

  let neighbours (x, y) =
    List.concat_map
      (fun dx -> List.map (fun dy -> (x + dx, y + dy)) [ -1; 0; 1 ])
      [ -1; 0; 1 ]
    |> List.filter (( <> ) (x, y))

  let frequencies list =
    let frequency l = fun e -> (e, List.length (List.filter (( = ) e) l)) in
    List.map (frequency list) (List.sort_uniq Stdlib.compare list)

  let add_cells cells board =
    List.fold_left (fun ret cell -> add cell ret) board cells

  let step board =
    let build_board b (cell, n) =
      if n = 3 || (n = 2 && mem cell board) then add cell b else b
    in

    to_list board |> List.concat_map neighbours |> frequencies
    |> List.fold_left build_board empty
end

(* Finally, the whole state of the game is one infinite lazilly evaluated 'list' of boards
   The module Gol provides the method at_time which takes this state and an integer t, and returns
   the board which we can think of as the board at the discrete time t. Or alternatively, the board
   after t steps. *)
module Gol = struct
  type t = Cons of Board.t * t Lazy.t

  let rec create board = Cons (board, lazy (create (Board.step board)))

  let rec at_time t (Cons (hd, tl)) =
    if t = 0 then hd else at_time (t - 1) (Lazy.force tl)
end
