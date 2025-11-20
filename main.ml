open Engine

let display_board b =
  for y = 0 to 10 do
    for x = 0 to 10 do
      match (Board.mem (x,y) b) with
      | true -> print_string "#"
      | false -> print_string "."
    done;
    print_newline ()
  done;
  print_newline ()

let () =
  let blinker = [(1,1);(1,2);(1,3)] in
  let starting_board = Board.empty |> Board.add_cells blinker in
  let all_life = Gol.create starting_board in

  let rec loop t =
    all_life |> Gol.at_time t |> display_board;
    loop (t + 1)
  in
  
  loop 0 

