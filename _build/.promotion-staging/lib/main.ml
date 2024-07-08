open! Core
open! Async
open! Game_strategies_common_lib

module Exercises = struct
  (* Here are some functions which know how to create a couple different kinds
     of games *)
  let empty_game = Game.empty Game.Game_kind.Tic_tac_toe

  let place_piece (game : Game.t) ~piece ~position : Game.t =
    let board = Map.set game.board ~key:position ~data:piece in
    { game with board }
  ;;

  let win_for_x =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 2 }
  ;;

  let non_win =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
  ;;

  let print_game (game : Game.t) =
    match game.game_kind with 
    | Tic_tac_toe -> (
      let board = List.init 3 ~f:(fun x -> 
        List.init 3 ~f:(fun y -> 
          match (Map.find game.board {Game.Position.row = x; column = y}) with 
          | Some O -> "O"
          | Some X -> "X"
          | None -> " "
        )
      ) in
      print_endline "Current Board: ";
      List.iteri board ~f:(fun x row -> 
        List.iteri row ~f:(fun y item -> 
          print_string (item);
          if (y = 0 || y = 1) then print_string " | " else print_string "");

        if (x = 0 || x = 1) then (
            print_endline "";
            print_endline "---------")
          else print_endline ""
        )
      )
    | Omok -> failwith "Not implemented yet . . ."
  ;;

  let%expect_test "print_win_for_x" =
    print_game win_for_x;
    [%expect
      {|
      Current Board:
      X | O | X
      ---------
      O | O | X
      ---------
      O | X | X
      |}];
    return ()
  ;;

  let%expect_test "print_non_win" =
    print_game non_win;
    [%expect
      {|
      Current Board:
      X |   |
      ---------
      O |   |
      ---------
      O |   | X
      |}];
    return ()
  ;;

  (* Exercise 1 *)
  (* None goes to true and false is to filled up *)
  let available_moves (game : Game.t) : Game.Position.t list =
    match game.game_kind with 
    | Tic_tac_toe -> (
      let nineList = [[0;1;2];[3;4;5];[6;7;8]] in
      let posList = [] in
      let newPosList = List.foldi nineList ~f:(fun x currList listRow -> (List.foldi listRow ~f:(fun y currList _listItem -> 
        match Map.find game.board {Game.Position.row = x; column = y} with 
        | None -> currList @ [{Game.Position.row = x; column = y}]
        | _ -> posList
        
        ) ~init:(currList))) ~init:(posList) in
      newPosList
    )
    | Omok -> failwith "Game type not implemented"
  ;;

  (* Exercise 2 *)
  let evaluate (game : Game.t) : Game.Evaluation.t =
    ignore game;
    failwith "Implement me!"
  ;;

  (* Exercise 3 *)
  let winning_moves ~(me : Game.Piece.t) (game : Game.t) : Game.Position.t list =
    ignore me;
    ignore game;
    failwith "Implement me!"
  ;;

  (* Exercise 4 *)
  let losing_moves ~(me : Game.Piece.t) (game : Game.t) : Game.Position.t list =
    ignore me;
    ignore game;
    failwith "Implement me!"
  ;;

  let exercise_one =
    Command.async
      ~summary:"Exercise 1: Where can I move?"
      (let%map_open.Command () = return () in
       fun () ->
         let moves = available_moves win_for_x in
         print_s [%sexp (moves : Game.Position.t list)];
         let moves = available_moves non_win in
         print_s [%sexp (moves : Game.Position.t list)];
         return ())
  ;;

  let exercise_two =
    Command.async
      ~summary:"Exercise 2: Is the game over?"
      (let%map_open.Command () = return () in
       fun () ->
         let evaluation = evaluate win_for_x in
         print_s [%sexp (evaluation : Game.Evaluation.t)];
         let evaluation = evaluate win_for_x in
         print_s [%sexp (evaluation : Game.Evaluation.t)];
         return ())
  ;;

  let piece_flag =
    let open Command.Param in
    flag
      "piece"
      (required (Arg_type.create Game.Piece.of_string))
      ~doc:
        ("PIECE "
         ^ (Game.Piece.all |> List.map ~f:Game.Piece.to_string |> String.concat ~sep:", ")
        )
  ;;

  let exercise_three =
    Command.async
      ~summary:"Exercise 3: Is there a winning move?"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let winning_moves = winning_moves ~me:piece non_win in
         print_s [%sexp (winning_moves : Game.Position.t list)];
         return ())
  ;;

  let exercise_four =
    Command.async
      ~summary:"Exercise 4: Is there a losing move?"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let losing_moves = losing_moves ~me:piece non_win in
         print_s [%sexp (losing_moves : Game.Position.t list)];
         return ())
  ;;

  let command =
    Command.group
      ~summary:"Exercises"
      [ "one"  , exercise_one
      ; "two"  , exercise_two
      ; "three", exercise_three
      ; "four" , exercise_four
      ]
  ;;
end


let handle_take_turn (_client : unit) (_query)=
  let piece = Game.Piece.X in
  let position = {Game.Position.row = 0; column = 0} in
  let response = {Rpcs.Take_turn.Response.piece; position} in
  print_s [%message "Response" (response : Rpcs.Take_turn.Response.t)]; 
  return (response)
;;

let command_play =
  Command.async
    ~summary:"Play"
    (let%map_open.Command () = return ()
     and port = flag "-port" (required int) ~doc:"_ port to listen on" in
     fun () ->
       (* We should start listing on the supplied [port], ready to handle incoming
          queries for [Take_turn] and [Game_over]. We should also connect to the
          controller and send a [Start_game] to initiate the game. *)
        let%bind server =
        let implementations = Rpc.Implementations.create_exn
        ~on_unknown_rpc:`Close_connection
        ~implementations:[Rpc.Rpc.implement Rpcs.Take_turn.rpc handle_take_turn]
     in
          Rpc.Connection.serve
            ~implementations
            ~initial_connection_state:(fun _client_identity _client_addr ->
              (* This constructs the "client" values which are passed to the
                 implementation function above. We're just using unit for now. *)
              ())
            ~where_to_listen:(Tcp.Where_to_listen.of_port port)
            ()
        in
        Tcp.Server.close_finished server)
;;

let command =
  Command.group
    ~summary:"Game Strategies"
    [ "play", command_play; "exercises", Exercises.command ]
;;
