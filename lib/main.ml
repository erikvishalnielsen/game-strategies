open! Core
open! Async
open! Game_strategies_common_lib

module Exercises = struct
  (* Here are some functions which know how to create a couple different
     kinds of games *)
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

  let diag_win_o =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 2 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
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
    | Tic_tac_toe ->
      let board =
        List.init 3 ~f:(fun x ->
          List.init 3 ~f:(fun y ->
            match
              Map.find game.board { Game.Position.row = x; column = y }
            with
            | Some O -> "O"
            | Some X -> "X"
            | None -> " "))
      in
      List.iteri board ~f:(fun x row ->
        List.iteri row ~f:(fun y item ->
          print_string item;
          if y = 0 || y = 1 then print_string " | " else print_string "");
        if x = 0 || x = 1
        then (
          print_endline "";
          print_endline "---------")
        else print_endline "")
    | Omok ->
      let board =
        List.init 15 ~f:(fun x ->
          List.init 15 ~f:(fun y ->
            match
              Map.find game.board { Game.Position.row = x; column = y }
            with
            | Some O -> "O"
            | Some X -> "X"
            | None -> " "))
      in
      List.iteri board ~f:(fun x row ->
        List.iteri row ~f:(fun y item ->
          Core.print_string item;
          if not (y = 14) then Core.print_string " | " else Core.print_string "");
        if not (x = 14)
        then (
          Core.print_endline "";
          Core.print_endline "---------")
        else Core.print_endline "")
  ;;

  let%expect_test "print_win_for_x" =
    print_game win_for_x;
    [%expect
      {|
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
    | Tic_tac_toe ->
      let nineList = [ [ 0; 1; 2 ]; [ 3; 4; 5 ]; [ 6; 7; 8 ] ] in
      let posList = [] in
      let newPosList =
        List.foldi
          nineList
          ~f:(fun x currList listRow ->
            List.foldi
              listRow
              ~f:(fun y currList _listItem ->
                match
                  Map.find game.board { Game.Position.row = x; column = y }
                with
                | None ->
                  currList @ [ { Game.Position.row = x; column = y } ]
                | _ -> currList)
              ~init:currList)
          ~init:posList
      in
      newPosList
    | Omok ->
      let posList =
        List.concat
          (List.init 15 ~f:(fun x ->
             List.init 15 ~f:(fun y ->
               match
                 Map.find game.board { Game.Position.row = x; column = y }
               with
               | None -> Some { Game.Position.row = x; column = y }
               | _ -> None)))
      in
      let finalList =
        List.filter posList ~f:(fun item ->
          match item with None -> false | _ -> true)
      in
      let newList =
        List.map finalList ~f:(fun item ->
          match item with
          | Some location -> location
          | _ -> failwith "This shouldn't happen")
      in
      newList
  ;;

  let vertWin ~(game : Game.t) ~x ~y ~letter ~inBounds ~winLength =
    let winList = List.init (winLength - 1) ~f:(fun i -> i + 1) in
    let winOrNot =
      List.fold
        winList
        ~f:(fun bool addX ->
          match inBounds ~x:(x + addX) ~y with
          | false -> false
          | true ->
            let pieceOpt =
              Map.find
                game.board
                { Game.Position.row = x + addX; column = y }
            in
            (match pieceOpt with
             | None -> false
             | Some piece ->
               (match Game.Piece.equal piece letter with
                | true -> bool
                | false -> false)))
        ~init:true
    in
    winOrNot
  ;;

  let horizWin ~(game : Game.t) ~x ~y ~letter ~inBounds ~winLength =
    let winList = List.init (winLength - 1) ~f:(fun i -> i + 1) in
    let winOrNot =
      List.fold
        winList
        ~f:(fun bool addY ->
          match inBounds ~x ~y:(y + addY) with
          | false -> false
          | true ->
            let pieceOpt =
              Map.find
                game.board
                { Game.Position.row = x; column = y + addY }
            in
            (match pieceOpt with
             | None -> false
             | Some piece ->
               (match Game.Piece.equal piece letter with
                | true -> bool
                | false -> false)))
        ~init:true
    in
    winOrNot
  ;;

  let diagTLBR ~(game : Game.t) ~x ~y ~letter ~inBounds ~winLength =
    let winList = List.init (winLength - 1) ~f:(fun i -> i + 1) in
    let winOrNot =
      List.fold
        winList
        ~f:(fun bool add ->
          match inBounds ~x:(x - add) ~y:(y + add) with
          | false -> false
          | true ->
            let pieceOpt =
              Map.find
                game.board
                { Game.Position.row = x - add; column = y + add }
            in
            (match pieceOpt with
             | None -> false
             | Some piece ->
               (match Game.Piece.equal piece letter with
                | true -> bool
                | false -> false)))
        ~init:true
    in
    winOrNot
  ;;

  let diagBLTR ~(game : Game.t) ~x ~y ~letter ~inBounds ~winLength =
    let winList = List.init (winLength - 1) ~f:(fun i -> i + 1) in
    let winOrNot =
      List.fold
        winList
        ~f:(fun bool add ->
          match inBounds ~x:(x + add) ~y:(y + add) with
          | false -> false
          | true ->
            let pieceOpt =
              Map.find
                game.board
                { Game.Position.row = x + add; column = y + add }
            in
            (match pieceOpt with
             | None -> false
             | Some piece ->
               (match Game.Piece.equal piece letter with
                | true -> bool
                | false -> false)))
        ~init:true
    in
    winOrNot
  ;;

  (* Exercise 2 *)
  let evaluate (game : Game.t) : Game.Evaluation.t =
    let boardLength = Game.Game_kind.board_length game.game_kind in
    let inBounds ~x ~y =
      if (x >= 0 && x < boardLength) && y >= 0 && y < boardLength
      then true
      else false
    in
    let winLength = Game.Game_kind.win_length game.game_kind in
    let indexList = List.init boardLength ~f:(fun i -> i) in
    let result =
      List.fold
        indexList
        ~f:(fun piece indexX ->
          List.fold
            indexList
            ~f:(fun piece indexY ->
              let currPiece =
                match
                  Map.find
                    game.board
                    { Game.Position.row = indexX; column = indexY }
                with
                | Some letter ->
                  let winVert =
                    vertWin
                      ~game
                      ~x:indexX
                      ~y:indexY
                      ~letter
                      ~inBounds
                      ~winLength
                  in
                  let winHoriz =
                    horizWin
                      ~game
                      ~x:indexX
                      ~y:indexY
                      ~letter
                      ~inBounds
                      ~winLength
                  in
                  let diag1 =
                    diagBLTR
                      ~game
                      ~x:indexX
                      ~y:indexY
                      ~letter
                      ~inBounds
                      ~winLength
                  in
                  let diag2 =
                    diagTLBR
                      ~game
                      ~x:indexX
                      ~y:indexY
                      ~letter
                      ~inBounds
                      ~winLength
                  in
                  (match winVert || winHoriz || diag1 || diag2 with
                   | true -> Some letter
                   | false -> piece)
                | None -> piece
              in
              currPiece)
            ~init:piece)
        ~init:None
    in
    match result with
    | Some letter -> Game.Evaluation.Game_over { winner = Some letter }
    | None ->
      (match List.is_empty (available_moves game) with
       | true -> Game.Evaluation.Game_over { winner = None }
       | false -> Game.Evaluation.Game_continues)
  ;;

  (*MAKE SURE TO ACCOUNT FOR CASE WHEN GAME IS OVER BUT NOBODY HAS WON*)

  let vertMove ~(game : Game.t) ~x ~y ~letter ~inBounds ~winLength =
    let winList = List.init winLength ~f:(fun i -> i) in
    let winOrNot =
      List.fold
        winList
        ~f:(fun sum addX ->
          match inBounds ~x:(x + addX) ~y with
          | false -> sum
          | true ->
            let pieceOpt =
              Map.find
                game.board
                { Game.Position.row = x + addX; column = y }
            in
            (match pieceOpt with
             | None -> sum
             | Some piece ->
               (match Game.Piece.equal piece letter with
                | true -> sum + 1
                | false -> sum)))
        ~init:0
    in
    match winOrNot = winLength - 1 with
    | false -> None
    | true ->
      (List.hd (List.filter_map winList ~f:(fun add ->
              match inBounds ~x:(x + add) ~y with
              | true ->
                (match
                   Map.find
                     game.board
                     { Game.Position.row = x + add; column = y }
                 with
                 | None -> Some { Game.Position.row = x + add; column = y }
                 | _ -> None)
              | false -> None)))
           
  ;;

  let horizMove ~(game : Game.t) ~x ~y ~letter ~inBounds ~winLength =
    let winList = List.init winLength ~f:(fun i -> i) in
    let winOrNot =
      List.fold
        winList
        ~f:(fun sum add ->
          match inBounds ~x ~y:(y + add) with
          | false -> sum
          | true ->
            let pieceOpt =
              Map.find game.board { Game.Position.row = x; column = y + add }
            in
            (match pieceOpt with
             | None -> sum
             | Some piece ->
               (match Game.Piece.equal piece letter with
                | true -> sum + 1
                | false -> sum)))
        ~init:0
    in
    match winOrNot = winLength - 1 with
    | false -> None
    | true ->
      (List.hd (List.filter_map winList ~f:(fun add ->
        match inBounds ~x:(x) ~y:(y+add) with
        | true ->
          (match
             Map.find
               game.board
               { Game.Position.row = x; column = y + add}
           with
           | None -> Some { Game.Position.row = x; column = y + add}
           | _ -> None)
        | false -> None)))
  ;;

  let diag1Move ~(game : Game.t) ~x ~y ~letter ~inBounds ~winLength =
    let winList = List.init winLength ~f:(fun i -> i) in
    let winOrNot =
      List.fold
        winList
        ~f:(fun sum add ->
          match inBounds ~x:(x - add) ~y:(y + add) with
          | false -> sum
          | true ->
            let pieceOpt =
              Map.find
                game.board
                { Game.Position.row = x - add; column = y + add }
            in
            (match pieceOpt with
             | None -> sum
             | Some piece ->
               (match Game.Piece.equal piece letter with
                | true -> sum + 1
                | false -> sum)))
        ~init:0
    in
    match winOrNot = winLength - 1 with
    | false -> None
    | true ->
      (List.hd (List.filter_map winList ~f:(fun add ->
        match inBounds ~x:(x-add) ~y:(y+add) with
        | true ->
          (match
             Map.find
               game.board
               { Game.Position.row = x - add; column = y + add}
           with
           | None -> Some { Game.Position.row = x - add; column = y + add}
           | _ -> None)
        | false -> None)))
  ;;

  let diag2Move ~(game : Game.t) ~x ~y ~letter ~inBounds ~winLength =
    let winList = List.init winLength ~f:(fun i -> i) in
    let winOrNot =
      List.fold
        winList
        ~f:(fun sum add ->
          match inBounds ~x:(x + add) ~y:(y + add) with
          | false -> sum
          | true ->
            let pieceOpt =
              Map.find
                game.board
                { Game.Position.row = x + add; column = y + add }
            in
            (match pieceOpt with
             | None -> sum
             | Some piece ->
               (match Game.Piece.equal piece letter with
                | true -> sum + 1
                | false -> sum)))
        ~init:0
    in
    match winOrNot = winLength - 1 with
    | false -> None
    | true ->
      (List.hd (List.filter_map winList ~f:(fun add ->
        match inBounds ~x:(x+add) ~y:(y+add) with
        | true ->
          (match
             Map.find
               game.board
               { Game.Position.row = x + add; column = y + add}
           with
           | None -> Some { Game.Position.row = x + add; column = y + add}
           | _ -> None)
        | false -> None)))
  ;;

  (* Exercise 3 *)
  (* GET RID OF LIST.HD ON EACH OF THE FUNCTIONS IN CASE THERE ARE MULTIPLE OF ONE TYPE OF WIN *)
  let winning_moves ~(me : Game.Piece.t) (game : Game.t)
    : Game.Position.t list
    =
    let boardLength = Game.Game_kind.board_length game.game_kind in
    let allMoves =
      List.concat
        (List.init boardLength ~f:(fun x ->
           List.init boardLength ~f:(fun y ->
             { Game.Position.row = x; column = y })))
    in
    let inBounds ~x ~y =
      if (x >= 0 && x < boardLength) && y >= 0 && y < boardLength
      then true
      else false
    in
    let winLength = Game.Game_kind.win_length game.game_kind in
    let winnerMoves =
      List.fold
        allMoves
        ~f:(fun resultsList move ->
          match
            Map.find
              game.board
              { Game.Position.row = move.row; column = move.column }
          with
          | Some piece ->
            (match Game.Piece.equal piece me with
             | false -> resultsList
             | true ->
               resultsList
               @ [ vertMove
                     ~game
                     ~x:move.row
                     ~y:move.column
                     ~letter:me
                     ~inBounds
                     ~winLength
                 ; horizMove
                     ~game
                     ~x:move.row
                     ~y:move.column
                     ~letter:me
                     ~inBounds
                     ~winLength
                 ; diag1Move
                     ~game
                     ~x:move.row
                     ~y:move.column
                     ~letter:me
                     ~inBounds
                     ~winLength
                 ; diag2Move
                     ~game
                     ~x:move.row
                     ~y:move.column
                     ~letter:me
                     ~inBounds
                     ~winLength
                 ])
          | None ->
            resultsList
            @ [ vertMove
                  ~game
                  ~x:move.row
                  ~y:move.column
                  ~letter:me
                  ~inBounds
                  ~winLength
              ; horizMove
                  ~game
                  ~x:move.row
                  ~y:move.column
                  ~letter:me
                  ~inBounds
                  ~winLength
              ; diag1Move
                  ~game
                  ~x:move.row
                  ~y:move.column
                  ~letter:me
                  ~inBounds
                  ~winLength
              ; diag2Move
                  ~game
                  ~x:move.row
                  ~y:move.column
                  ~letter:me
                  ~inBounds
                  ~winLength
              ])
        ~init:[]
    in
    List.dedup_and_sort
      (List.map
         (List.filter winnerMoves ~f:(fun move ->
            match move with Some _move -> true | None -> false))
         ~f:(fun move ->
           match move with
           | Some move -> move
           | _ -> failwith "This shouldn't happen!"))
      ~compare:Game.Position.compare
  ;;

  (* Exercise 4 *)
  let losing_moves ~(me : Game.Piece.t) (game : Game.t) : Game.Position.t list =
    winning_moves ~me:(Game.Piece.flip me) game
  ;;

  let available_moves_that_do_not_immediately_lose ~(me : Game.Piece.t) (game : Game.t) : Game.Position.t list =
    let losingMoves = losing_moves ~me:me game in
    match (List.length losingMoves) with 
    | 1 -> losingMoves
    | 0 -> available_moves game
    | _ -> []
  ;;

  let tictacHeur ~(me : Game.Piece.t) (game : Game.t) ~isMax ~depth = 
    let getLosingMovesMe = losing_moves ~me:me game in
    let getWinningMovesMe = winning_moves ~me:me game in
    let getLosingMovesOpp = losing_moves ~me:(Game.Piece.flip me) game in
    let getWinningMovesOpp = winning_moves ~me:(Game.Piece.flip me) game in
    match isMax with 
    | true -> (
      (* MY TURN *)
      match List.length getWinningMovesMe with 
      | 0 -> (match List.length getLosingMovesMe with 
        | 0 -> 10
        | 1 -> -10
        | _ -> -10000000 * depth)
      | _ -> 10000000 * depth
    )
    | false -> (
      (* OPP TURN *)
      match List.length getWinningMovesOpp with 
      | 0 -> (match List.length getLosingMovesOpp with 
        | 0 -> -10
        | 1 -> 10
        | _ -> 10000000 * depth)
      | _ -> -10000000 * depth
    )
  ;;

  let rowIn ~(game : Game.t) ~x ~y ~letter ~inBounds ~xFunct ~yFunct ~openBefore ~inARow ~openAfter : bool =
    let desiredList = (List.init openBefore ~f:(fun _i -> None)) @ (List.init inARow ~f:(fun _i -> Some letter)) @ 
      (List.init openAfter ~f:(fun _i -> None)) in
    let yesOrNo = (
      List.foldi
        desiredList
        ~f:(fun add bool desiredPiece ->
          match inBounds ~x:(x + (xFunct*add)) ~y:(y + (yFunct*add)) with
          | false -> bool
          | true ->
            let pieceOpt =
              Map.find
                game.board
                { Game.Position.row = (x + (xFunct*add)); column = (y + (yFunct*add)) }
            in
            (match (pieceOpt, desiredPiece) with
            | None, None -> bool
            | Some piece, Some desired -> if (Game.Piece.equal piece desired) then bool else false
            | _ -> false
            ))
        ~init:(true))
    in
    yesOrNo
  ;;

  let allDirs ~(game : Game.t) ~x ~y ~letter ~inBounds ~openBefore ~(inARow : int) ~openAfter ~posScore : int = 
    let functHeur = [(0,1);(1,0);(1,1);(-1,1)] in
    let finalSum = List.fold ~init:(0) functHeur ~f:(fun sum funct -> 
      if (rowIn ~game ~x ~y ~letter ~inBounds ~xFunct:(fst funct) ~yFunct:(snd funct) ~openBefore ~inARow ~openAfter) then sum + posScore else sum
    ) in
    finalSum
  ;;

  let allMovesOmok = 
    List.init 225 ~f:(fun num -> { Game.Position.row = num / 15; column = num % 15 })
  ;;

  let omokHeur ~(me : Game.Piece.t) (game : Game.t) = 
    (*let getLosingMovesMe = losing_moves ~me:me game in
    let getWinningMovesMe = winning_moves ~me:me game in
    let getLosingMovesOpp = losing_moves ~me:(Game.Piece.flip me) game in
    let getWinningMovesOpp = winning_moves ~me:(Game.Piece.flip me) game in *)

    let inBounds ~x ~y =
      if (x >= 0 && x < (Game.Game_kind.board_length game.Game.game_kind) && y >= 0 && y < (Game.Game_kind.board_length game.Game.game_kind))
      then true
      else false
    in
    let totalScore = List.fold ~init:(0) (allMovesOmok) ~f:(fun sum move -> (
      let surroundingTiles = List.map Game.Position.all_offsets ~f:(fun funct -> funct move) in
      if (List.exists surroundingTiles ~f:(fun tile -> 
        match (Map.find game.board { Game.Position.row = tile.row; column = tile.column }) with 
        | None -> false
        | _ -> true
      )) then (
            sum + allDirs ~game ~x:move.row ~y:move.column ~letter:me ~inBounds ~openBefore:0 ~inARow:5 ~openAfter:0 ~posScore:100000000000 + 
            allDirs ~game ~x:move.row ~y:move.column ~letter:me ~inBounds ~openBefore:1 ~inARow:4 ~openAfter:1 ~posScore:10000000 +
            allDirs ~game ~x:move.row ~y:move.column ~letter:me ~inBounds ~openBefore:1 ~inARow:4 ~openAfter:0 ~posScore:100000 + 
            allDirs ~game ~x:move.row ~y:move.column ~letter:me ~inBounds ~openBefore:0 ~inARow:4 ~openAfter:1 ~posScore:100000 + 
            allDirs ~game ~x:move.row ~y:move.column ~letter:me ~inBounds ~openBefore:1 ~inARow:3 ~openAfter:1 ~posScore:5000 + 
            allDirs ~game ~x:move.row ~y:move.column ~letter:me ~inBounds ~openBefore:1 ~inARow:3 ~openAfter:0 ~posScore:100 + 
            allDirs ~game ~x:move.row ~y:move.column ~letter:me ~inBounds ~openBefore:0 ~inARow:3 ~openAfter:1 ~posScore:100 + 
            allDirs ~game ~x:move.row ~y:move.column ~letter:me ~inBounds ~openBefore:1 ~inARow:2 ~openAfter:1 ~posScore:50 + 
            allDirs ~game ~x:move.row ~y:move.column ~letter:me ~inBounds ~openBefore:1 ~inARow:2 ~openAfter:0 ~posScore:25 +
            allDirs ~game ~x:move.row ~y:move.column ~letter:me ~inBounds ~openBefore:0 ~inARow:2 ~openAfter:1 ~posScore:25 + 
            allDirs ~game ~x:move.row ~y:move.column ~letter:(Game.Piece.flip me) ~inBounds ~openBefore:0 ~inARow:5 ~openAfter:0 ~posScore:(-1100000000) + 
            allDirs ~game ~x:move.row ~y:move.column ~letter:(Game.Piece.flip me) ~inBounds ~openBefore:1 ~inARow:4 ~openAfter:1 ~posScore:(-110000000) + 
            allDirs ~game ~x:move.row ~y:move.column ~letter:(Game.Piece.flip me) ~inBounds ~openBefore:1 ~inARow:4 ~openAfter:0 ~posScore:(-11000000) + 
            allDirs ~game ~x:move.row ~y:move.column ~letter:(Game.Piece.flip me) ~inBounds ~openBefore:0 ~inARow:4 ~openAfter:1 ~posScore:(-11000000) + 
            allDirs ~game ~x:move.row ~y:move.column ~letter:(Game.Piece.flip me) ~inBounds ~openBefore:1 ~inARow:3 ~openAfter:1 ~posScore:(-11000) +
            allDirs ~game ~x:move.row ~y:move.column ~letter:(Game.Piece.flip me) ~inBounds ~openBefore:1 ~inARow:3 ~openAfter:0 ~posScore:(-10) +
            allDirs ~game ~x:move.row ~y:move.column ~letter:(Game.Piece.flip me) ~inBounds ~openBefore:0 ~inARow:3 ~openAfter:1 ~posScore:(-10) +
            allDirs ~game ~x:move.row ~y:move.column ~letter:(Game.Piece.flip me) ~inBounds ~openBefore:1 ~inARow:2 ~openAfter:1 ~posScore:(-10) + 
            allDirs ~game ~x:move.row ~y:move.column ~letter:(Game.Piece.flip me) ~inBounds ~openBefore:1 ~inARow:2 ~openAfter:0 ~posScore:(-5) +
            allDirs ~game ~x:move.row ~y:move.column ~letter:(Game.Piece.flip me) ~inBounds ~openBefore:0 ~inARow:2 ~openAfter:1 ~posScore:(-5)
            )
            
            (*allDirs ~game ~x:move.row ~y:move.column ~letter:me ~inBounds ~openBefore:0 ~inARow:5 ~openAfter:0 ~posScore:(-100000000) + 
            allDirs ~game ~x:move.row ~y:move.column ~letter:me ~inBounds ~openBefore:1 ~inARow:4 ~openAfter:1 ~posScore:(-10000000) + 
            allDirs ~game ~x:move.row ~y:move.column ~letter:me ~inBounds ~openBefore:1 ~inARow:4 ~openAfter:0 ~posScore:(-10000) + 
            allDirs ~game ~x:move.row ~y:move.column ~letter:me ~inBounds ~openBefore:0 ~inARow:4 ~openAfter:1 ~posScore:(-10000) + 
            allDirs ~game ~x:move.row ~y:move.column ~letter:me ~inBounds ~openBefore:1 ~inARow:3 ~openAfter:1 ~posScore:(-475) + 
            allDirs ~game ~x:move.row ~y:move.column ~letter:me ~inBounds ~openBefore:1 ~inARow:3 ~openAfter:0 ~posScore:(-15) + 
            allDirs ~game ~x:move.row ~y:move.column ~letter:me ~inBounds ~openBefore:0 ~inARow:3 ~openAfter:1 ~posScore:(-15) + 
            allDirs ~game ~x:move.row ~y:move.column ~letter:me ~inBounds ~openBefore:1 ~inARow:2 ~openAfter:1 ~posScore:(-10) + 
            allDirs ~game ~x:move.row ~y:move.column ~letter:me ~inBounds ~openBefore:1 ~inARow:2 ~openAfter:0 ~posScore:(-5) +
            allDirs ~game ~x:move.row ~y:move.column ~letter:me ~inBounds ~openBefore:0 ~inARow:2 ~openAfter:1 ~posScore:(-5) )*)
      else sum + 0)) in 
            (*allDirs ~game ~x:move.row ~y:move.column ~letter:(Game.Piece.flip me) ~inBounds ~openBefore:1 ~inARow:3 ~openAfter:0 ~posScore:(-75) + 
            allDirs ~game ~x:move.row ~y:move.column ~letter:(Game.Piece.flip me) ~inBounds ~openBefore:1 ~inARow:2 ~openAfter:1 ~posScore:(-25) + 
            allDirs ~game ~x:move.row ~y:move.column ~letter:(Game.Piece.flip me) ~inBounds ~openBefore:1 ~inARow:2 ~openAfter:0 ~posScore:(-10)))*)
    totalScore
  ;;

  let heuristic ~(me : Game.Piece.t) (game : Game.t) ~isMax ~depth = 
    match game.game_kind with 
    | Tic_tac_toe -> tictacHeur ~depth:(depth) ~me:me game ~isMax:(isMax)
    | Omok -> omokHeur ~me:me game
  ;;

  let rec omokMinimax ~depth ~(me : Game.Piece.t) (game : Game.t) isMax = 
    let _inBounds ~x ~y =
      if (x >= 0 && x < (Game.Game_kind.board_length game.Game.game_kind) && y >= 0 && y < (Game.Game_kind.board_length game.Game.game_kind))
      then true
      else false
    in
    match depth = 0, evaluate game with 
    | _, Game.Evaluation.Game_over {winner} -> (
      match winner with
      | None -> 0
      | Some winnerPiece -> match (Game.Piece.equal me winnerPiece) with | true -> 10000000 * depth | false -> -10000000 * depth)
    | true, _ -> heuristic ~me:me game ~isMax:isMax ~depth:depth
    | _ -> (
      match isMax with
      | true -> (
        List.fold (available_moves game) ~init:(-10000000 * depth) ~f:(fun value pos -> 
          let newGame = 
            place_piece game ~piece:(me) ~position:({ Game.Position.row = pos.row; column = pos.column })
          in
          (*let score = (allDirs ~game ~x:pos.row ~y:pos.column ~letter:me ~inBounds ~openBefore:0 ~inARow:5 ~openAfter:0 ~posScore:10000000 + 
          allDirs ~game ~x:pos.row ~y:pos.column ~letter:me ~inBounds ~openBefore:1 ~inARow:4 ~openAfter:1 ~posScore:10000 + 
          allDirs ~game ~x:pos.row ~y:pos.column ~letter:(Game.Piece.flip me) ~inBounds ~openBefore:1 ~inARow:3 ~openAfter:1 ~posScore:(-7500) +
          allDirs ~game ~x:pos.row ~y:pos.column ~letter:(Game.Piece.flip me) ~inBounds ~openBefore:0 ~inARow:5 ~openAfter:0 ~posScore:(-100000) +
          allDirs ~game ~x:pos.row ~y:pos.column ~letter:(Game.Piece.flip me) ~inBounds ~openBefore:1 ~inARow:4 ~openAfter:1 ~posScore:(-100000) +
          allDirs ~game ~x:pos.row ~y:pos.column ~letter:(Game.Piece.flip me) ~inBounds ~openBefore:1 ~inARow:4 ~openAfter:0 ~posScore:(-10000) +
          allDirs ~game ~x:pos.row ~y:pos.column ~letter:me ~inBounds ~openBefore:1 ~inARow:4 ~openAfter:0 ~posScore:5000 + 
          allDirs ~game ~x:pos.row ~y:pos.column ~letter:me ~inBounds ~openBefore:1 ~inARow:3 ~openAfter:1 ~posScore:500 + 
          allDirs ~game ~x:pos.row ~y:pos.column ~letter:me ~inBounds ~openBefore:1 ~inARow:3 ~openAfter:0 ~posScore:100 + 
          allDirs ~game ~x:pos.row ~y:pos.column ~letter:me ~inBounds ~openBefore:1 ~inARow:2 ~openAfter:1 ~posScore:50 + 
          allDirs ~game ~x:pos.row ~y:pos.column ~letter:me ~inBounds ~openBefore:1 ~inARow:2 ~openAfter:0 ~posScore:25) in
          if (score > 0 && score > value / 2) then (Int.max (value) (omokMinimax ~depth:(depth-1) ~me:me newGame (not isMax))) else heuristic ~me:me newGame ~isMax:(not isMax) ~depth:depth-1*)
          (*let score = (heuristic ~me:me newGame ~isMax:(not isMax) ~depth:depth-1) in
          if (score >= value) then (Int.max (value) (omokMinimax ~depth:(depth-1) ~me:me newGame (not isMax))) else value*)

          (Int.max (value) (omokMinimax ~depth:(depth-1)~me:me newGame (not isMax)))
          ))
      | false -> (
        List.fold (available_moves game) ~init:(10000000 * depth) ~f:(fun value pos -> 
          let newGame = 
            place_piece game ~piece:(me) ~position:({ Game.Position.row = pos.row; column = pos.column })
          in
         (*let score = -1 * (allDirs ~game ~x:pos.row ~y:pos.column ~letter:me ~inBounds ~openBefore:0 ~inARow:5 ~openAfter:0 ~posScore:10000000 + 
          allDirs ~game ~x:pos.row ~y:pos.column ~letter:me ~inBounds ~openBefore:1 ~inARow:4 ~openAfter:1 ~posScore:10000 + 
          allDirs ~game ~x:pos.row ~y:pos.column ~letter:(Game.Piece.flip me) ~inBounds ~openBefore:1 ~inARow:3 ~openAfter:1 ~posScore:(-7500) +
          allDirs ~game ~x:pos.row ~y:pos.column ~letter:(Game.Piece.flip me) ~inBounds ~openBefore:0 ~inARow:5 ~openAfter:0 ~posScore:(-100000) +
          allDirs ~game ~x:pos.row ~y:pos.column ~letter:(Game.Piece.flip me) ~inBounds ~openBefore:1 ~inARow:4 ~openAfter:1 ~posScore:(-100000) +
          allDirs ~game ~x:pos.row ~y:pos.column ~letter:(Game.Piece.flip me) ~inBounds ~openBefore:1 ~inARow:4 ~openAfter:0 ~posScore:(-10000) +
          allDirs ~game ~x:pos.row ~y:pos.column ~letter:me ~inBounds ~openBefore:1 ~inARow:4 ~openAfter:0 ~posScore:5000 + 
          allDirs ~game ~x:pos.row ~y:pos.column ~letter:me ~inBounds ~openBefore:1 ~inARow:3 ~openAfter:1 ~posScore:500 + 
          allDirs ~game ~x:pos.row ~y:pos.column ~letter:me ~inBounds ~openBefore:1 ~inARow:3 ~openAfter:0 ~posScore:100 + 
          allDirs ~game ~x:pos.row ~y:pos.column ~letter:me ~inBounds ~openBefore:1 ~inARow:2 ~openAfter:1 ~posScore:50 + 
          allDirs ~game ~x:pos.row ~y:pos.column ~letter:me ~inBounds ~openBefore:1 ~inARow:2 ~openAfter:0 ~posScore:25) in 
          if (score < 0 && score < value / 2) then (Int.min (value) (omokMinimax ~depth:(depth-1) ~me:me newGame (not isMax))) else (heuristic ~me:me newGame ~isMax:(not isMax) ~depth:depth-1) * -1 *)
          (*let score = -1 * (heuristic ~me:(Game.Piece.flip me) newGame ~isMax:(not isMax) ~depth:depth-1) in
          (* Core.print_s [%message "SCORE: " (score : int)]; *)
          if (score <= value) then (Int.min (value) (omokMinimax ~depth:(depth-1) ~me:me newGame (not isMax))) else value*)

          (Int.min (value) (omokMinimax ~depth:(depth-1)~me:me newGame (not isMax)))
        )
    ))
  ;;

  (* TRUE IS MAX FALSE IS MIN. MAX IS ALWAYS ME*)
  let rec minimax ~depth ~(me : Game.Piece.t) (game : Game.t) isMax = 
    let curr_heuristic = heuristic ~me:me game ~isMax:isMax ~depth:depth in
    match depth = 0, evaluate game with 
    | _, Game.Evaluation.Game_over {winner} -> (
      match winner with
      | None -> 0
      | Some winnerPiece -> match (Game.Piece.equal me winnerPiece) with | true -> 10000000 * depth | false -> -10000000 * depth)
    | true, _ -> curr_heuristic
    | _ -> (
      match isMax with
      | true -> (
        List.fold (available_moves game) ~init:(-10000000 * depth) ~f:(fun value pos -> 
          let newGame = 
            place_piece game ~piece:(me) ~position:({ Game.Position.row = pos.row; column = pos.column })
          in
        (Int.max (value) (minimax ~depth:(depth-1) ~me:me newGame (not isMax)))))
      | false -> (
        List.fold (available_moves game) ~init:(10000000 * depth) ~f:(fun value pos -> 
          let newGame = 
            place_piece game ~piece:(Game.Piece.flip me) ~position:({ Game.Position.row = pos.row; column = pos.column })
          in
        (Int.min (value) (minimax ~depth:(depth-1) ~me:me newGame (not isMax)))))
    )
  ;;

  let findNextMove ~(me : Game.Piece.t) (game : Game.t) =
    let allMoves = available_moves game in
    match (game.game_kind) with 
    | Omok -> (
      let winners = (winning_moves ~me:me game) @ (losing_moves ~me:me game) in
      if not (List.length winners = 0) then (List.hd_exn winners) else (
      let maxMove = (List.fold allMoves ~init:(Int.min_value,{ Game.Position.row = -1; column = -1 }) ~f:(fun max move -> 
      let surroundingTiles = List.map Game.Position.all_offsets ~f:(fun funct -> funct move) in
      if (List.exists surroundingTiles ~f:(fun tile -> 
        match (Map.find game.board { Game.Position.row = tile.row; column = tile.column }) with 
        | None -> false
        | _ -> true
      )) then (
      let newGame = place_piece game ~piece:(me) ~position:(move) in
      let var = omokMinimax ~depth:1 ~me:me newGame false in (* CHANGE DEPTH HERE!! *)
      if var > fst max then var,move else max)
      else max)) in
    if (List.exists allMoves ~f:(fun move -> Game.Position.equal move { Game.Position.row = 7; column = 7 })) then { Game.Position.row = 7; column = 7 } 
    else (if (List.exists allMoves ~f:(fun move -> Game.Position.equal move { Game.Position.row = 8; column = 8 })) then { Game.Position.row = 8; column = 8 } 
      else (snd maxMove)))  (* IF ERROR ITS PROLLY THIS *))
    | Tic_tac_toe -> (let rankedMoves = (List.sort allMoves ~compare:(fun move1 move2 -> 
      let newGame1 = place_piece game ~piece:(me) ~position:(move1) in
      let newGame2 = place_piece game ~piece:(me) ~position:(move2) in
      if ((minimax ~depth:5 ~me:me newGame1 false) < (minimax ~depth:5 ~me:me newGame2 false)) then 1 else -1
    )) in 
    if (List.exists rankedMoves ~f:(fun move -> Game.Position.equal move { Game.Position.row = 1; column = 1 })) then { Game.Position.row = 1; column = 1 } 
    else List.hd_exn rankedMoves (* IF ERROR ITS PROLLY THIS *))
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
         let evaluation = evaluate non_win in
         print_s [%sexp (evaluation : Game.Evaluation.t)];
         let evaluation = evaluate diag_win_o in
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
         ^ (Game.Piece.all
            |> List.map ~f:Game.Piece.to_string
            |> String.concat ~sep:", "))
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

  let exercise_five =
    Command.async
      ~summary:"Exercise 5: Is immediately losing move?"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let losing_moves = available_moves_that_do_not_immediately_lose ~me:piece non_win in
         print_s [%sexp (losing_moves : Game.Position.t list)];
         return ())
  ;;

  let exercise_six =
    Command.async
      ~summary:"Exercise 6: Best move?"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let best_move = findNextMove ~me:piece non_win in
         print_s [%sexp (best_move : Game.Position.t)];
         return ())
  ;;
  let exercise_seven =
    Command.async
      ~summary:"Exercise 7: Omok against self"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         Core.print_s [%message "starting"];
         let list = List.init 225 ~f:(fun i -> i) in
         Core.print_s [%message (List.length list : int)];
         let _winner =
           List.fold
             list
             ~init:(Game.empty Game.Game_kind.Omok, piece)
             ~f:(fun (board, piece) _num ->
               Core.print_s [%message "BOARD: "];
               let best_move =
                 findNextMove board ~me:piece
               in
               Core.print_s [%message (best_move : Game.Position.t)];
               let new_board, next_piece =
                 place_piece board ~piece ~position:best_move, Game.Piece.flip piece
               in
               print_game new_board;
               new_board, next_piece)
         in
         return ())
  ;;

  let command =
    Command.group
      ~summary:"Exercises"
      [ "one", exercise_one
      ; "two", exercise_two
      ; "three", exercise_three
      ; "four", exercise_four
      ; "five", exercise_five
      ; "six", exercise_six
      ; "seven", exercise_seven
      ]
  ;;
end

let handle_take_turn (_client : unit) (query : Rpcs.Take_turn.Query.t) =
  let piece = query.you_play in
  let game = query.game in
  let position = Exercises.findNextMove game ~me:piece in

  let response = { Rpcs.Take_turn.Response.piece; position } in
  print_s [%message "Response" (response : Rpcs.Take_turn.Response.t)];
  return response
;;

let command_play =
  Command.async
    ~summary:"Play"
    (let%map_open.Command () = return ()
     and port = flag "-port" (required int) ~doc:"_ port to listen on" in
     fun () ->
       (* We should start listing on the supplied [port], ready to handle
          incoming queries for [Take_turn] and [Game_over]. We should also
          connect to the controller and send a [Start_game] to initiate the
          game. *)
       let%bind server =
         let implementations =
           Rpc.Implementations.create_exn
             ~on_unknown_rpc:`Close_connection
             ~implementations:
               [ Rpc.Rpc.implement Rpcs.Take_turn.rpc handle_take_turn ]
         in
         Rpc.Connection.serve
           ~implementations
           ~initial_connection_state:(fun _client_identity _client_addr ->
             (* This constructs the "client" values which are passed to the
                implementation function above. We're just using unit for
                now. *)
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
