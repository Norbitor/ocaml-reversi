open Printf
open Scanf


(*
Legalny ruch:
 - pionek stawiany jest w linii prostej (pion,poziom) lub ukosnej względem
   innego pionku gracza 													=> zrobione
 - pionek musi być postawiony obok pionku przeciwnika						=> to trzeba podlaczyc do ifa
 - postawienie pionka powoduje zmiane koloru (u nas X na O lub odwrotnie)
   pionków w całej wyznaczonej linii										=> to trzeba zrobic
 - jeśli gracz nie ma legalnych ruchów, traci kolejkę na rzecz przeciwnika	=> tego nie ma, ale nie wiem czy robimy
*)

let player1 = 'X';;
let player2 = 'O';;
let boardSize = 8;;
let board = Array.make_matrix boardSize boardSize '_';;


let init_board brd =
    let xmid1 = (boardSize / 2 - 1, boardSize / 2 - 1) in
    let xmid2 = ((fst xmid1) + 1, (snd xmid1) + 1) in
    let omid1 = (boardSize / 2 - 1, boardSize / 2) in
    let omid2 = ((fst omid1) + 1, boardSize / 2 - 1) in
    brd.(fst xmid1).(snd xmid1) <- 'X';
    brd.(fst xmid2).(snd xmid2) <- 'X';
    brd.(fst omid1).(snd omid1) <- 'O';
    brd.(fst omid2).(snd omid2) <- 'O';;

    (* przypadki testowe przylegania przeciwnika
       (x-1, y-1) (x, y-1) (x+1, y-1)
       (x-1, y) (x+1, y)
       (x-1, y+1) (x, y+1), (x+, y+1)
    *)


(* sprawdza czy podane wspolrzedne ruchu nie wykraczaja poza plansze *)
let check_pos board x y =
  x >= 0 && 
  y >= 0 && 
  y < Array.length board && 
  x < Array.length board.(0)
;;


(* sprawdza czy ruch w danym kierunku jest mozliwy *)
let check_direction board opponent (x, y) (dx, dy) =
  let rec check_direction_rec (x, y) valid = 
  if not (check_pos board x y) then
  false
else(
  match board.(x).(y) with
  | '_' -> false
  | cell ->
  if cell = opponent then 
  check_direction_rec (x + dx, y + dy) true
else 
  valid
)
in check_direction_rec (x + dx, y + dy) false
;;


(* obraca pionki *)
let reverse_tokens board me opponent x y =
  let directions = 
  [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]
in
(List.iter 
  (fun (dx, dy) -> 
    if (check_direction board opponent (x, y) (dx, dy)) then
    let rec take (x, y) =
    if (check_pos board x y) then
    if (board.(x).(y) = opponent) then(
      board.(x).(y) <- me; 
      take (x + dx, y + dy)
    )
  in take (x + dx, y + dy)
)
directions);
board.(x).(y) <- me
;;


(* sprawdza czy w danym miejscu mozliwy jest ruch*)
let check_move board opponent x y =
  if not (check_pos board x y) then
  false
else(
  let directions = 
  [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]
in
match board.(x).(y) with
| '_' -> (List.fold_left
  (fun a b -> a || b) 
  false
  (List.map 
    (fun d -> check_direction board opponent (x, y) d) 
  directions))
| _ -> false
)
;;




let print_board b =
    printf "   1 2 3 4 5 6 7 8\n";
    printf "  ----------------\n";
    for i = 0 to Array.length board - 1 do
        printf "%d| " (i+1);
        for j = 0 to Array.length board.(i) - 1 do
            printf "%c " board.(i).(j)
        done;
        printf "\n";
    done;
    printf "\n";;

(* wykonanie ruchu *)
let do_move board player opponent = 
	let quit_loop = ref false in
		while not !quit_loop do
			Printf.printf "Player %c move: " player;
			let move = read_int() in 
		    	let row = (move / 10) - 1 in 
				    let col = (move mod 10) - 1 in
				    	if (board.(row).(col) = '_') && (check_move board opponent row col) then (
				    		board.(row).(col) <- player;
				    		reverse_tokens board player opponent row col;
				    		quit_loop := true
				    	)
				    	else Printf.printf "Bad move!\n";
		done;;


(* sprawdza czy wszystkie pola zostaly zapelnione = koniec gry *)
let is_finished board = 
	let finished = ref true in
		for i=0 to Array.length board-1 do
  			for j=0 to Array.length board.(i)-1 do
  				if board.(j).(i) = '_' then
  					finished := false;
			done;
		done;
		!finished
;;



(* liczy pionki graczy *)
let count_pawns board player = 
	let res = ref 0 in
		for i = 0 to (Array.length board) - 1 do
			for j = 0 to (Array.length board.(i)) - 1 do
				if board.(i).(j) = player then
					res := succ !res; (* succ = successor = inkrementacja *)
			done;
		done;
		!res
;;

(* sprawdza kto wygral *)
let winner board =
	Printf.printf "X: %d\nO: %d\n" (count_pawns board player1) (count_pawns board player2);
	let a = count_pawns board player1 in
		let b = count_pawns board player2 in
			if a > b then
				Printf.printf "\n%c win this game\n" player1
			else
				if a == b then
					Printf.printf "\nREMIS !!!\n"
				else
					Printf.printf "\n%c win this game\n" player2;;



init_board board;;


let start_game = ref false in
	while not !start_game do
	    print_board board;
	    do_move board player1 player2;
	    if (is_finished board) then
	    	start_game := true;
	    print_board board;
	    do_move board player2 player1;
	    if (is_finished board) then
	    	start_game := true
	done;
	winner board;;
