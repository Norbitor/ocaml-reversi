open Printf
open Scanf

let player = ref 'X';;
let boardSize = 8;;
let board = Array.make_matrix boardSize boardSize '_';;


let init_board b =
    let xmid1 = (boardSize / 2 - 1, boardSize / 2 - 1) in
    let xmid2 = ((fst xmid1) + 1, (snd xmid1) + 1) in
    let omid1 = (boardSize / 2 - 1, boardSize / 2) in
    let omid2 = ((fst omid1) + 1, boardSize / 2 - 1) in
    board.(fst xmid1).(snd xmid1) <- 'X';
    board.(fst xmid2).(snd xmid2) <- 'X';
    board.(fst omid1).(snd omid1) <- 'O';
    board.(fst omid2).(snd omid2) <- 'O';;


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
    printf "  -----------\n";;


init_board board;;


while true do
    print_board board;
    print_string "Your move: ";
    let move = read_int() in 
    let row = (move / 10) - 1 in 
    let col = (move mod 10) - 1 in 
        board.(row).(col) <- 'X';
done;
