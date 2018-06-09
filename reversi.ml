open Printf
open Scanf

let board = Array.make_matrix 5 5 'O';;


let print_board b =
    printf "   1 2 3 4 5\n";
    printf "  -----------\n";
    for i = 0 to Array.length board - 1 do
        printf "%d| " (i+1);
        for j = 0 to Array.length board.(i) - 1 do
            printf "%c " board.(i).(j)
        done;
        printf "\n";
    done;
    printf "  -----------\n";;





while true do
    print_board board;
    print_string "Your move: ";
    let move = read_int() in 
        let row = (move / 10) - 1 in 
        let col = (move mod 10) - 1 in 
            board.(row).(col) <- 'X';
done;