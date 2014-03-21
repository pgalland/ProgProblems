(* Problem The Next Palindrome
http://www.spoj.com/problems/PALIN/ 
Code by Pierre Galland
*)

(* *********************************************** *)
(* *********************************************** *)


(* Structure is :
    * Auxilliary functions
    * Principal function
    * Application to test cases
*)


(* Auxilliary functions useful in the main function *)

let rec reverse_string s =
    let n = String.length s in
    let reverse = String.create n in
    for i = 0 to (n-1) do
        reverse.[i] <- s.[n-1-i];
    done;
    reverse;;
    


let rec all_nine_aux number pos length =
    
    if (pos >= length) then true
    else (number.[pos]=='9')&&(all_nine_aux number (pos+1) length)
    

let all_nine number = 
    all_nine_aux number 0 (String.length number);;


let plusOne c = match c with
    | '0' -> '1'
    | '1' -> '2'
    | '2' -> '3'
    | '3' -> '4'
    | '4' -> '5'
    | '5' -> '6'
    | '6' -> '7'
    | '7' -> '8'
    | '8' -> '9'
    | _ -> '*'
    
    
let rec next_number_aux number pos = 

    if (pos<0) then begin
        let n = String.length number in
        let result = String.make (n+1) '0' in
        result.[0] <- '1';
        result
        end
        
    else begin
    
        if ((compare number.[pos] '9') < 0) then begin
            number.[pos] <- plusOne number.[pos] ;
            number
        end
        
        else begin 
            number.[pos] <- '0';
            next_number_aux number (pos-1)
        end
        
    end;;

let next_number s = let n = String.length s in next_number_aux s (n-1) ;; 


(* *********************************************** *)
(* *********************************************** *)


(* Main function *)


(* There are several special cases, depending on the oddity, wheter the 
first half, second half are all nine digits, 
not that when the length in odd, the first half and the second half 
include the middle digit *)
let next_palindrome number =
    
    let n = String.length number in
    let oddity = n mod 2 in

    let firstHalf = String.sub number 0 ((n+oddity)/2) in
    let secondHalf = String.sub number ((n-oddity)/2) ((n+oddity)/2) in
    
    if (all_nine firstHalf) then begin
    
        if (all_nine secondHalf) then begin (*number is all nines in this case *)
            
            let result = String.make (n+1) '0' in
            result.[0] <- '1' ;
            result.[n] <- '1' ;
            result 
            end     
            
        else begin (* only the first half is nine *)
                     
            String.blit firstHalf 0 number ((n-oddity)/2) ((n+oddity)/2) ;          
            number
            end
            
        end
    
    else begin (* the first half is not all nine *)

        let firstHalf_reverse = reverse_string firstHalf in
        
        if (String.compare secondHalf firstHalf_reverse < 0) then begin
            
            String.blit firstHalf_reverse 0 number ((n-oddity)/2) ((n+oddity)/2) ;
            number
            end
        
        else begin
        
            let firstHalfPlusOne = next_number firstHalf in
            String.blit firstHalfPlusOne 0 number 0 ((n+oddity)/2) ;
            String.blit (reverse_string firstHalfPlusOne) 0 number ((n-oddity)/2) ((n+oddity)/2) ;
            number
            end
        
        end;;  


    
(* *********************************************** *)
(* *********************************************** *)

(* Application to the test cases *)

let rec go_test_case numberOfTestCase =
    if (numberOfTestCase > 0) then begin
        let number = read_line() in        
        print_string (next_palindrome number) ;
        print_string "\n" ;
        go_test_case (numberOfTestCase-1)
        end;;

let numberOfTest = read_int() in go_test_case numberOfTest ;;         


