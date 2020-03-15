module Synthesis

let abelar num = ( (num > 12) && (num < 3097) && (num % 12 = 0))

   // num > 12
    

let area num1 num2:float =
    
     match ( (num1 < 0.0) || (num2 < 0.0)) with
     | true -> failwith "no!!!!"
     | false -> num1*num2*0.5

        

let zollo num =   
    match (num < 0) with
    |true -> num * -1
    |false -> num *2

let min num1 num2 =
    match num1 <num2 with
    | true -> num1
    | false -> num2

let max num1 num2 =
    match num1 > num2 with
    | true -> num1
    | _ -> num2

let ofTime num1 num2 num3 = num1*3600 + num2*60 + num3


let toTime num =

        match (num < 0) with
        |true -> (0,0,0)
        | false ->
        
        let hour = num/3600
        let min = (num - hour*3600)/60
        let sec = num - hour*3600 - min*60
        (hour,min,sec)

let digits num =
        let rec dig num count:int =
            match ((num/10) = 0) with
            |true -> count
            |false -> (dig (num/10) (count + 1))
        dig num 1


let minmax num =
    let num1,num2,num3,num4 = num
    (min (min num1 num2) (min num3 num4)),(max (max num1 num2 ) (max num3 num4) )

let isLeap num =
    match num < 1582 with
    |true -> failwith"nOOOO!"
    |false ->
    match ((num % 4 = 0 && num % 100 <> 0) || (num % 400 = 0)) with
    |true -> true
    |false -> false

let month num =
       match num with
        |1 -> "January",31
        |2 -> "February",28
        |3 -> ("March", 31)
        |4 -> ("April", 30)
        |5 -> ("May", 31)
        |6 -> ("June", 30)
        |7 -> ("July", 31)
        |8 -> ("August", 31)
        |9 -> ("September", 30)
        |10 -> ("October", 31)
        |11 -> ("November", 30)
        |12 -> ("December", 31)
        |_ -> failwith "NOO!"
        

let toBinary _ =
    failwith "Not implemented"

let bizFuzz n =
        let cnt1 = 0
        let cnt2 = 0
        let cnt3 = 0
        let cnt4 = 1
        let rec counter n cnt1 cnt2 cnt3 cnt4 =
            match(n < 0) with
             | true -> (0,0,0)
             | false ->
                 match (cnt4 = n+1) with
                 |true -> (cnt1,cnt2,cnt3)
                 |false-> 
                     match ((cnt4 % 5 = 0) && (cnt4 % 3 = 0)) with
                     |true -> counter n (cnt1) (cnt2) (cnt3 + 1) (cnt4 + 1)
                     |false ->
                         match ((cnt4 % 3 = 0) )with
                         |true -> counter n (cnt1 + 1) (cnt2) (cnt3) (cnt4 + 1)
                         |false ->
                             match ((cnt4 % 5 = 0)) with
                             |true -> counter n cnt1 (cnt2 + 1) (cnt3) (cnt4 + 1)
                             |false -> counter n cnt1 cnt2 cnt3 (cnt4 + 1)
        counter n cnt1 cnt2 cnt3 cnt4
            


let monthDay num1 num2 =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"