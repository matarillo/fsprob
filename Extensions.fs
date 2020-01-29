module Extensions

open System.Collections.Generic

let rec gcd a b =
    if b = 0 then a else gcd b (a % b)

let gcdOfList =
    List.reduce gcd

let lcm a b =
    a * b / (gcd a b)

let lcmOfList =
    List.fold lcm 1
