module Pseudorandom

open System;
open System.Threading;

module private BetterRandom =
    type private CRNG = System.Security.Cryptography.RandomNumberGenerator
    let private crng =
        new ThreadLocal<CRNG> (
            Func<CRNG> (
                CRNG.Create
            )
        )
    let private bytes =
        new ThreadLocal<byte array> (
            Func<byte array> (
                fun () -> Array.zeroCreate sizeof<int>
            )
        )
    let nextInt () =
        crng.Value.GetBytes bytes.Value
        BitConverter.ToInt32 (bytes.Value, 0) &&& Int32.MaxValue
    let nextDouble () =
        let rec loop () =
            let hi = int64 (nextInt () &&& 0x001FFFFF) 
            let lo = int64 (nextInt ())
            let x = (hi <<< 31) ||| lo
            let n = double x
            let d = double (1L <<< 52)
            let q = n / d
            if q <> 1.0 then q else loop ()
        loop ()

let private prng =
    new ThreadLocal<Random> (
        Func<Random> (
            fun () -> Random (BetterRandom.nextInt ())
        )
    )
let nextInt () = prng.Value.Next ()
let nextDouble () = prng.Value.NextDouble ()
