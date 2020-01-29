module Prob

open System.Collections.Generic
open System.Linq;

type IDistribution<'T> =
    abstract member Sample : unit -> 'T

type SCU private () =
    static let singleton = SCU () :> IDistribution<double>
    static member Distribution with get () = singleton
    interface IDistribution<double> with
        member this.Sample () = Pseudorandom.nextDouble ()

type IDiscreteDistribution<'T> =
    inherit IDistribution<'T>
    abstract member Support : unit -> 'T list
    abstract member Weight : 'T -> int

type Empty<'T> private () =
    static let singleton = Empty<'T> () :> IDiscreteDistribution<'T>
    static member Distribution with get () = singleton
    interface IDiscreteDistribution<'T> with
        member _.Sample () = failwith "Cannot sample from empty distribution"
        member _.Support () = []
        member _.Weight (_ : 'T) = 0

type Singleton<'T> private (t : 'T) =
    static member Distribution (t : 'T) = Singleton<'T> t :> IDiscreteDistribution<'T>
    override _.ToString () = sprintf "Singleton[%A]" t
    interface IDiscreteDistribution<'T> with
        member _.Sample () = t
        member _.Support () = [t]
        member _.Weight (x : 'T) =
            if EqualityComparer<'T>.Default.Equals(t, x) then 1 else 0

type SDU private (min : int, max : int) =
    static member Distribution (min : int, max : int) : IDiscreteDistribution<int> =
        if min > max then Empty<int>.Distribution
        elif min = max then Singleton<int>.Distribution min
        else SDU (min, max) :> IDiscreteDistribution<int>
    interface IDiscreteDistribution<int> with
        member _.Sample () =
            int ((SCU.Distribution.Sample () * (1.0 + float max - float min)) + float min)
        member _.Support () = [min .. max]
        member _.Weight (i : int) =
            if (min <= i) && (i <= max) then 1 else 0

type Bernoulli private (zero : int, one : int) =
    static member Distribution (zero : int, one : int) : IDiscreteDistribution<int> =
        if zero < 0 || one < 0 then System.ArgumentException () |> raise
        elif zero = 0 && one = 0 then Empty<_>.Distribution
        elif zero = 0 then Singleton<_>.Distribution 1
        elif one = 0 then Singleton<_>.Distribution 0
        else
            let gcd = Extensions.gcd zero one
            Bernoulli (zero / gcd, one / gcd) :> IDiscreteDistribution<_>
    override _.ToString () = sprintf "Bernoulli[%d, %d]" zero one
    interface IDiscreteDistribution<int> with
        member _.Sample () =
            let p = float zero / float (zero + one)
            if SCU.Distribution.Sample () <= p then 0 else 1
        member _.Support () = [0; 1]
        member _.Weight (i : int) =
            if i = 0 then zero
            elif i = 1 then one
            else 0

type WeightedInteger private (ws : int list) =
    let weights = List.toArray ws
    let count = Array.length weights
    let distributions : IDistribution<int> array =
        let s = Array.sum weights
        let dist = Array.zeroCreate<IDistribution<int>> count
        let lows = Dictionary<int, int> ()
        let highs = Dictionary<int, int> ()
        for i = 0 to count - 1 do
            let w = weights.[i] * count
            if w = s then
                dist.[i] <- Singleton<int>.Distribution i :> IDistribution<_>
            elif w < s then
                lows.Add (i, w)
            else
                highs.Add (i, w)
        while lows.Any () do
            let low = lows.First ()
            lows.Remove low.Key |> ignore
            let high = highs.First()
            highs.Remove high.Key |> ignore
            let lowNeeds = s - low.Value
            dist.[low.Key] <- (Bernoulli.Distribution (low.Value, lowNeeds)) :> IDistribution<_>
            ()
        dist
    override _.ToString () = sprintf "WeightedInteger[%A]" weights
    interface IDiscreteDistribution<int> with
        member _.Support () = List.filter (fun x -> x <> 0) ws
        member _.Weight (i : int) =
            if 0 <= i && i < count then weights.[i]
            else 0
        member _.Sample () =
            let i = (SDU.Distribution (0, count - 1)).Sample ()
            distributions.[i].Sample ()
