module Prob

open System.Collections.Generic
open System.Linq;

type IDistribution<'T> =
    abstract member Sample : unit -> 'T

type SCU private () =
    static let singleton = SCU () :> IDistribution<double>
    static member Distribution with get () = singleton
    interface IDistribution<double> with
        member _.Sample () = Pseudorandom.nextDouble ()

type IDiscreteDistribution<'T> =
    inherit IDistribution<'T>
    abstract member Support : unit -> 'T list
    abstract member Weight : 'T -> int

type Empty<'T> private () =
    static let singleton = Empty<'T> () :> IDiscreteDistribution<_>
    static member Distribution with get () = singleton
    interface IDiscreteDistribution<'T> with
        member _.Sample () = failwith "Cannot sample from empty distribution"
        member _.Support () = []
        member _.Weight _ = 0

type Singleton<'T> private (t : 'T) =
    static member Distribution t = Singleton<'T> t :> IDiscreteDistribution<_>
    override _.ToString () = sprintf "Singleton[%A]" t
    interface IDiscreteDistribution<'T> with
        member _.Sample () = t
        member _.Support () = [t]
        member _.Weight x =
            if EqualityComparer<'T>.Default.Equals(t, x) then 1 else 0

type Projected<'A, 'R when 'R : comparison> private (underlying : IDiscreteDistribution<'A>, projection : 'A -> 'R) =
    let weights =
        let sumWeight = List.map underlying.Weight >> List.sum
        underlying.Support ()
        |> List.groupBy projection
        |> List.map (fun (k, v) -> (k, sumWeight v))
        |> Map.ofList
    static member Distribution (u, p) =
        let result = Projected<_, _> (u, p) :> IDiscreteDistribution<_>
        match result.Support () with
        | []  -> Empty<_>.Distribution
        | [t] -> Singleton<_>.Distribution t
        | _   -> result
    interface IDiscreteDistribution<'R> with
        member _.Sample () = projection (underlying.Sample ());
        member _.Support () = Map.fold (fun keys k _ -> k::keys) [] weights
        member _.Weight r =
            match Map.tryFind r weights with
            | Some v -> v
            | _ -> 0

type SDU private (min : int, max : int) =
    static member Distribution (min, max) =
        if min > max then Empty<_>.Distribution
        elif min = max then Singleton<_>.Distribution min
        else SDU (min, max) :> IDiscreteDistribution<_>
    interface IDiscreteDistribution<int> with
        member _.Sample () =
            int ((SCU.Distribution.Sample () * (1.0 + float max - float min)) + float min)
        member _.Support () = [min .. max]
        member _.Weight i =
            if (min <= i) && (i <= max) then 1 else 0

type Bernoulli private (zero : int, one : int) =
    static member Distribution (zero, one) =
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
        member _.Weight i =
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
    static member Distribution weights =
        if List.exists (fun x -> x < 0) weights then
            System.ArgumentException () |> raise
        if not <| List.exists (fun x -> x > 0) weights then
            Empty<_>.Distribution
        else
            match weights with
            | [_]    -> Singleton<_>.Distribution 0
            | [x; y] -> Bernoulli.Distribution (x, y)
            | _      ->
                let gcd = Extensions.gcdOfList weights
                weights
                |> List.map (fun x -> x / gcd)
                |> WeightedInteger :> IDiscreteDistribution<_>
    override _.ToString () = sprintf "WeightedInteger[%A]" weights
    interface IDiscreteDistribution<int> with
        member _.Support () = List.filter (fun x -> x <> 0) ws
        member _.Weight i =
            if 0 <= i && i < count then weights.[i]
            else 0
        member _.Sample () =
            let i = (SDU.Distribution (0, count - 1)).Sample ()
            distributions.[i].Sample ()

let map<'A, 'R when 'R : comparison> (projection : 'A -> 'R) (d : IDiscreteDistribution<'A>) =
    let sumWeight = List.map d.Weight >> List.sum
    let dict =
        d.Support ()
        |> List.groupBy projection
        |> List.map (fun (k, v) -> (k, sumWeight v))
        |> Map.ofList
    let rs = Map.fold (fun keys key _ -> key::keys) [] dict
    let u = WeightedInteger.Distribution (List.map (fun r -> dict.[r]) rs)
    Projected<_, _>.Distribution (u, fun i -> rs.[i])
