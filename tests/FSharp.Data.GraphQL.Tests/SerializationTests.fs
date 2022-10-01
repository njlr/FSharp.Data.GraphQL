module FSharp.Data.GraphQL.Tests.SerializationTests

open System
open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Serialization

[<Fact>]
let ``Auto generateSerializer works for int``() =
  let serializer = Auto.generateSerializer<int> ()

  serializer 0 |> equals (IntValue 0)
  serializer 1 |> equals (IntValue 1)
  serializer 123 |> equals (IntValue 123)
  serializer -7 |> equals (IntValue -7)

[<Fact>]
let ``Auto generateSerializer works for options``() =
  let serializer = Auto.generateSerializer<string option> ()

  serializer None |> equals NullValue
  serializer (Some "abc") |> equals (StringValue "abc")
  serializer (Some "") |> equals (StringValue "")

[<Fact>]
let ``Auto generateSerializer works for lists``() =
  let serializer = Auto.generateSerializer<bool list> ()

  let t = BooleanValue true
  let f = BooleanValue false

  serializer [ true ] |> equals (ListValue [ t ])
  serializer [] |> equals (ListValue [])
  serializer [ false; true; true; false; false ] |> equals (ListValue [ f; t; t; f; f ])

[<Fact>]
let ``Auto generateSerializer works for seq``() =
  let serializer = Auto.generateSerializer<int seq> ()

  serializer Seq.empty |> equals (ListValue [])
  serializer (seq { 1 }) |> equals (ListValue [ IntValue 1 ])
  serializer (seq { 1; 2; 3 }) |> equals (ListValue [ IntValue 1; IntValue 2; IntValue 3 ])

[<Fact>]
let ``Auto generateSerializer works for arrays``() =
  let serializer = Auto.generateSerializer<int array> ()

  serializer [||] |> equals (ListValue [])
  serializer [| 6; 7 |] |> equals (ListValue [ IntValue 6; IntValue 7 ])

[<Fact>]
let ``Auto generateSerializer works for DateTime``() =
  let serializer = Auto.generateSerializer<DateTime> ()

  let dt = DateTime.Parse("2022-09-30T22:04:30.8560000Z", null, Globalization.DateTimeStyles.RoundtripKind)

  serializer dt |> equals (StringValue "2022-09-30T22:04:30.8560000Z")

[<Fact>]
let ``Auto generateSerializer works for Guid``() =
  let serializer = Auto.generateSerializer<Guid> ()

  serializer (Guid.Parse("2fa55ac7-435b-46f0-9f5b-a6887582da30")) |> equals (StringValue "2fa55ac7-435b-46f0-9f5b-a6887582da30")

type private Fruit =
  | Apple
  | Banana
  | Cherry

[<Fact>]
let ``Auto generateSerializer works for discriminated unions``() =
  let serializer = Auto.generateSerializer<Fruit> ()

  serializer Apple |> equals (StringValue "Apple")
  serializer Banana |> equals (StringValue "Banana")
  serializer Cherry |> equals (StringValue "Cherry")

[<Fact>]
let ``Auto generateSerializer works for sets``() =
  let serializer = Auto.generateSerializer<Set<int>> ()

  serializer Set.empty |> equals (ListValue [])
  serializer (Set.ofSeq [ 1; 2; 4 ]) |> equals (ListValue [ IntValue 1; IntValue 2; IntValue 4 ])

[<Fact>]
let ``Auto generateSerializer works for Map<string, _>``() =
  let serializer = Auto.generateSerializer<Map<string, int>> ()

  let m =
    Map.ofSeq [
      "abc", 123
      "def", 456
    ]

  serializer m |> equals (ObjectValue (Map.ofSeq [ "abc", IntValue 123; "def", IntValue 456 ]))

type private TrafficLight =
  | Red = 0
  | Amber = 1
  | Green = 2

[<Fact>]
let ``Auto generateSerializer works for enums``() =
  let serializer = Auto.generateSerializer<TrafficLight> ()

  serializer TrafficLight.Red |> equals (IntValue 0)
  serializer TrafficLight.Amber |> equals (IntValue 1)
  serializer TrafficLight.Green |> equals (IntValue 2)
