namespace FSharp.Data.GraphQL.Serialization

open System
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Ast

[<RequireQualifiedAccess>]
module Value =

  let rec toJsonValue (v : Ast.Value) : JsonValue =
    match v with
    | NullValue -> JsonValue.Null
    | IntValue i ->
      if i > int64 Int32.MaxValue || i < int64 Int32.MinValue then
        JsonValue.Float (float i)
      else
        JsonValue.Integer (int i)
    | FloatValue f -> JsonValue.Float f
    | BooleanValue b -> JsonValue.Boolean b
    | StringValue s -> JsonValue.String s
    | EnumValue s -> JsonValue.String s
    | ListValue xs ->
      xs
      |> Seq.map toJsonValue
      |> Array.ofSeq
      |> JsonValue.Array
    | ObjectValue m ->
      m
      |> Map.toSeq
      |> Seq.map (fun (k, v) -> k, toJsonValue v)
      |> Array.ofSeq
      |> JsonValue.Record
    | Variable x -> JsonValue.String x

  let serialize v = (toJsonValue v).ToString(JsonSaveOptions.None)

module Auto =

  open System.Collections.Generic
  open System.Globalization
  open FSharp.Reflection

  let private listSerializer (serializer : obj -> Value) =
    fun (xs : obj) ->
      [
        for x in xs :?> System.Collections.IEnumerable do
          serializer x
      ]
      |> ListValue

  let rec private generateSerializerInteral (typ : Type) : (obj -> Value) =
    if typ.IsArray then
      typ.GetElementType() |> generateSerializerInteral |> listSerializer
    else if typ.FullName = typeof<string>.FullName then
      fun t -> StringValue (box t :?> string)
    else if typ.FullName = typeof<bool>.FullName then
      fun t -> BooleanValue (box t :?> bool)
    else if typ.FullName = typeof<int>.FullName then
      fun t -> IntValue (box t :?> int |> int64)
    else if typ.FullName = typeof<int64>.FullName then
      fun t -> IntValue (box t :?> int64)
    else if typ.FullName = typeof<float>.FullName then
      fun t -> FloatValue (box t :?> float)
    else if typ.FullName = typeof<DateTime>.FullName then
      fun t -> StringValue ((t :?> DateTime).ToString("o", CultureInfo.InvariantCulture))
    else if typ.FullName = typeof<Guid>.FullName then
      fun t -> StringValue ((box t :?> Guid).ToString())
    else if typ.IsGenericType && typ.GetGenericTypeDefinition().FullName = typedefof<obj option>.FullName then
      let serializer = generateSerializerInteral typ.GenericTypeArguments[0]

      fun value ->
        if isNull value then
          NullValue
        else
          let _, fields = FSharpValue.GetUnionFields(value, typ, true)
          serializer fields[0]

    else if typ.IsGenericType && typ.GetGenericTypeDefinition().FullName = typedefof<obj list>.FullName then
      let serializer = generateSerializerInteral typ.GenericTypeArguments[0]

      listSerializer serializer
    else if typ.IsGenericType && typ.GetGenericTypeDefinition().FullName = typedefof<obj seq>.FullName then
      let serializer = generateSerializerInteral typ.GenericTypeArguments[0]

      listSerializer serializer
    else if typ.IsGenericType && typ.GetGenericTypeDefinition().FullName = typedefof<Set<IComparable>>.FullName then
      let serializer = generateSerializerInteral typ.GenericTypeArguments[0]

      listSerializer serializer
    else if FSharpType.IsRecord(typ) then
      let extractors =
        [
          for pi in FSharpType.GetRecordFields(typ) do
            let k = pi.Name
            let v = generateSerializerInteral pi.PropertyType

            fun t ->
              k, v (pi.GetValue(t))
        ]

      fun (t : obj) ->
        [
          for extract in extractors do
            extract t
        ]
        |> Map.ofSeq
        |> ObjectValue
    else if FSharpType.IsUnion(typ, true) then
      fun (v : obj) ->
        let info, _ = FSharpValue.GetUnionFields(v, typ, true)
        StringValue info.Name
    else if FSharpType.IsTuple(typ) then
      let serializers =
        FSharpType.GetTupleElements(typ)
        |> Array.map generateSerializerInteral

      fun (v : obj) ->
        FSharpValue.GetTupleFields(v)
        |> Seq.map2 (fun serializer x -> serializer x) serializers
        |> Seq.toList
        |> ListValue
    else if (
      typ.IsGenericType
      && typ.GetGenericTypeDefinition().FullName = typedefof<Map<IComparable, obj>>.FullName
      && typ.GenericTypeArguments[0].FullName = typeof<string>.FullName) then
      let keyType = typ.GenericTypeArguments[0]
      let valueType = typ.GenericTypeArguments[1]
      let valueEncoder = generateSerializerInteral valueType
      let kvProps = typedefof<KeyValuePair<obj,obj>>.MakeGenericType(keyType, valueType).GetProperties()

      let valueSerializer = generateSerializerInteral typ.GenericTypeArguments[1]

      fun (v : obj) ->
        [
          for kvp in v :?> System.Collections.IEnumerable do
            let k = kvProps[0].GetValue(kvp) :?> string
            let v = kvProps[1].GetValue(kvp)

            k, valueSerializer v
        ]
        |> Map.ofSeq
        |> ObjectValue
    else
      // This behaviour is likely not what the user wants.
      // However, if they are not using standard F# constructs then
      // a custom serializer is the best option.
      fun x -> StringValue (string x)

  let generateSerializer<'t> () : ('t -> Value) =
    fun (t : 't) ->
      generateSerializerInteral (typeof<'t>) (box t)
