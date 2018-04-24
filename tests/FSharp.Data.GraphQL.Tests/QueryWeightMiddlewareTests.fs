module FSharp.Data.GraphQL.Tests.QueryWeightMiddlewareTests

open System
open System.Threading
open System.Collections.Concurrent
open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Server.Middlewares
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution


#nowarn "40"

type Root = {
    clientId : int
}

and Subject = 
    | A of A
    | B of B

and A = {
    id : int
    value : string
    subjects : int list
}

and B = {
    id : int
    value : int
    subjects : int list
}

let executor =
    let a1 : A = { id = 1; value = "A1"; subjects = [ 2; 6 ] }
    let a2 : A = { id = 2; value = "A2"; subjects = [ 1; 3; 5 ] }
    let a3 : A = { id = 3; value = "A3"; subjects = [ 1; 2; 4 ] }
    let b1 = { id = 4; value = 1000; subjects = [ 1; 5 ] }
    let b2 = { id = 5; value = 2000; subjects = [ 3; 4 ; 6] }
    let b3 = { id = 6; value = 3000; subjects = [ 1; 3; 5 ] }
    let al = [ a1; a2; a3 ]
    let bl = [ b1; b2; b3 ]
    let getA id = al |> List.tryFind (fun a -> a.id = id)
    let getB id = bl |> List.tryFind (fun b -> b.id = id)
    let subjects = (al |> List.map A) @ (bl |> List.map B)
    let getSubject id = 
        let matchesId id = function A a -> a.id = id | B b -> b.id = id
        subjects |> List.tryFind (matchesId id)
    let rec SubjectType = 
        Define.Union(
            name = "Subject",
            options = [ AType; BType ],
            resolveValue = (fun u -> match u with A a -> box a | B b -> box b),
            resolveType = (fun u -> match u with A _ -> upcast AType | B _ -> upcast BType))
    and AType = 
        Define.Object<A>(
            name = "A",
            isTypeOf = (fun o -> o :? A),
            fieldsFn = fun () ->
                [ Define.Field("id", Int, resolve = fun _ a -> a.id)
                  Define.Field("value", String, resolve = fun _ a -> a.value)
                  Define.Field("subjects", ListOf (Nullable SubjectType), 
                    resolve = fun _ (a : A) -> a.subjects |> List.map getSubject |> List.toSeq).WithQueryWeight(1.0) ])
    and BType = 
        Define.Object<B>(
            name = "B",
            isTypeOf = (fun o -> o :? B),
            fieldsFn = fun () ->
                [ Define.Field("id", Int, resolve = fun _ b -> b.id)
                  Define.Field("value", Int, resolve = fun _ b -> b.value)
                  Define.Field("subjects", ListOf (Nullable SubjectType), 
                    resolve = fun _ (b : B) -> b.subjects |> List.map getSubject |> List.toSeq).WithQueryWeight(1.0) ])
    let Query =
        Define.Object<Root>(
            name = "Query",
            fields = 
                [ Define.Field("A", Nullable AType, "A Field", [ Define.Input("id", Int) ], resolve = fun ctx _ -> getA (ctx.Arg("id")))
                  Define.Field("B", Nullable BType, "B Field", [ Define.Input("id", Int) ], resolve = fun ctx _ -> getB (ctx.Arg("id"))) ])
    let schema = Schema(Query)
    let middlewares = [ QueryWeightMiddleware<Root>(2.0) :> IExecutionMiddleware<Root> ]
    Executor(schema, middlewares)

let expectedErrors : Error list =
    [ ("Query complexity exceeds maximum threshold. Please reduce the amount of queried data and try again.", [ "" ]) ]

[<Fact>]
let ``Simple query: Should pass when below threshold``() =
    let query = 
        parse """query testQuery {
            A (id : 1) {
                id
                value
                subjects {
                    id
                    value
                }                
            }
        }"""
    let expected = 
        NameValueLookup.ofList [
            "A", upcast NameValueLookup.ofList [
                "id", upcast 1
                "value", upcast "A1"
                "subjects", upcast [ 
                    NameValueLookup.ofList [
                        "id", upcast 2
                        "value", upcast "A2"                    
                    ]
                    NameValueLookup.ofList [
                        "id", upcast 6
                        "value", upcast 3000
                    ]
                ]
            ]
        ]
    let result = query |> executor.AsyncExecute |> sync
    match result with
    | Direct (data, errors) ->
        empty errors
        data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQLResponse"

[<Fact>]
let ``Simple query: Should not pass when above threshold``() =
    let query = 
        parse """query testQuery {
            A (id : 1) {
                id
                value
                subjects {
                    id
                    value
                    subjects {
                        id
                        value
                    }
                }                
            }
        }"""
    let result = query |> executor.AsyncExecute |> sync
    match result with
    | Direct (data, errors) ->
        errors |> equals expectedErrors
        empty data
    | _ -> fail "Expected Direct GQLResponse"

[<Fact>]
let ``Deferred queries : Should pass when below threshold``() =
    let query = 
        parse """query testQuery {
            A (id : 1) {
                id
                value
                subjects @defer {
                    id
                    value
                }                
            }
        }"""
    let expected = 
        NameValueLookup.ofList [
            "A", upcast NameValueLookup.ofList [
                "id", upcast 1
                "value", upcast "A1"
            ]
        ]
    let expectedDeferred = 
        NameValueLookup.ofList [
            "data", upcast [
                NameValueLookup.ofList [
                    "id", upcast 2
                    "value", upcast "A2"                    
                ]
                NameValueLookup.ofList [
                    "id", upcast 6
                    "value", upcast 3000
                ]
            ]
            "path", upcast [ "A" :> obj; "subjects" :> obj ]
        ]
    let result = query |> executor.AsyncExecute |> sync
    use mre = new ManualResetEvent(false)
    let actualDeferred = ConcurrentBag<Output>()
    match result with
    | Deferred(data, errors, deferred) ->
        empty errors
        data.["data"] |> equals (upcast expected)
        deferred |> Observable.add (fun x -> actualDeferred.Add(x); mre.Set() |> ignore)
        if TimeSpan.FromSeconds(float 30) |> mre.WaitOne |> not
        then fail "Timeout while waiting for Deferred GQLResponse"
        actualDeferred |> single |> equals (upcast expectedDeferred)
    | _ -> fail "Expected Deferred GQLResponse"

[<Fact>]
let ``Streamed queries : Should pass when below threshold``() =
    let query = 
        parse """query testQuery {
            A (id : 1) {
                id
                value
                subjects @stream {
                    id
                    value
                }                
            }
        }"""
    let expected = 
        NameValueLookup.ofList [
            "A", upcast NameValueLookup.ofList [
                "id", upcast 1
                "value", upcast "A1"
            ]
        ]
    let expectedDeferred1 = 
        NameValueLookup.ofList [
            "data", upcast [
                NameValueLookup.ofList [
                    "id", upcast 2
                    "value", upcast "A2"                    
                ]
            ]
            "path", upcast [ "A" :> obj; "subjects" :> obj; 0 :> obj ]
        ]
    let expectedDeferred2 = 
        NameValueLookup.ofList [
            "data", upcast [
                NameValueLookup.ofList [
                    "id", upcast 6
                    "value", upcast 3000
                ]
            ]
            "path", upcast [ "A" :> obj; "subjects" :> obj; 1 :> obj ]
        ]
    let result = query |> executor.AsyncExecute |> sync
    use mre = new ManualResetEvent(false)
    let actualDeferred = ConcurrentBag<Output>()
    match result with
    | Deferred(data, errors, deferred) ->
        empty errors
        data.["data"] |> equals (upcast expected)
        deferred 
        |> Observable.add (fun x -> 
            actualDeferred.Add(x)
            if actualDeferred.Count = 2 then mre.Set() |> ignore)
        if TimeSpan.FromSeconds(float 30) |> mre.WaitOne |> not
        then fail "Timeout while waiting for Deferred GQLResponse"
        actualDeferred
        |> Seq.cast<NameValueLookup>
        |> contains expectedDeferred1
        |> contains expectedDeferred2
        |> ignore
    | _ -> fail "Expected Deferred GQLResponse"

[<Fact>]
let ``Deferred and Streamed queries : Should not pass when above threshold``() =
    let query = 
        sprintf """query testQuery {
            A (id : 1) {
                id
                value
                subjects @%s {
                    id
                    value
                    subjects {
                        id
                        value
                    }
                }                
            }
        }"""
    asts query
    |> Seq.map (executor.AsyncExecute >> sync)
    |> Seq.iter (fun result ->
        match result with
        | Direct(data, errors) ->
            errors |> equals expectedErrors
            empty data
        | _ -> fail "Expected Direct GQLResponse")

[<Fact>]
let ``Inline fragment query : Should pass when below threshold``() =
    let query = 
        parse """query testQuery {
            A (id : 1) {
                id
                value
                subjects {
                    ... on A {
                        id
                        value
                    }
                    ... on B {
                        id
                    }
                }                
            }
        }"""
    let expected = 
        NameValueLookup.ofList [
            "A", upcast NameValueLookup.ofList [
                "id", upcast 1
                "value", upcast "A1"
                "subjects", upcast [ 
                    NameValueLookup.ofList [
                        "id", upcast 2
                        "value", upcast "A2"                    
                    ]
                    NameValueLookup.ofList [
                        "id", upcast 6
                    ]
                ]
            ]
        ]
    let result = query |> executor.AsyncExecute |> sync
    match result with
    | Direct (data, errors) ->
        empty errors
        data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQLResponse"

[<Fact>]
let ``Inline fragment query : Should not pass when above threshold``() =
    let query = 
        parse """query testQuery {
            A (id : 1) {
                id
                value
                subjects {
                    ... on A {
                        id
                        value
                    }
                    ... on B {
                        id
                        subjects {
                            id
                            value
                            subjects {
                                id
                                value
                            }
                        }
                    }
                }                
            }
        }"""
    let result = query |> executor.AsyncExecute |> sync
    match result with
    | Direct (data, errors) ->
        errors |> equals expectedErrors
        empty data
    | _ -> fail "Expected Direct GQLResponse"