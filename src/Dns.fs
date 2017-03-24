namespace DreamhostNet

open Common
open System
open Chiron.Mapping
open Chiron.Operators
open Chiron.Parsing

module Dns =
    type ICommandParam =
        abstract member ApiCommand : string

    type ListRecords = 
        | ListRecords
    with
        interface ICommandParam with
            member x.ApiCommand = "dns-list_records"
        interface IParameterizedCommand with
            member x.ToQueryParameters () =
                Map.empty<string,string>

    type DnsValue = 
        | A of Name:string * Value:string
        | MX of Name:string * Value:string
        | NS of Name:string * Value:string
        | CNAME of Name:string * Value:string
        | PTR of Name:string * Value:string
        | NAPTR of Name:string * Value:string
        | TXT of Name:string * Value:string
        | SRV of Name:string * Value:string
        | SPF of Name:string * Value:string
        | AAAA of Name:string * Value:string
        | A6 of Name:string * Value:string
        | UnknownType of Name:string * Value:string * Code:string
        member x.TypeCode = 
            match x with
            | A _ -> "A"
            | MX _ -> "MX"
            | NS _ -> "NS"
            | CNAME _ -> "CNAME"
            | PTR _ -> "PTR"
            | NAPTR _ -> "NAPTR"
            | TXT _ -> "TXT"
            | SRV _ -> "SRV"
            | SPF _ -> "SPF"
            | AAAA _ -> "AAAA"
            | A6 _ -> "A6"
            | UnknownType (_, _, typeCode) -> typeCode

    type AddRecord = 
        | AddRecord of Value:DnsValue * Comment:string option
    with
        interface IParameterizedCommand with
            member x.ToQueryParameters () =
                match x with
                | AddRecord (record, comment) ->
                    [
                        match record with
                        | A (name, value)
                        | MX (name, value)
                        | NS (name, value)
                        | CNAME (name, value)
                        | PTR (name, value)
                        | NAPTR (name, value)
                        | TXT (name, value)
                        | SRV (name, value)
                        | SPF (name, value)
                        | AAAA (name, value)
                        | A6 (name, value) ->
                            yield "record", name
                            yield "type", record.TypeCode
                            yield "value", value
                        | UnknownType (name, value, typeCode) ->
                            yield "record", name
                            yield "type", typeCode
                            yield "value", value
                        match comment with
                        | Some c -> yield "comment", c
                        | None -> ()
                    ] |> Map.ofList<string,string>
        interface ICommandParam with
            member x.ApiCommand = "dns-add_record"

    type DnsRecord = {
        AccountId : int
        Zone : string
        Value : DnsValue
        Comment : string option
        Editable : bool
    } 
    with
        static member FromJson (_:DnsRecord) =
                fun comment record recordValue zone editable recordType accountId ->
                    {
                        AccountId = Int32.Parse accountId
                        Zone = zone
                        Comment = match comment with | "" -> None | s -> Some s
                        Editable = editable |> function | "0" -> false | "1" -> true | _ -> false
                        Value = 
                            match recordType with
                            | "A" -> A (record, recordValue)
                            | "NS" -> NS (record, recordValue)
                            | "MX" -> MX (record, recordValue)
                            | "CNAME" -> CNAME (record, recordValue)
                            | "PTR" -> PTR (record, recordValue)
                            | "NAPTR" -> NAPTR (record, recordValue)
                            | "TXT" -> TXT (record, recordValue)
                            | "SRV" -> SRV (record, recordValue)
                            | "SPF" -> SPF (record, recordValue)
                            | "AAAA" -> AAAA (record, recordValue)
                            | "A6" -> A6 (record, recordValue)
                            | unknownType -> UnknownType (record, recordValue, unknownType)
                    }
            <!> Json.read "comment"
            <*> Json.read "record"
            <*> Json.read "value"
            <*> Json.read "zone"
            <*> Json.read "editable"
            <*> Json.read "type"
            <*> Json.read "account_id"
    
    type DnsRecordResponse = {
        Data: DnsRecord list
        Result: ResponseStatus
    }
    with
        static member FromJson (_:DnsRecordResponse) =
                fun data result reason ->
                    {
                        Data = data
                        Result = 
                            match result with
                            | "success" -> ResponseStatus.Success
                            | "error" -> 
                                match reason with
                                | Some r -> ResponseStatus.Error r
                                | None -> ResponseStatus.Error ""
                            | unknown -> ResponseStatus.Unknown unknown
                    }
            <!> Json.read "data"
            <*> Json.read "result"
            <*> Json.tryRead "reason"

    type SimpleResponse = {
        Data : string
    } with
        static member FromJson (_:SimpleResponse) =
                fun data ->
                    {
                        Data = data
                    }
            <!> Json.read "data"
    
    let deserializeResponse (json:string) = 
        json |> Json.parse |> Json.deserialize : DnsRecordResponse

    let deserializeSimpleResponse (json:string) = 
        json |> Json.parse |> Json.deserialize : SimpleResponse

    let listRecords key = 
        async {
            let cmd = ListRecords
            let apiParams = ApiParams(key, (cmd :> ICommandParam).ApiCommand, None, Some(Json))
            let! dnsRecords = get DefaultApiEndpoint apiParams cmd
            return deserializeResponse dnsRecords
        }

    let addRecord key record = 
        async {
            let addRecord = AddRecord(record, None)
            let apiParams = ApiParams(key, (addRecord :> ICommandParam).ApiCommand, None, Some(Json))
            let! result = post DefaultApiEndpoint apiParams addRecord
            return deserializeSimpleResponse result
        }
