#load @".paket\load\netcoreapp3.0\main.group.fsx"

open FParsec

// type UserState = unit

type UserState =
    { PhoneCount: int
      AddressCount: int }
    static member Default =
        { PhoneCount = 0
          AddressCount = 0 }

type Parser<'t> = Parser<'t, UserState>

module Common =
    let popenparens: Parser<_> = pchar '(' .>> spaces

    let pcloseparens = pchar ')' .>> spaces

    let pdash = pchar '-'

    let pcomma = pchar ',' .>> spaces

module PhoneNumber =
    open Common

    type PhoneNumber =
        { AreaCode: int
          Prefix: int
          LineNumber: int }

    let createPhoneNumber a p l =
        { AreaCode = a
          Prefix = p
          LineNumber = l }

    let pareacode = between popenparens pcloseparens pint32 <|> pint32 .>> pdash

    let pprefix = pint32 .>> pdash

    let plinenumber = pint32

    let pphonenumber = pipe3 pareacode pprefix plinenumber createPhoneNumber

module StreetAddress =
    open Common

    type State =
        | OH
        | KY
        | IN
        | TN

    type StreetAddress =
        { Street: int * string
          City: string
          State: State
          Zipcode: int }

    let createStreetAddress str c st zip =
        { Street = str
          City = c
          State = st
          Zipcode = zip }

    let isStreetName c = isLetter c || isAnyOf " ." c

    let pstreet = pint32 .>> spaces .>>. (many1Satisfy isStreetName) .>> pcomma |> attempt

    let pcity = many1Satisfy isLetter .>> pcomma

    let pstate =
        choice
            [ stringReturn "OH" OH
              stringReturn "KY" KY
              stringReturn "IN" IN
              stringReturn "TN" TN ]
        .>> spaces

    let pzipcode = pint32

    let paddress = pipe4 pstreet pcity pstate pzipcode createStreetAddress

module ContactInfo =

    open PhoneNumber
    open StreetAddress

    type ContactInfo =
        | Phone of PhoneNumber
        | Address of StreetAddress

    let pcontactinfo =
        [ paddress |>> Address
          pphonenumber |>> Phone ]
        |> List.map (fun p -> p .>> spaces)
        |> choice
        |> many



module State =
    open PhoneNumber
    open StreetAddress
    open ContactInfo

    let incrementPhoneNumber =
        let incrementPhone us = { us with PhoneCount = us.PhoneCount + 1 }

        updateUserState incrementPhone

    let incrementStreetAddress =
        let incrementAddress us = { us with AddressCount = us.AddressCount + 1 }

        updateUserState incrementAddress

    let pcontactinfo =
        [ paddress .>> incrementStreetAddress |>> Address .>> spaces
          pphonenumber .>> incrementPhoneNumber |>> Phone .>> spaces ]
        |> choice
        |> many

    let test p str =
        match runParserOnString p UserState.Default "test stream" str with
        | Success(result, us, _) ->
            printfn "Success: %A\n\n UserState: %A" result us
        | Failure(errMsg, _, _) -> printfn "Failure: %s" errMsg

module OrderOfOperations =

    open Common

    type Operator =
        | Addition
        | Subtraction
        | Multiplication
        | Division

    type Value =
        | Number of float
        | Variable of string
        | Operation of Operation
        | Negative of Value

    and Operation =
        { Operator: Operator
          LeftOperand: Value
          RightOperand: Value }

    let createOperation op x y =
        { Operator = op
          LeftOperand = x
          RightOperand = y }
        |> Operation

    let pvariable = many1Satisfy isLetter |>> Variable

    let pnumber = pfloat |>> Number

    let opp = new OperatorPrecedenceParser<Value, unit, UserState>()

    let poperation = opp.ExpressionParser

    let term = (pnumber <|> pvariable .>> spaces) <|> between popenparens pcloseparens poperation

    opp.TermParser <- term

    opp.AddOperator(InfixOperator("+", spaces, 1, Associativity.Left, createOperation Addition))
    opp.AddOperator(InfixOperator("-", spaces, 1, Associativity.Left, createOperation Subtraction))
    opp.AddOperator(InfixOperator("*", spaces, 2, Associativity.Left, createOperation Multiplication))
    opp.AddOperator(InfixOperator("/", spaces, 2, Associativity.Left, createOperation Division))
    opp.AddOperator(PrefixOperator("-", spaces, 3, true, Negative))

    let poperations =
        [ attempt (PhoneNumber.pphonenumber .>> spaces) >>% None
          attempt (StreetAddress.paddress .>> spaces) >>% None
          poperation .>> spaces |>> Some ]
        |> choice
        |> many
        |>> (List.choose id)

open System.IO
open Common
open PhoneNumber
open StreetAddress
open ContactInfo
open OrderOfOperations

let test p str =
    match run p str with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(errMsg, _, _) -> printfn "Failure: %s" errMsg

let readTxtFile name = sprintf "%s/Data/%s.txt" __SOURCE_DIRECTORY__ name |> File.ReadAllText
let contactInfoData = readTxtFile "contact-info"
let oppData = readTxtFile "opp"
