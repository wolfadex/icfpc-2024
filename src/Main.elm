module Main exposing (main)

import Browser
import Css
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http
import Json.Encode
import Markdown
import Result.Extra
import String.Extra


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type ResponseState
    = Loading
    | HttpError Http.Error
    | Success (List Expression)
    | ParseError String


type Scoreboard
    = LoadingScore
    | ScoreError Http.Error
    | ScoreLoaded String


type alias Model =
    { input : String
    , responses : List ( String, ResponseState )
    , bearerToken : String
    , scoreboard : Scoreboard
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { input = ""
      , responses = []
      , bearerToken = ""
      , scoreboard = ScoreError (Http.BadBody "Click Refresh")
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = InputChanged String
    | SubmitInput String
    | GotResponse (Result Http.Error String)
    | TokenChanged String
    | ParseInput String
    | EncodeInput
    | RefreshScoreboard
    | GotScoreboard (Result Http.Error String)


refreshScoreboard : String -> Cmd Msg
refreshScoreboard bearerToken =
    submitInputRequest bearerToken "S'%4}3#/2%\"/!2$" GotScoreboard


submitInputRequest : String -> String -> (Result Http.Error String -> Msg) -> Cmd Msg
submitInputRequest bearerToken input toMsg =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "Authorization" ("Bearer " ++ bearerToken)
            ]
        , url = "http://localhost:3000/communicate"
        , body = Http.stringBody "application/text" input
        , expect = Http.expectString toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChanged newInput ->
            ( { model | input = newInput }, Cmd.none )

        SubmitInput input ->
            ( { model | responses = ( input, Loading ) :: model.responses }
            , submitInputRequest model.bearerToken input GotResponse
            )

        EncodeInput ->
            case icfpEncode (String model.input) of
                Err _ ->
                    ( model, Cmd.none )

                Ok input ->
                    ( { model | input = input }, Cmd.none )

        GotResponse result ->
            case model.responses of
                ( input, Loading ) :: rest ->
                    case result of
                        Ok response ->
                            ( { model
                                | responses =
                                    ( input
                                    , case parseIcfp response of
                                        Ok res ->
                                            Success res

                                        Err err ->
                                            ParseError err
                                    )
                                        :: rest
                              }
                            , Cmd.none
                            )

                        Err httpError ->
                            ( { model | responses = ( input, HttpError httpError ) :: rest }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        TokenChanged newToken ->
            ( { model | bearerToken = newToken }, Cmd.none )

        ParseInput input ->
            ( { model
                | responses =
                    ( input
                    , case parseIcfp input of
                        Ok res ->
                            Success res

                        Err err ->
                            ParseError err
                    )
                        :: model.responses
              }
            , Cmd.none
            )

        RefreshScoreboard ->
            ( { model | scoreboard = LoadingScore }
            , refreshScoreboard model.bearerToken
            )

        GotScoreboard result ->
            case result of
                Ok response ->
                    ( { model
                        | scoreboard =
                            case parseString (String.dropLeft 1 response) of
                                Ok score ->
                                    ScoreLoaded score

                                Err err ->
                                    ScoreError (Http.BadBody err)
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "ICFP Contest 2024"
    , body =
        [ Html.div [ Css.container ]
            [ Html.div [ Css.scoreboardContainer ]
                [ Html.div [ Css.scoreboardHeader ]
                    [ Html.h2 [] [ Html.text "Scoreboard" ]
                    , Html.button
                        [ Html.Attributes.type_ "button"
                        , Css.responseButton
                        , Html.Events.onClick RefreshScoreboard
                        ]
                        [ Html.text "Refresh Scoreboard" ]
                    ]
                , Html.div [ Css.scoreboardContent ]
                    [ case model.scoreboard of
                        LoadingScore ->
                            viewLoadingIndicator

                        ScoreError error ->
                            Html.text (errorToString error)

                        ScoreLoaded score ->
                            Markdown.toHtmlWith
                                { githubFlavored = Just { tables = True, breaks = True }
                                , defaultHighlighting = Nothing
                                , sanitize = False
                                , smartypants = True
                                }
                                [ Html.Attributes.class "markdown-body" ]
                                score
                    ]
                ]
            , Html.div [ Css.mainContent ]
                [ Html.h1 [] [ Html.text "ICFP Contest 2024" ]
                , Html.div [ Css.inputContainer ]
                    [ Html.div [ Css.bearerTokenInput ]
                        [ Html.input
                            [ Html.Attributes.type_ "password"
                            , Html.Attributes.value model.bearerToken
                            , Html.Attributes.placeholder "Enter bearer token"
                            , Html.Events.onInput TokenChanged
                            ]
                            []
                        ]
                    , Html.form [ Html.Events.onSubmit (SubmitInput model.input) ]
                        [ Html.input
                            [ Html.Attributes.value model.input
                            , Html.Attributes.placeholder "Enter text here"
                            , Html.Events.onInput InputChanged
                            ]
                            []
                        , Html.button
                            [ Html.Attributes.type_ "submit"
                            , Html.Attributes.disabled (List.any (\( _, state ) -> state == Loading) model.responses)
                            ]
                            [ Html.text "Submit" ]
                        , Html.button
                            [ Html.Attributes.type_ "button"
                            , Css.responseButton
                            , Html.Events.onClick (ParseInput model.input)
                            ]
                            [ Html.text "Parse" ]
                        , Html.button
                            [ Html.Attributes.type_ "button"
                            , Css.responseButton
                            , Html.Events.onClick EncodeInput
                            ]
                            [ Html.text "Encode" ]
                        ]
                    ]
                , Html.div [ Css.responsesContainer ]
                    (List.map
                        (\( input, state ) ->
                            Html.div [ Css.response ]
                                [ Html.div [ Css.responseInput ] [ Html.text ("Input: " ++ input) ]
                                , Html.div [ Css.responseText ]
                                    [ case state of
                                        Loading ->
                                            viewLoadingIndicator

                                        Success expressions ->
                                            Html.div []
                                                [ Html.text "Parse success"
                                                , Html.div [ Css.expressionList ]
                                                    (List.map
                                                        (\expr ->
                                                            case expr of
                                                                String _ ->
                                                                    Html.code []
                                                                        [ Html.text (prettyPrint expr) ]

                                                                _ ->
                                                                    Html.text (prettyPrint expr)
                                                        )
                                                        expressions
                                                    )
                                                ]

                                        HttpError errorMsg ->
                                            Html.div [ Css.error ]
                                                [ Html.text
                                                    (case errorMsg of
                                                        Http.BadUrl url ->
                                                            "Bad URL: " ++ url

                                                        Http.Timeout ->
                                                            "Request timed out"

                                                        Http.NetworkError ->
                                                            "Network error"

                                                        Http.BadStatus status ->
                                                            "Bad status: " ++ String.fromInt status

                                                        Http.BadBody body ->
                                                            "Bad body: " ++ body
                                                    )
                                                ]

                                        ParseError errorMsg ->
                                            Html.div [ Css.error ]
                                                [ Html.text ("Parse error: " ++ errorMsg) ]
                                    ]
                                ]
                        )
                        model.responses
                    )
                ]
            ]
        ]
    }


viewLoadingIndicator : Html Msg
viewLoadingIndicator =
    Html.div [ Css.loadingContainer ]
        [ Html.div [ Css.loadingIndicator ]
            [ Html.div [] []
            , Html.div [] []
            , Html.div [] []
            , Html.div [] []
            ]
        , Html.div [ Css.loadingText ] [ Html.text "Loading..." ]
        ]


errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Request timed out"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus status ->
            "Bad status: " ++ String.fromInt status

        Http.BadBody body ->
            "Bad body: " ++ body



--


type Expression
    = Boolean Bool
    | Integer Int
    | String String
    | UnaryOp UrOp
    | BinaryOp BinOp
    | If
    | Lambda Int
    | Variable Int


prettyPrint : Expression -> String
prettyPrint expr =
    case expr of
        Boolean b ->
            "Boolean: "
                ++ (if b then
                        "true"

                    else
                        "false"
                   )

        Integer i ->
            "Integer: " ++ String.fromInt i

        String s ->
            "String: " ++ s

        UnaryOp op ->
            "Unary Operation: " ++ prettyPrintUrOp op

        BinaryOp op ->
            "Binary Operation: " ++ prettyPrintBinOp op

        If ->
            "If Expression"

        Lambda i ->
            "Lambda with " ++ String.fromInt i ++ " arguments"

        Variable i ->
            "Variable with index " ++ String.fromInt i


prettyPrintUrOp : UrOp -> String
prettyPrintUrOp op =
    case op of
        Negate ->
            "Negate"

        Not ->
            "Not"

        StringToInt ->
            "String to Int"

        IntToString ->
            "Int to String"


prettyPrintBinOp : BinOp -> String
prettyPrintBinOp op =
    case op of
        Add ->
            "Add"

        Subtract ->
            "Subtract"

        Multiply ->
            "Multiply"

        Divide ->
            "Divide"

        Modulo ->
            "Modulo"

        Equal ->
            "Equal"

        LessThan ->
            "Less Than"

        GreaterThan ->
            "Greater Than"

        And ->
            "And"

        Or ->
            "Or"

        Concat ->
            "Concat"

        Take ->
            "Take"

        Drop ->
            "Drop"

        Apply ->
            "Apply"


type UrOp
    = Negate
    | Not
    | StringToInt
    | IntToString


type BinOp
    = Add
    | Subtract
    | Multiply
    | Divide
    | Modulo
    | Equal
    | LessThan
    | GreaterThan
    | And
    | Or
    | Concat
    | Take
    | Drop
    | Apply


{-| Maps 33 - 126 to one of the following chars

    abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!"#$%&'()*+,-./:;<=>?@[\]^_`|~<space><newline>

-}
charLookup : Dict Char Char
charLookup =
    Dict.fromList
        [ ( '!', 'a' )
        , ( '"', 'b' )
        , ( '#', 'c' )
        , ( '$', 'd' )
        , ( '%', 'e' )
        , ( '&', 'f' )
        , ( '\'', 'g' )
        , ( '(', 'h' )
        , ( ')', 'i' )
        , ( '*', 'j' )
        , ( '+', 'k' )
        , ( ',', 'l' )
        , ( '-', 'm' )
        , ( '.', 'n' )
        , ( '/', 'o' )
        , ( '0', 'p' )
        , ( '1', 'q' )
        , ( '2', 'r' )
        , ( '3', 's' )
        , ( '4', 't' )
        , ( '5', 'u' )
        , ( '6', 'v' )
        , ( '7', 'w' )
        , ( '8', 'x' )
        , ( '9', 'y' )
        , ( ':', 'z' )
        , ( ';', 'A' )
        , ( '<', 'B' )
        , ( '=', 'C' )
        , ( '>', 'D' )
        , ( '?', 'E' )
        , ( '@', 'F' )
        , ( 'A', 'G' )
        , ( 'B', 'H' )
        , ( 'C', 'I' )
        , ( 'D', 'J' )
        , ( 'E', 'K' )
        , ( 'F', 'L' )
        , ( 'G', 'M' )
        , ( 'H', 'N' )
        , ( 'I', 'O' )
        , ( 'J', 'P' )
        , ( 'K', 'Q' )
        , ( 'L', 'R' )
        , ( 'M', 'S' )
        , ( 'N', 'T' )
        , ( 'O', 'U' )
        , ( 'P', 'V' )
        , ( 'Q', 'W' )
        , ( 'R', 'X' )
        , ( 'S', 'Y' )
        , ( 'T', 'Z' )
        , ( 'U', '0' )
        , ( 'V', '1' )
        , ( 'W', '2' )
        , ( 'X', '3' )
        , ( 'Y', '4' )
        , ( 'Z', '5' )
        , ( '[', '6' )
        , ( '\\', '7' )
        , ( ']', '8' )
        , ( '^', '9' )
        , ( '_', '!' )
        , ( '`', '"' )
        , ( 'a', '#' )
        , ( 'b', '$' )
        , ( 'c', '%' )
        , ( 'd', '&' )
        , ( 'e', '\'' )
        , ( 'f', '(' )
        , ( 'g', ')' )
        , ( 'h', '*' )
        , ( 'i', '+' )
        , ( 'j', ',' )
        , ( 'k', '-' )
        , ( 'l', '.' )
        , ( 'm', '/' )
        , ( 'n', ':' )
        , ( 'o', ';' )
        , ( 'p', '<' )
        , ( 'q', '=' )
        , ( 'r', '>' )
        , ( 's', '?' )
        , ( 't', '@' )
        , ( 'u', '[' )
        , ( 'v', '\\' )
        , ( 'w', ']' )
        , ( 'x', '^' )
        , ( 'y', '_' )
        , ( 'z', '`' )
        , ( '{', '|' )
        , ( '|', '~' )
        , ( '}', ' ' )
        , ( '~', '\n' )
        ]


charEncode : Dict Char Char
charEncode =
    charLookup
        |> Dict.toList
        |> List.map (\( a, b ) -> ( b, a ))
        |> Dict.fromList


parseIcfp : String -> Result String (List Expression)
parseIcfp input =
    input
        |> String.split " "
        |> List.foldr
            (\str res ->
                if String.Extra.isBlank str then
                    res

                else
                    case res of
                        Err err ->
                            Err err

                        Ok acc ->
                            case parseIcfpHelper str of
                                Err err ->
                                    Err err

                                Ok expr ->
                                    Ok (expr :: acc)
            )
            (Ok [])


parseIcfpHelper : String -> Result String Expression
parseIcfpHelper input =
    case String.uncons input of
        Nothing ->
            Err "Expected input to be non-empty"

        Just ( indicator, rest ) ->
            case indicator of
                'T' ->
                    if String.isEmpty rest then
                        Ok (Boolean True)

                    else
                        Err "Expected empty string after T"

                'F' ->
                    if String.isEmpty rest then
                        Ok (Boolean False)

                    else
                        Err "Expected empty string after F"

                'I' ->
                    if String.isEmpty rest then
                        Err "Expected a non-empty string after I"

                    else
                        rest
                            |> parseInt
                            |> Integer
                            |> Ok

                'S' ->
                    parseString rest
                        |> Result.map String

                'U' ->
                    case rest of
                        "-" ->
                            Ok (UnaryOp Negate)

                        "!" ->
                            Ok (UnaryOp Not)

                        "#" ->
                            Ok (UnaryOp StringToInt)

                        "$" ->
                            Ok (UnaryOp IntToString)

                        _ ->
                            Err ("Unsupported unary operator: " ++ rest)

                'B' ->
                    case rest of
                        "+" ->
                            Ok (BinaryOp Add)

                        "-" ->
                            Ok (BinaryOp Subtract)

                        "*" ->
                            Ok (BinaryOp Multiply)

                        "/" ->
                            Ok (BinaryOp Divide)

                        "%" ->
                            Ok (BinaryOp Modulo)

                        "<" ->
                            Ok (BinaryOp LessThan)

                        ">" ->
                            Ok (BinaryOp GreaterThan)

                        "=" ->
                            Ok (BinaryOp Equal)

                        "|" ->
                            Ok (BinaryOp Or)

                        "&" ->
                            Ok (BinaryOp And)

                        "." ->
                            Ok (BinaryOp Concat)

                        "T" ->
                            Ok (BinaryOp Take)

                        "D" ->
                            Ok (BinaryOp Drop)

                        "$" ->
                            Ok (BinaryOp Apply)

                        _ ->
                            Err ("Unsupported binary operator: " ++ rest)

                '?' ->
                    if String.isEmpty rest then
                        Ok If

                    else
                        Err ("Unsupported conditional body: " ++ rest)

                'L' ->
                    if String.isEmpty rest then
                        Err ("Unsupported lambda body: " ++ rest)

                    else
                        rest
                            |> parseInt
                            |> Lambda
                            |> Ok

                'v' ->
                    if String.isEmpty rest then
                        Err ("Unsupported variable: " ++ rest)

                    else
                        rest
                            |> parseInt
                            |> Variable
                            |> Ok

                _ ->
                    Err ("Unsupported indicator: " ++ String.fromChar indicator)


parseString : String -> Result String String
parseString input =
    String.foldr
        (\char res ->
            case res of
                Err err ->
                    Err err

                Ok acc ->
                    case Dict.get char charLookup of
                        Nothing ->
                            Err ("Un-mappable char: " ++ String.fromChar char)

                        Just mappedChar ->
                            Ok (mappedChar :: acc)
        )
        (Ok [])
        input
        |> Result.map String.fromList


parseInt : String -> Int
parseInt =
    String.toList
        >> List.map (Char.toCode >> (\c -> c - 33))
        >> List.foldr
            (\val ( total, index ) ->
                ( total + val * 94 ^ index, index + 1 )
            )
            ( 0, 0 )
        >> Tuple.first


icfpEncode : Expression -> Result () String
icfpEncode expr =
    case expr of
        Boolean b ->
            if b then
                Ok "T"

            else
                Ok "F"

        String str ->
            Ok
                ("S"
                    ++ (String.foldr
                            (\char acc ->
                                case Dict.get char charEncode of
                                    Nothing ->
                                        acc

                                    Just mappedChar ->
                                        mappedChar :: acc
                            )
                            []
                            str
                            |> String.fromList
                       )
                )

        _ ->
            Err ()
