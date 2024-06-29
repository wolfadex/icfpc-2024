module Main exposing (main)

import Browser
import Css
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http
import Json.Encode as Encode


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type ResponseState
    = Initial
    | Loading
    | Success String
    | HttpError Http.Error
    | ParseError String


type alias Model =
    { input : String
    , responses : List ( String, ResponseState )
    , bearerToken : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { input = "", responses = [], bearerToken = "" }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = InputChanged String
    | SubmitInput String
    | GotResponse (Result Http.Error String)
    | TokenChanged String
    | ParseInput String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChanged newInput ->
            ( { model | input = newInput }, Cmd.none )

        SubmitInput input ->
            ( { model | responses = ( input, Loading ) :: model.responses }
            , Http.request
                { method = "POST"
                , headers =
                    [ Http.header "Authorization" ("Bearer " ++ model.bearerToken)
                    ]
                , url = "http://localhost:3000/communicate"
                , body = Http.jsonBody (Encode.object [ ( "input", Encode.string input ) ])
                , expect = Http.expectString GotResponse
                , timeout = Nothing
                , tracker = Nothing
                }
            )

        GotResponse result ->
            case model.responses of
                ( input, Loading ) :: rest ->
                    case result of
                        Ok response ->
                            ( { model | responses = ( input, Success response ) :: rest }, Cmd.none )

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
                    , case parseIcfpToHuman input of
                        Ok res ->
                            Success res

                        Err err ->
                            ParseError err
                    )
                        :: model.responses
              }
            , Cmd.none
            )


parseIcfpToHuman : String -> Result String String
parseIcfpToHuman input =
    case String.uncons input of
        Nothing ->
            Err "Expected input to be non-empty"

        Just ( indicator, rest ) ->
            case indicator of
                'T' ->
                    if String.isEmpty rest then
                        Ok "true"

                    else
                        Err "Expected empty string after T"

                'F' ->
                    if String.isEmpty rest then
                        Ok "false"

                    else
                        Err "Expected empty string after F"

                'S' ->
                    String.foldl
                        (\char res ->
                            case res of
                                Err err ->
                                    Err err

                                Ok acc ->
                                    let
                                        _ =
                                            Debug.log "char - code" ( char, Char.toCode char )
                                    in
                                    case Dict.get (Char.toCode char) charLookup of
                                        Nothing ->
                                            Err ("Un-mappable char: " ++ String.fromChar char)

                                        Just mappedChar ->
                                            Ok (mappedChar :: acc)
                        )
                        (Ok [])
                        rest
                        |> Result.map (String.fromList >> String.reverse)

                _ ->
                    Err ("Unsupported indicator: " ++ String.fromChar indicator)


{-| Maps 33 - 126 to one of the following chars

    abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!"#$%&'()*+,-./:;<=>?@[\]^_`|~<space><newline>

-}
charLookup : Dict Int Char
charLookup =
    Dict.fromList
        [ ( 33, 'a' )
        , ( 34, 'b' )
        , ( 35, 'c' )
        , ( 36, 'd' )
        , ( 37, 'e' )
        , ( 38, 'f' )
        , ( 39, 'g' )
        , ( 40, 'h' )
        , ( 41, 'i' )
        , ( 42, 'j' )
        , ( 43, 'k' )
        , ( 44, 'l' )
        , ( 45, 'm' )
        , ( 46, 'n' )
        , ( 47, 'o' )
        , ( 48, 'p' )
        , ( 49, 'q' )
        , ( 50, 'r' )
        , ( 51, 's' )
        , ( 52, 't' )
        , ( 53, 'u' )
        , ( 54, 'v' )
        , ( 55, 'w' )
        , ( 56, 'x' )
        , ( 57, 'y' )
        , ( 58, 'z' )
        , ( 59, 'A' )
        , ( 60, 'B' )
        , ( 61, 'C' )
        , ( 62, 'D' )
        , ( 63, 'E' )
        , ( 64, 'F' )
        , ( 65, 'G' )
        , ( 66, 'H' )
        , ( 67, 'I' )
        , ( 68, 'J' )
        , ( 69, 'K' )
        , ( 70, 'L' )
        , ( 71, 'M' )
        , ( 72, 'N' )
        , ( 73, 'O' )
        , ( 74, 'P' )
        , ( 75, 'Q' )
        , ( 76, 'R' )
        , ( 77, 'S' )
        , ( 78, 'T' )
        , ( 79, 'U' )
        , ( 80, 'V' )
        , ( 81, 'W' )
        , ( 82, 'X' )
        , ( 83, 'Y' )
        , ( 84, 'Z' )
        , ( 85, '0' )
        , ( 86, '1' )
        , ( 87, '2' )
        , ( 88, '3' )
        , ( 89, '4' )
        , ( 90, '5' )
        , ( 91, '6' )
        , ( 92, '7' )
        , ( 93, '8' )
        , ( 94, '9' )
        , ( 95, '!' )
        , ( 96, '"' )
        , ( 97, '#' )
        , ( 98, '$' )
        , ( 99, '%' )
        , ( 100, '&' )
        , ( 101, '\'' )
        , ( 102, '(' )
        , ( 103, ')' )
        , ( 104, '*' )
        , ( 105, '+' )
        , ( 106, ',' )
        , ( 107, '-' )
        , ( 108, '.' )
        , ( 109, '/' )
        , ( 110, ':' )
        , ( 111, ';' )
        , ( 112, '<' )
        , ( 113, '=' )
        , ( 114, '>' )
        , ( 115, '?' )
        , ( 116, '@' )
        , ( 117, '[' )
        , ( 118, '\\' )
        , ( 119, ']' )
        , ( 120, '^' )
        , ( 121, '_' )
        , ( 122, '`' )
        , ( 123, '|' )
        , ( 124, '~' )
        , ( 125, ' ' )
        , ( 126, '\n' )
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "ICFP Contest 2024"
    , body =
        [ Html.div []
            [ Html.h1 [] [ Html.text "ICFP Contest 2024" ]
            , Html.div [ Css.bearerTokenInput ]
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
                    , Html.Events.onClick (ParseInput model.input)
                    ]
                    [ Html.text "Parse" ]
                ]
            , Html.div []
                (List.map
                    (\( input, state ) ->
                        Html.div [ Css.response ]
                            [ Html.div [ Css.responseInput ] [ Html.text ("Input: " ++ input) ]
                            , Html.div [ Css.responseText ]
                                [ case state of
                                    Initial ->
                                        Html.text ""

                                    Loading ->
                                        Html.text "Loading..."

                                    Success response ->
                                        Html.div []
                                            [ Html.text ("Response: " ++ response)
                                            , Html.div [ Css.responseButtons ]
                                                [ Html.button
                                                    [ Html.Attributes.type_ "button"
                                                    , Html.Attributes.class "responseButton"
                                                    , Html.Events.onClick (SubmitInput response)
                                                    ]
                                                    [ Html.text "Submit" ]
                                                , Html.button
                                                    [ Html.Attributes.type_ "button"
                                                    , Html.Attributes.class "responseButton"
                                                    , Html.Events.onClick (ParseInput response)
                                                    ]
                                                    [ Html.text "Parse" ]
                                                ]
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
    }
