module Main exposing (main)

import Browser
import Css
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
    | Error Http.Error


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
    | SubmitInput
    | GotResponse (Result Http.Error String)
    | TokenChanged String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChanged newInput ->
            ( { model | input = newInput }, Cmd.none )

        SubmitInput ->
            ( { model | responses = ( model.input, Loading ) :: model.responses }
            , Http.request
                { method = "POST"
                , headers =
                    [ Http.header "Authorization" ("Bearer " ++ model.bearerToken)
                    ]
                , url = "http://localhost:3000/communicate"
                , body = Http.jsonBody (Encode.object [ ( "input", Encode.string model.input ) ])
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
                            ( { model | responses = ( input, Success response ) :: rest, input = "" }, Cmd.none )

                        Err err ->
                            ( { model
                                | responses =
                                    ( input
                                    , Error err
                                    )
                                        :: rest
                              }
                            , Cmd.none
                            )

                _ ->
                    ( model, Cmd.none )

        TokenChanged newToken ->
            ( { model | bearerToken = newToken }, Cmd.none )


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
            , Html.form [ Html.Events.onSubmit SubmitInput ]
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
                                        Html.text ("Response: " ++ response)

                                    Error errorMsg ->
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
                                ]
                            ]
                    )
                    model.responses
                )
            ]
        ]
    }
