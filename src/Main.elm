module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Iso8601 as Iso
import Json.Decode as D
import Parser
import Platform.Cmd as Cmd
import Time



-- Main


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


initUrl : String
initUrl =
    "https://ll.thespacedevs.com/2.2.0/launch/upcoming/?format=json&limit=10"


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getLaunches initUrl )



-- Model


type Model
    = Failure String
    | Loading
    | Success Response


type alias Response =
    { count : Int
    , next : Maybe String
    , previous : Maybe String
    , results : Launches
    }


responseDecoder : D.Decoder Response
responseDecoder =
    D.map4 Response
        (D.field "count" D.int)
        (D.field "next" (D.nullable D.string))
        (D.field "previous" (D.nullable D.string))
        (D.field "results" (D.list launchDecoder))


type alias Launches =
    List Launch


type alias Launch =
    { id : String
    , url : String
    , name : String
    , status : LaunchStatus
    , image : String
    , windowStart : String
    }


launchDecoder : D.Decoder Launch
launchDecoder =
    D.map6 Launch
        (D.field "id" D.string)
        (D.field "url" D.string)
        (D.field "name" D.string)
        (D.field "status" statusDecoder)
        (D.field "image" D.string)
        (D.field "window_start" D.string)


type alias LaunchStatus =
    { name : String }


statusDecoder : D.Decoder LaunchStatus
statusDecoder =
    D.map LaunchStatus
        (D.field "name" D.string)



-- Update


type Msg
    = Next
    | Previous
    | GotResponse (Result Http.Error Response)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotResponse result ->
            case result of
                Ok response ->
                    ( Success response, Cmd.none )

                Err err ->
                    case err of
                        Http.BadStatus status ->
                            case status of
                                429 ->
                                    ( Failure "Too many requests", Cmd.none )

                                _ ->
                                    ( Failure "Failure", Cmd.none )

                        _ ->
                            ( Failure "Failure", Cmd.none )

        Next ->
            case model of
                Loading ->
                    ( Loading, Cmd.none )

                Success response ->
                    case response.next of
                        Just next ->
                            ( Loading, getLaunches next )

                        Nothing ->
                            ( Success response, Cmd.none )

                Failure message ->
                    ( Failure message, Cmd.none )

        Previous ->
            case model of
                Loading ->
                    ( Loading, Cmd.none )

                Success response ->
                    case response.previous of
                        Just previous ->
                            ( Loading, getLaunches previous )

                        Nothing ->
                            ( Success response, Cmd.none )

                Failure message ->
                    ( Failure message, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Upcoming Launches" ]
        , viewResponse model
        , viewButtons model
        ]


viewResponse : Model -> Html Msg
viewResponse model =
    case model of
        Failure message ->
            div []
                [ text message ]

        Loading ->
            progress [] []

        Success response ->
            div []
                (List.map viewLaunch response.results)


viewButtons : Model -> Html Msg
viewButtons model =
    case model of
        Failure _ ->
            div [] []

        Loading ->
            div [] []

        Success response ->
            case ( response.previous, response.next ) of
                ( Just _, Just _ ) ->
                    div [ class "grid" ]
                        [ button [ onClick Previous ] [ text "Previous" ]
                        , button [ onClick Next ] [ text "Next" ]
                        ]

                ( Just _, Nothing ) ->
                    div []
                        [ button [ onClick Previous ] [ text "Previous" ] ]

                ( Nothing, Just _ ) ->
                    div []
                        [ button [ onClick Next ] [ text "Next" ] ]

                _ ->
                    div [] []


viewLaunch : Launch -> Html Msg
viewLaunch launch =
    let
        windowStart =
            Iso.toTime launch.windowStart
    in
    article []
        [ header []
            [ div [ class "grid" ]
                [ figure
                    [ style "background-image" ("url(" ++ launch.image ++ ")")
                    , style "background-position" "center"
                    , style "background-size" "cover"
                    , style "height" "100px"
                    , style "width" "100px"
                    , style "border-radius" "0.25rem"
                    ]
                    []
                , div
                    [ style "display" "flex"
                    , style "align-items" "center"
                    ]
                    [ text launch.name ]
                ]
            ]
        , div [] [ text ("Status: " ++ launch.status.name) ]
        , div [] [ text ("Window Start: " ++ toHumanTime windowStart ++ " UTC") ]
        ]



-- HTTP


getLaunches : String -> Cmd Msg
getLaunches url =
    Http.get
        { url = url
        , expect = Http.expectJson GotResponse responseDecoder
        }



-- Human Time


toHumanTime : Result (List Parser.DeadEnd) Time.Posix -> String
toHumanTime time =
    case time of
        Ok t ->
            String.fromInt (Time.toYear Time.utc t)
                ++ "-"
                ++ toHumanMonth (Time.toMonth Time.utc t)
                ++ "-"
                ++ String.fromInt (Time.toDay Time.utc t)
                ++ " "
                ++ String.fromInt (Time.toHour Time.utc t)
                ++ ":"
                ++ String.fromInt (Time.toMinute Time.utc t)

        Err _ ->
            ""


toHumanMonth : Time.Month -> String
toHumanMonth month =
    case month of
        Time.Jan ->
            "Jan"

        Time.Feb ->
            "Feb"

        Time.Mar ->
            "Mar"

        Time.Apr ->
            "Apr"

        Time.May ->
            "May"

        Time.Jun ->
            "Jun"

        Time.Jul ->
            "Jul"

        Time.Aug ->
            "Aug"

        Time.Sep ->
            "Sep"

        Time.Oct ->
            "Oct"

        Time.Nov ->
            "Nov"

        Time.Dec ->
            "Dec"
