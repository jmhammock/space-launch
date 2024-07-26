module Timestamp exposing (format, view)

import Html exposing (Html, span, text)
import Time exposing (Month(..))



-- view


view : Time.Zone -> Time.Posix -> Html msg
view timeZone timestamp =
    span [] [ text (format timeZone timestamp) ]


format : Time.Zone -> Time.Posix -> String
format timeZone timestamp =
    let
        month =
            case Time.toMonth timeZone timestamp of
                Jan ->
                    "Jan"

                Feb ->
                    "Feb"

                Mar ->
                    "Mar"

                Apr ->
                    "Apr"

                May ->
                    "May"

                Jun ->
                    "Jun"

                Jul ->
                    "Jul"

                Aug ->
                    "Aug"

                Sep ->
                    "Sep"

                Oct ->
                    "Oct"

                Nov ->
                    "Nov"

                Dec ->
                    "Dec"

        min =
            String.padLeft 2 '0' (String.fromInt (Time.toMinute timeZone timestamp))

        hour =
            String.padLeft 2 '0' (String.fromInt (Time.toHour timeZone timestamp))

        day =
            String.fromInt (Time.toDay timeZone timestamp)

        year =
            String.fromInt (Time.toYear timeZone timestamp)
    in
    month ++ " " ++ day ++ ", " ++ year ++ " " ++ hour ++ ":" ++ min
