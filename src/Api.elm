module Api exposing
    ( Action(..)
    , fetchChannelDays
    , fetchChannelLogs
    , fetchChannels
    )

import Data.Channel as Channel exposing (Channel)
import Data.Message as Message exposing (Message)
import Data.YearMonthDay as YearMonthDay exposing (YearMonthDay)
import Http
import Json.Decode as Decode exposing (Decoder)


type Action
    = FetchChannels
    | FetchChannelDays Channel.Name
    | FetchChannelLogs Channel.Name YearMonthDay


fetchChannels : Cmd (Result Http.Error (List Channel))
fetchChannels =
    Http.get
        { url = "/export/channels.json"
        , expect = Http.expectJson identity (Decode.list Channel.decoder)
        }


fetchChannelDays : Channel.Name -> Cmd (Result Http.Error (List YearMonthDay))
fetchChannelDays channelName =
    Http.get
        { url = "/export/" ++ channelName ++ "/days.json"
        , expect = Http.expectJson identity (Decode.list YearMonthDay.decoder)
        }


fetchChannelLogs : Channel.Name -> YearMonthDay -> Cmd (Result Http.Error (List Message))
fetchChannelLogs channelName ( y, m, d ) =
    Http.get
        { url =
            "/export/"
                ++ channelName
                ++ "/"
                ++ ([ y, m, d ]
                        |> List.map (String.fromInt >> String.padLeft 2 '0')
                        |> String.join "-"
                   )
                ++ ".json"
        , expect =
            Http.expectJson identity
                (Decode.list Message.decoder
                    |> Decode.map (List.filterMap identity)
                )
        }
