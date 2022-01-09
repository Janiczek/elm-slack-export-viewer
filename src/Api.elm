module Api exposing (fetchChannels
, fetchChannelDays)

import Http
import Json.Decode as Decode exposing (Decoder)
import Data.Channel as Channel expsoing (Channel)

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
