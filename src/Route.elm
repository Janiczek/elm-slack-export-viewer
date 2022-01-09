module Route exposing (Route(..), fromUrl, toUrl)

import Data.Channel as Channel
import Data.YearMonthDay as YearMonthDay exposing (YearMonthDay)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


{-| TODO ChannelDayThread?
-}
type Route
    = ChannelsRoute
    | ChannelRoute Channel.Name
    | ChannelDayRoute Channel.Name YearMonthDay


toUrl : Route -> String
toUrl route =
    case route of
        ChannelsRoute ->
            "/"

        ChannelRoute name ->
            "/" ++ name

        ChannelDayRoute name ymd ->
            "/" ++ name ++ "/" ++ YearMonthDay.toString ymd


fromUrl : Url -> Route
fromUrl url =
    Parser.parse parser url
        |> Maybe.withDefault ChannelsRoute


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map ChannelsRoute Parser.top
        , Parser.map ChannelRoute Parser.string
        , Parser.map
            (\name year month day -> ChannelDayRoute name ( year, month, day ))
            (Parser.string </> Parser.int </> Parser.int </> Parser.int)
        ]
