module Data.YearMonthDay exposing (YearMonthDay, decoder)

import Json.Decode as Decode exposing (Decoder)


type alias YearMonthDay =
    ( Int, Int, Int )


decoder : Decoder YearMonthDay
decoder =
    Decode.map3 (\y m d -> ( y, m, d ))
        (Decode.index 0 Decode.int)
        (Decode.index 1 Decode.int)
        (Decode.index 2 Decode.int)
