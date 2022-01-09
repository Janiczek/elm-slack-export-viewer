module Data.Channel exposing
    ( Channel
    , Name
    , decoder
    )

import Json.Decode as Decode exposing (Decoder)


type alias Name =
    String


type alias Channel =
    { name : Name

    -- topic
    -- purpose
    -- isArchived
    -- id
    -- pins
    -- members
    }


decoder : Decoder Channel
decoder =
    Decode.map Channel
        (Decode.field "name" Decode.string)
