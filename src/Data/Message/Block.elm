module Data.Message.Block exposing
    ( Accessory(..)
    , Block(..)
    , RichTextElement(..)
    , blocksDecoder
    , textToBlocks
    )

import Json.Decode as Decode exposing (Decoder)


type Block
    = RichText (List RichTextElement)
      -- All of the below are needed for one freak message :facepalm:
    | Header String
    | Image ImageBlockData
    | Divider
    | Section SectionBlockData
    | Context (List RichTextElement)


type RichTextElement
    = Text String
    | Link LinkData
    | InlineCode String
    | Emoji String
    | Quote (List RichTextElement)
    | Preformatted (List RichTextElement)
    | User UserData
    | Channel ChannelData
    | Color String
      -- Only used in one freak message:
    | Markdown String


type alias UserData =
    { userId : String }


type alias ChannelData =
    { channelId : String }


type alias LinkData =
    { url : String
    , text : String
    }


type alias SectionBlockData =
    { content : List RichTextElement
    , accessory : Maybe Accessory
    }


type Accessory
    = ButtonAccessory AccessoryButton
    | ImageAccessory AccessoryImage


type alias AccessoryButton =
    { text : String
    , url : String
    }


type alias AccessoryImage =
    { url : String
    , alt : String
    }


type alias ImageBlockData =
    { url : String
    , alt : String
    }


blocksDecoder : Decoder (List Block)
blocksDecoder =
    {- Either we have "blocks" field with rich text blocks or we have just
       plaintext "text" which we can convert to [RichText [Text string]]
    -}
    Decode.oneOf
        [ Decode.field "blocks" (Decode.list blockDecoder)
        , Decode.field "text" Decode.string
            |> Decode.map textToBlocks
        ]


blockDecoder : Decoder Block
blockDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "rich_text" ->
                        richTextBlockDecoder

                    "header" ->
                        headerBlockDecoder

                    "image" ->
                        imageBlockDecoder

                    "section" ->
                        sectionBlockDecoder

                    "divider" ->
                        dividerBlockDecoder

                    "context" ->
                        contextBlockDecoder

                    _ ->
                        let
                            _ =
                                Debug.log "unknown block" type_
                        in
                        Decode.fail <| "Unknown block type: '" ++ type_ ++ "'"
            )


contextBlockDecoder : Decoder Block
contextBlockDecoder =
    Decode.field "elements" (Decode.list richTextElementDecoder)
        |> Decode.map (List.concat >> Context)


dividerBlockDecoder : Decoder Block
dividerBlockDecoder =
    Decode.succeed Divider


headerBlockDecoder : Decoder Block
headerBlockDecoder =
    Decode.at [ "text", "text" ] Decode.string
        |> Decode.map Header


sectionBlockDecoder : Decoder Block
sectionBlockDecoder =
    Decode.map2 SectionBlockData
        (Decode.field "text" richTextElementDecoder)
        (Decode.maybe (Decode.field "accessory" accessoryDecoder))
        |> Decode.map Section


accessoryDecoder : Decoder Accessory
accessoryDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "image" ->
                        imageAccessoryDecoder

                    "button" ->
                        buttonAccessoryDecoder

                    _ ->
                        let
                            _ =
                                Debug.log "unknown accessory" type_
                        in
                        Decode.fail <| "Unknown accessory type: '" ++ type_ ++ "'"
            )


buttonAccessoryDecoder : Decoder Accessory
buttonAccessoryDecoder =
    Decode.map2 AccessoryButton
        (Decode.at [ "text", "text" ] Decode.string)
        (Decode.field "url" Decode.string)
        |> Decode.map ButtonAccessory


imageBlockDecoder : Decoder Block
imageBlockDecoder =
    Decode.map2 ImageBlockData
        (Decode.field "image_url" Decode.string)
        (Decode.field "alt_text" Decode.string)
        |> Decode.map Image


imageAccessoryDecoder : Decoder Accessory
imageAccessoryDecoder =
    Decode.map2 AccessoryImage
        (Decode.field "image_url" Decode.string)
        (Decode.field "alt_text" Decode.string)
        |> Decode.map ImageAccessory


richTextBlockDecoder : Decoder Block
richTextBlockDecoder =
    Decode.field "elements" (Decode.list richTextElementDecoder)
        |> Decode.map (List.concat >> RichText)


richTextElementDecoder : Decoder (List RichTextElement)
richTextElementDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "rich_text_section" ->
                        Decode.field "elements" (Decode.list richTextElementDecoder)
                            |> Decode.map List.concat

                    "rich_text_preformatted" ->
                        Decode.field "elements" (Decode.list richTextElementDecoder)
                            |> Decode.map (List.concat >> Preformatted >> List.singleton)

                    "text" ->
                        Decode.map2
                            (\text isInlineCode ->
                                if isInlineCode then
                                    [ InlineCode text ]

                                else
                                    [ Text text ]
                            )
                            (Decode.field "text" Decode.string)
                            (Decode.maybe (Decode.at [ "style", "code" ] Decode.bool)
                                |> Decode.map (Maybe.withDefault False)
                            )

                    "mrkdwn" ->
                        Decode.field "text" Decode.string
                            |> Decode.map (Markdown >> List.singleton)

                    "link" ->
                        Decode.map2 (\url text -> LinkData url (Maybe.withDefault url text))
                            (Decode.field "url" Decode.string)
                            (Decode.maybe (Decode.field "text" Decode.string))
                            |> Decode.map (Link >> List.singleton)

                    "emoji" ->
                        Decode.field "name" Decode.string
                            |> Decode.map (Emoji >> List.singleton)

                    "rich_text_quote" ->
                        Decode.field "elements" (Decode.list richTextElementDecoder)
                            |> Decode.map (List.concat >> Quote >> List.singleton)

                    -- only used in one freak message
                    "plain_text" ->
                        Decode.at [ "text", "text" ] Decode.string
                            |> Decode.map (Text >> List.singleton)

                    "user" ->
                        Decode.field "user_id" Decode.string
                            |> Decode.map (UserData >> User >> List.singleton)

                    "channel" ->
                        Decode.field "channel_id" Decode.string
                            |> Decode.map (ChannelData >> Channel >> List.singleton)

                    "color" ->
                        Decode.field "value" Decode.string
                            |> Decode.map (Color >> List.singleton)

                    "rich_text_list" ->
                        Decode.field "elements" (Decode.list richTextElementDecoder)
                            |> Decode.map List.concat

                    _ ->
                        let
                            _ =
                                Debug.log "unknown element" type_
                        in
                        Decode.fail <| "Unknown rich text element type: '" ++ type_ ++ "'"
            )


textToBlocks : String -> List Block
textToBlocks text =
    [ RichText [ Text text ] ]
