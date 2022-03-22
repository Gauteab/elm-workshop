module Comment exposing (Comment, decoder, text, username)

import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)


type Comment
    = Comment CommentInfo


type alias CommentInfo =
    { text : String
    , username : String
    }



--- CONTENTS ---


text : Comment -> String
text (Comment info) =
    info.text


username : Comment -> String
username (Comment info) =
    info.username



--- DECODING ---


commentInfoDecoder : Decoder CommentInfo
commentInfoDecoder =
    Json.Decode.succeed CommentInfo
        |> required "text" Json.Decode.string
        |> required "username" Json.Decode.string


decoder : Decoder Comment
decoder =
    Json.Decode.map Comment commentInfoDecoder
