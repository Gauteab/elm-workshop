module Page.Article exposing (Model, Msg, init, update, viewDocument)

import Api
import Article exposing (Article)
import ArticleId exposing (ArticleId)
import Browser exposing (Document)
import Comment exposing (Comment)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import LogElement
import MarkdownString exposing (Markdown)
import ViewElements.Button as Button
import ViewElements.Container as Container
import ViewElements.Header as Header
import ViewElements.Textarea as Textarea



--- MODEL ---


type Model
    = Loading
    | Failure Http.Error
    | Success SuccessModel


type alias SuccessModel =
    { article : Article
    , newCommentState : NewCommentState
    , comments : CommentsState
    }


type NewCommentState
    = WritingComment String
    | SavingComment String
    | ErrorSavingComment String Http.Error


type CommentsState
    = LoadingComments
    | FailureGettingComments Http.Error
    | SucceededGettingComments (List Comment)



--- UPDATE ---


type Msg
    = FetchedArticle (Result Http.Error Article)
    | CommentUpdated String
    | ErrorLogged (Result Http.Error ())
    | PostCommentButtonClicked
    | SavingCommentFinished (Result Http.Error (List Comment))
    | FetchingCommentsFinished (Result Http.Error (List Comment))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchedArticle result ->
            case result of
                Ok article ->
                    ( Success { article = article, newCommentState = WritingComment "", comments = LoadingComments }
                    , Api.getComments FetchingCommentsFinished (Article.id article)
                    )

                Err error ->
                    ( Failure error
                    , error
                        |> LogElement.fromHttpError "Get article"
                        |> Maybe.map (Api.writeToServerLog ErrorLogged)
                        |> Maybe.withDefault Cmd.none
                    )

        CommentUpdated string ->
            case model of
                Success successModel ->
                    case successModel.newCommentState of
                        WritingComment _ ->
                            ( Success { successModel | newCommentState = WritingComment string }, Cmd.none )

                        ErrorSavingComment _ _ ->
                            ( Success { successModel | newCommentState = WritingComment string }, Cmd.none )

                        SavingComment _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        PostCommentButtonClicked ->
            case model of
                Success successModel ->
                    case successModel.newCommentState of
                        WritingComment commentText ->
                            ( Success { successModel | newCommentState = SavingComment commentText }
                            , Api.createCommentOnArticle SavingCommentFinished commentText (Article.id successModel.article)
                            )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ErrorLogged result ->
            ( model, Cmd.none )

        SavingCommentFinished result ->
            case model of
                Success successModel ->
                    case result of
                        Ok value ->
                            ( Success { successModel | newCommentState = WritingComment "", comments = SucceededGettingComments value }, Cmd.none )

                        Err err ->
                            case successModel.newCommentState of
                                SavingComment commentText ->
                                    ( Success { successModel | newCommentState = ErrorSavingComment commentText err }, Cmd.none )

                                _ ->
                                    ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        FetchingCommentsFinished result ->
            case model of
                Success successModel ->
                    case result of
                        Ok res ->
                            ( Success { successModel | comments = SucceededGettingComments res }, Cmd.none )

                        Err err ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )



--- VIEW ---


viewDocument : Model -> Document Msg
viewDocument model =
    { title = "Articles"
    , body = view model
    }


view : Model -> List (Html Msg)
view model =
    [ div [ class "app" ]
        [ Header.header
        , Container.mainContent
            (viewContent model)
        ]
    ]


viewContent : Model -> List (Html Msg)
viewContent model =
    case model of
        Loading ->
            [ text "" ]

        Failure _ ->
            [ text "error" ]

        Success successModel ->
            viewSuccess successModel


viewSuccess : SuccessModel -> List (Html Msg)
viewSuccess successModel =
    [ viewArticle successModel.article
    , viewCommentsSection successModel
    ]


viewArticle : Article -> Html Msg
viewArticle article =
    div [ class "article" ]
        [ h2 []
            [ article
                |> Article.title
                |> MarkdownString.toHtml
            ]
        , article
            |> Article.lead
            |> Maybe.map viewLead
            |> Maybe.withDefault (text "")
        , article
            |> Article.body
            |> MarkdownString.toHtml
        ]


viewLead : Markdown -> Html msg
viewLead markdownContent =
    div [ class "lead" ]
        [ MarkdownString.toHtml markdownContent ]


viewCommentsSection : SuccessModel -> Html Msg
viewCommentsSection successModel =
    case successModel.comments of
        LoadingComments ->
            text "Loading comments ..."

        SucceededGettingComments comments ->
            div [ class "comment-section" ]
                [ h2 [] [ text "X comments" ]
                , div [ class "comments" ]
                    (List.map viewComment comments)
                , viewWriteComment successModel
                ]

        FailureGettingComments httpError ->
            text "Failed to load comments ..."


viewComment : Comment -> Html Msg
viewComment comment =
    div [ class "comment" ]
        [ div [ class "comment-username" ]
            [ text (Comment.username comment) ]
        , div [ class "comment-text" ]
            [ text (Comment.text comment) ]
        ]


viewWriteComment : SuccessModel -> Html Msg
viewWriteComment successModel =
    case successModel.newCommentState of
        WritingComment commentText ->
            div [ class "write-new-comment" ]
                [ commentText
                    |> Textarea.textarea { label = "Add comment", onInput = CommentUpdated }
                    |> Textarea.toHtml
                , Container.buttonRow
                    [ Button.button PostCommentButtonClicked "Post"
                        |> Button.toHtml
                    ]
                ]

        SavingComment commentText ->
            div [ class "write-new-comment" ]
                [ commentText
                    |> Textarea.textarea { label = "Add comment", onInput = CommentUpdated }
                    |> Textarea.toHtml
                , Container.buttonRow
                    [ Button.button PostCommentButtonClicked "Post"
                        |> Button.withSpinner
                        |> Button.toHtml
                    ]
                ]

        ErrorSavingComment commentText err ->
            div [ class "write-new-comment" ]
                [ commentText
                    |> Textarea.textarea { label = "Add comment", onInput = CommentUpdated }
                    |> Textarea.toHtml
                , Container.buttonRow
                    [ Button.button PostCommentButtonClicked "Post"
                        |> Button.toHtml
                    ]
                , text "Error saving"
                ]



--- INIT ---


init : ArticleId -> ( Model, Cmd Msg )
init articleId =
    ( Loading, Api.getArticle FetchedArticle articleId )
