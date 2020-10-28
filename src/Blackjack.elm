module Blackjack exposing
    ( cardToString
    , deck
    , main
    )

import Random
import Random.List


type CardValue
    = Ace
    | King
    | Queen
    | Jack
    | Num Int


valueToString : CardValue -> String
valueToString val =
    case val of
        Ace ->
            "Ace"

        King ->
            "King"

        Queen ->
            "Queen"

        Jack ->
            "Jack"

        Num num ->
            String.fromInt num


type CardColour
    = Hearts
    | Clubs
    | Diamonds
    | Spades


colourToString : CardColour -> String
colourToString colour =
    case colour of
        Hearts ->
            "♥"

        Clubs ->
            "♣"

        Diamonds ->
            "♦"

        Spades ->
            "♠"


type alias Card =
    { colour : CardColour
    , value : CardValue
    }


cardToString : Card -> String
cardToString { colour, value } =
    "(" ++ valueToString value ++ " " ++ colourToString colour ++ ")"


type alias Deck =
    List Card


type Model
    = NotStarted
    | GameOn Blackjack
    | Woops String


modelToString : Model -> String
modelToString model =
    case model of
        NotStarted ->
            "NotStarted"

        Woops str ->
            "Woops huston we got a problem -> " ++ str

        GameOn blackjack ->
            "GameOn " ++ blackjackToString blackjack


type alias Blackjack =
    { stack : Deck
    , sam : ( Card, Card )
    , dealer : ( Card, Card )
    }


blackjackToString : Blackjack -> String
blackjackToString { stack, sam, dealer } =
    let
        ( sc1, sc2 ) =
            sam

        ( dc1, dc2 ) =
            dealer
    in
    "\n\tDeck -> "
        ++ String.join " ; " (List.map cardToString stack)
        ++ "\n\tSam -> ["
        ++ cardToString sc1
        ++ " ; "
        ++ cardToString sc2
        ++ "]"
        ++ "\n\tDealer -> ["
        ++ cardToString dc1
        ++ " ; "
        ++ cardToString dc2
        ++ "]"


allColours : List CardColour
allColours =
    [ Hearts, Clubs, Diamonds, Spades ]


allValues : List CardValue
allValues =
    [ Ace, King, Queen, Jack ] ++ (List.map Num <| List.range 2 10)


deck : List Card
deck =
    allColours
        |> List.map (\colour -> List.map (Card colour) allValues)
        |> List.concat


type Msg
    = SuffledDeck Deck


init : ( Model, Cmd Msg )
init =
    ( NotStarted
    , Random.generate SuffledDeck <| Random.List.shuffle deck
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "msg" msg

        _ =
            Debug.log "model" <| modelToString model
    in
    case msg of
        SuffledDeck newDeck ->
            case newDeck of
                card1 :: card2 :: card3 :: card4 :: deck_ ->
                    ( GameOn
                        { stack = deck_
                        , sam = ( card1, card3 )
                        , dealer = ( card2, card4 )
                        }
                    , Cmd.none
                    )

                _ ->
                    ( Woops "shuflled deck wasn't quite right ... ", Cmd.none )


main : Platform.Program () Model Msg
main =
    Platform.worker
        { init = \_ -> init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
