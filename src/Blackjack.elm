module Blackjack exposing
    ( Blackjack
    , Deck
    , blackjackToString
    , cardToString
    , deck
    , main
    , scoreHand
    )

import List.Extra
import Random
import Random.List
import Task


type CardValue
    = Ace
    | King
    | Queen
    | Jack
    | Num Int


valueToPoint : CardValue -> Int
valueToPoint value =
    case value of
        Ace ->
            11

        King ->
            10

        Queen ->
            10

        Jack ->
            10

        Num i ->
            i


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


cardToPoint : Card -> Int
cardToPoint { value } =
    valueToPoint value


type alias Deck =
    List Card


type alias Hand =
    List Card


handToString : Hand -> String
handToString =
    List.map cardToString >> String.join ", "


scoreHand : Hand -> Int
scoreHand =
    List.map cardToPoint >> List.sum


type Model
    = NotStarted
    | GameOn Blackjack
    | Woops String


map : Model -> (Blackjack -> Blackjack) -> Model
map model fct =
    case model of
        NotStarted ->
            NotStarted

        Woops str ->
            Woops str

        GameOn blackjack ->
            GameOn <| fct blackjack


play : Model -> String -> (Blackjack -> ( Model, Msg )) -> ( Model, Cmd Msg )
play model errText fct =
    case model of
        NotStarted ->
            ( Woops errText, Cmd.none )

        Woops str ->
            ( Woops str, Cmd.none )

        GameOn blackjack ->
            fct blackjack |> Tuple.mapSecond runMsg


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
    , sam : Hand
    , dealer : Hand
    }


blackjackToString : Blackjack -> String
blackjackToString { stack, sam, dealer } =
    "\n\tDeck -> "
        ++ String.join " ; " (List.map cardToString stack)
        ++ "\n\tSam -> ["
        ++ handToString sam
        ++ "]"
        ++ "\n\tDealer -> ["
        ++ handToString dealer
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
    | Start
    | SamsTurn
    | DealersTurn
    | SamWon
    | DealerWon
    | Push
    | Failure


msgToString : Msg -> String
msgToString msg =
    case msg of
        SuffledDeck suffledDeck ->
            "SuffledDeck [" ++ String.join ", " (List.map cardToString suffledDeck) ++ "]"

        _ ->
            Debug.toString msg


init : ( Model, Cmd Msg )
init =
    ( NotStarted
    , Random.generate SuffledDeck <| Random.List.shuffle deck
    )


runMsg : Msg -> Cmd Msg
runMsg msg =
    Task.succeed () |> Task.perform (\_ -> msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "msg" <| msgToString msg

        _ =
            Debug.log "model" <| modelToString model
    in
    case msg of
        SuffledDeck newDeck ->
            case newDeck of
                card1 :: card2 :: card3 :: card4 :: deck_ ->
                    ( GameOn
                        { stack = deck_
                        , sam = [ card1, card3 ]
                        , dealer = [ card2, card4 ]
                        }
                    , runMsg Start
                    )

                _ ->
                    ( Woops "shuflled deck wasn't quite right ... ", Cmd.none )

        Start ->
            play model
                "cannot start a game without dealing it first"
                (\({ sam, dealer } as blackjack) ->
                    let
                        samScore =
                            scoreHand sam

                        dealerScore =
                            scoreHand dealer
                    in
                    case ( samScore == 21, dealerScore == 21 ) of
                        ( True, True ) ->
                            ( GameOn blackjack, Push )

                        ( True, _ ) ->
                            ( GameOn blackjack, SamWon )

                        ( _, True ) ->
                            ( GameOn blackjack, DealerWon )

                        ( False, False ) ->
                            ( GameOn blackjack, SamsTurn )
                )

        SamsTurn ->
            play model
                "cannot start a game without dealing it first"
                (\({ stack, sam } as blackjack) ->
                    let
                        samScore =
                            scoreHand sam
                    in
                    case ( List.Extra.uncons stack, samScore < 17, samScore <= 21 ) of
                        ( _, False, False ) ->
                            -- sam lost
                            ( GameOn blackjack, DealerWon )

                        ( _, False, True ) ->
                            -- sam stop taking up cards
                            ( GameOn blackjack, DealersTurn )

                        ( Just ( card, stack_ ), True, _ ) ->
                            -- sam picks a card
                            ( GameOn
                                { blackjack
                                    | stack = stack_
                                    , sam = card :: sam
                                }
                            , SamsTurn
                            )

                        ( Nothing, _, _ ) ->
                            ( Woops "no winner and no cards left, how is that possible? ", Failure )
                )

        DealersTurn ->
            play model
                "cannot start a game without dealing it first"
                (\({ stack, sam, dealer } as blackjack) ->
                    let
                        samScore =
                            scoreHand sam

                        dealerScore =
                            scoreHand dealer
                    in
                    case ( List.Extra.uncons stack, dealerScore < samScore, dealerScore <= 21 ) of
                        ( _, False, False ) ->
                            -- dealer over 21
                            ( GameOn blackjack, SamWon )

                        ( _, False, True ) ->
                            ( GameOn blackjack, DealerWon )

                        ( Just ( card, stack_ ), True, _ ) ->
                            ( GameOn
                                { blackjack
                                    | stack = stack_
                                    , dealer = card :: dealer
                                }
                            , DealersTurn
                            )

                        ( Nothing, _, _ ) ->
                            ( Woops "no winner and no cards left, how is that possible?", Failure )
                )

        SamWon ->
            ( model, Cmd.none )

        DealerWon ->
            ( model, Cmd.none )

        Push ->
            ( model, Cmd.none )

        Failure ->
            ( model, Cmd.none )


main : Platform.Program () Model Msg
main =
    Platform.worker
        { init = \_ -> init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
