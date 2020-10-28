module Blackjack2 exposing (main)

import Blackjack exposing (Blackjack, Deck, blackjackToString, deck, scoreHand)
import List.Extra
import Random
import Random.List


type Game
    = Shuffling
    | Start Blackjack
    | SamsTurn Blackjack
    | DealersTurn Blackjack
    | SamWon Blackjack
    | DealerWon Blackjack
    | Push Blackjack
    | Err String


gameToString : Game -> String
gameToString game =
    case game of
        Shuffling ->
            "Shuffling"

        Start bj ->
            "Start \t" ++ blackjackToString bj

        SamsTurn bj ->
            "SamsTurn \t" ++ blackjackToString bj

        DealersTurn bj ->
            "DealersTurn \t" ++ blackjackToString bj

        SamWon bj ->
            "SamWon \t" ++ blackjackToString bj

        DealerWon bj ->
            "DealerWon \t" ++ blackjackToString bj

        Push bj ->
            "Push \t" ++ blackjackToString bj

        Err err ->
            "Err \t" ++ err


run : Deck -> ( Game, List Game )
run suffledDeck =
    dealGame suffledDeck
        |> tick []
        |> List.Extra.uncons
        |> Maybe.withDefault ( Err "woops", [] )


dealGame : Deck -> Game
dealGame suffledDeck =
    case suffledDeck of
        card1 :: card2 :: card3 :: card4 :: stack ->
            Start
                { stack = stack
                , sam = [ card1, card3 ]
                , dealer = [ card2, card4 ]
                }

        _ ->
            Err "not enough card to deal the game"


tick : List Game -> Game -> List Game
tick steps game =
    case game of
        Shuffling ->
            game :: steps

        Start bj ->
            initialCheck bj |> (\game_ -> tick (game :: steps) game_)

        SamsTurn bj ->
            samPlay bj |> (\game_ -> tick (game :: steps) game_)

        DealersTurn bj ->
            dealerPlay bj |> (\game_ -> tick (game :: steps) game_)

        SamWon _ ->
            game :: steps

        DealerWon _ ->
            game :: steps

        Push _ ->
            game :: steps

        Err _ ->
            game :: steps


initialCheck : Blackjack -> Game
initialCheck ({ sam, dealer } as blackjack) =
    let
        samScore =
            scoreHand sam

        dealerScore =
            scoreHand dealer
    in
    case ( samScore == 21, dealerScore == 21 ) of
        ( True, True ) ->
            Push blackjack

        ( True, False ) ->
            SamWon blackjack

        ( False, True ) ->
            DealerWon blackjack

        ( False, False ) ->
            SamsTurn blackjack


samPlay : Blackjack -> Game
samPlay ({ stack, sam } as blackjack) =
    let
        samScore =
            scoreHand sam
    in
    case ( List.Extra.uncons stack, samScore < 17, samScore <= 21 ) of
        ( _, _, False ) ->
            DealerWon blackjack

        ( _, False, _ ) ->
            DealersTurn blackjack

        ( Just ( card, stack_ ), _, _ ) ->
            SamsTurn
                { blackjack
                    | stack = stack_
                    , sam = card :: sam
                }

        ( Nothing, _, _ ) ->
            Err "no winner yet but no more card"


dealerPlay : Blackjack -> Game
dealerPlay ({ stack, sam, dealer } as blackjack) =
    let
        samScore =
            scoreHand sam

        dealerScore =
            scoreHand dealer
    in
    case ( List.Extra.uncons stack, dealerScore < samScore, dealerScore <= 21 ) of
        ( _, _, False ) ->
            SamWon blackjack

        ( _, False, _ ) ->
            if dealerScore == samScore then
                Push blackjack

            else
                DealerWon blackjack

        ( Just ( card, stack_ ), _, _ ) ->
            DealersTurn
                { blackjack
                    | stack = stack_
                    , dealer = card :: dealer
                }

        ( Nothing, _, _ ) ->
            Err "no winner yet but no more card"


type Msg
    = SuffledDeck Deck


update : Msg -> Game -> ( Game, Cmd Msg )
update msg _ =
    case msg of
        SuffledDeck suffledDeck ->
            ( run suffledDeck
                |> (\( result, steps ) ->
                        let
                            _ =
                                Debug.log "steps:" <| String.join "\n" <| List.map gameToString <| List.reverse steps

                            _ =
                                Debug.log "result -> " <| gameToString result
                        in
                        result
                   )
            , Cmd.none
            )


main : Platform.Program () Game Msg
main =
    Platform.worker
        { init = \_ -> ( Shuffling, Random.generate SuffledDeck <| Random.List.shuffle deck )
        , update = update
        , subscriptions = \_ -> Sub.none
        }
