module Example exposing (deckGeneration)

import Blackjack
import Expect
import Set
import Test exposing (Test)


deckGeneration : Test
deckGeneration =
    Test.describe "Deck generation"
        [ Test.test "it has 52 cards" <|
            \_ -> Expect.equal 52 <| List.length Blackjack.deck
        , Test.test "all cards are unique" <|
            \_ ->
                Blackjack.deck
                    |> List.map Blackjack.cardToString
                    |> Set.fromList
                    |> Set.size
                    |> Expect.equal 52
        ]
