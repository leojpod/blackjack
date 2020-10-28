module Blackjack exposing (..)


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


type alias Blackjack =
    { deck : Deck
    , sam : ( Card, Card )
    , dealer : ( Card, Card )
    }


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
