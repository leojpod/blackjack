module Blackjack exposing (..)


type CardValue
    = Ace
    | King
    | Queen
    | Jack
    | Num Int


valueToString : CardValue -> String
valueToString val =
    Debug.todo "aaaa"


type CardColour
    = Hearts
    | Club
    | Diamonds
    | Spades


colourToString : CardColour -> String
colourToString colour =
    Debug.todo "iiii"


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


deck : List Card
deck =
    Debug.todo "woops"
