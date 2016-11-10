module Main where

--var p = new Person('kevin', true);
--p.firstName === 'kevin';
--p.smoker === true

data Person =
    Person 
        String 
            -- firstName 
            -- starts with capital 5 >
        String -- surname    
        Bool -- 'y' or 'n'
            -- string of dots, digits or hyphens start with d end with #
        String -- phone number 
    deriving(Eq, Show)

data ParseResult a =
    Error String
    | Succeed String a 
    deriving(Eq, Show)

data Parser a = 
    Parser (String -> ParseResult a)


--new Parser(function(s) {
--    return new Error(s);
--});

--Parser (\x -> Error x)

value :: 
  x 
  -> Parser x
value a = 
  Parser(\input -> Succeed input a) 


character ::
    Parser Char 
character = 
    Parser (\input -> case input of 
              [] -> Error "Unexpected EOF"
              h:t -> Succeed t h)

parse :: Parser a -> (String -> ParseResult a)

parse(Parser p) = p 

mapParser ::
  (t1 -> t2)
  -> Parser t1
  -> Parser t2


mapParser f (Parser p) = 
    Parser(\input -> case p input of 
                     Error m -> Error m
                     Succeed r a -> Succeed r (f a))

flatMapParser ::
    (a -> Parser b)
    -> Parser a 
    -> Parser b

flatMapParser f (Parser p) = 
    Parser(\input -> case p input of
                    Error m -> Error m 
                    Succeed r a -> parse (f a) r)

choose :: Parser a -> Parser a -> Parser a
choose (Parser c) (Parser p) = 
    Parser(\input -> case c input of
                Error m -> p input
                Succeed r a -> Succeed r a)

flipMap :: Parser a -> (a -> Parser b) -> Parser b

flipMap = flip flatMapParser

list :: Parser a -> Parser[a]
list p = list1 p `choose` value[]

list1 :: Parser a -> Parser[a]
list1 p = flipMap p (\x ->
    flipMap (list p)(
        \y -> value( x : y)))

--satisfy ::
--    (Char -> Bool)
--    -> Parser Char







