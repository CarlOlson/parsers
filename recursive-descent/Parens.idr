module Parens

{-
  paren_list : paren
             | paren paren_list
  paren : LP RP
        | LP paren_list RP
-}

data Token : Type where
  LP : Token
  RP : Token

mutual
  data Parens : Type where
    MkParens : Paren -> Maybe Parens -> Parens

  data Paren : Type where
    MkParen : Maybe Parens -> Paren

data Result : t -> Type where
  Success : t -> Result t
  Failure : String -> Result t

(>>) : t -> Result $ List t -> Result $ List t
(>>) x (Success xs) = Success $ x :: xs
(>>) _ (Failure err) = Failure err

mutual
  acceptParens : List Token -> Maybe $ (Parens, List Token)
  acceptParens xs with (acceptParen xs)
    | Just (paren, ys) = case acceptParens ys of
                            Just (parens, zs) =>
                              Just (MkParens paren $ Just parens, zs)
                            Nothing =>
                              Just (MkParens paren Nothing, ys)
    | _ = Nothing

  acceptParen : List Token -> Maybe $ (Paren, List Token)
  acceptParen (LP :: RP :: xs) = Just (MkParen Nothing, xs)
  acceptParen (LP :: xs) with (acceptParens xs)
    | Just (parens, RP :: ys) = Just (MkParen $ Just parens, ys)
    | _ = Nothing
  acceptParen _ = Nothing

tokenize : String -> Result $ List Token
tokenize str = tokenizeh $ unpack str
  where
    tokenizeh : List Char -> Result $ List Token
    tokenizeh [] = Success []
    tokenizeh (x :: xs) =
      case x of
        '(' => LP >> (tokenizeh xs)
        ')' => RP >> (tokenizeh xs)
        c => Failure $ "BAD TOKEN " ++ (singleton c)

parse : String -> Maybe Parens
parse str with (tokenize str)
  | Success tokens with (acceptParens tokens)
    | Just (parens, []) = Just parens
    | _ = Nothing
  | _ = Nothing
