-- -*- idris-load-packages: ("contrib") -*-
module Main

import Data.Vect
import Data.Matrix

%default total

{-
  Start -> Expr
  Expr -> Expr Tail
  Expr -> i
  Tail -> Op Expr
  Op -> +
  Op -> *
-}

data HeteroList : List Type -> Type where
  HNil : HeteroList []
  (::) : ty -> HeteroList tys -> HeteroList (ty :: tys)

data Term : Type where
  Start : Term
  Expr : Term
  Tail : Term
  Op : Term

hasStartRule : List Term -> Bool
hasStartRule [] = False
hasStartRule (Start :: _) = True
hasStartRule (_ :: xs) = hasStartRule xs

charMap : List (Char, Term)
charMap = [('i', Expr), ('*', Op), ('+', Op)]

Terms : Type
Terms = List Term

mkMatrix : Vect m Term -> Matrix m m Terms
mkMatrix {m=Z} xs = []
mkMatrix {m=(S k)} xs = map (\x => [x] :: replicate k []) xs

tokenize : Vect n Char -> Maybe $ Vect n Term
tokenize [] = Just []
tokenize (char :: chars) =
  case (lookup char charMap) of
    Just term => map ((::) term) (tokenize chars)
    Nothing => Nothing

parseTokens : Maybe $ Fin n -> Vect n Term -> Bool
parseTokens Nothing _ = False
parseTokens (Just n) tokens = rec n (mkMatrix tokens)
  where
    rec : Fin m -> (Matrix m m Terms) -> Bool
    rec {m=(S n)} index matrix =
      if index == last {n} then
        hasStartRule (indices 0 last matrix)
      else
        ?nominalcase

parse : String -> Bool
parse input =
  let
    tokens = tokenize $ fromList $ cast input
  in case tokens of
    Just tokens => parseTokens (natToFin 0 _) tokens
    Nothing => False
