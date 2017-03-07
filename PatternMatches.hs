{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Nitpick.PatternMatches (patternMatches) where

{- The algorithm used here comes from "Warnings for Pattern Matching"
by Luc Maranget. Check it out for more information!

http://moscova.inria.fr/~maranget/papers/warn/warn.pdf

-}

import Control.Arrow ((***), second)
import qualified Data.Foldable as F
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)

import qualified AST.Expression.Canonical as C
import qualified AST.Helpers as Help
import qualified AST.Literal as L
import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Pattern as Pattern
import qualified AST.Variable as Var
import Elm.Utils ((|>))
import Nitpick.Pattern (Pattern(..), fromCanonicalPattern)
import qualified Optimize.DecisionTree as DT
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Pattern as Error
import qualified Reporting.Region as Region
import qualified Reporting.Result as Result



-- NITPICK PATTERN MATCHES


type Result warning a =
  Result.Result () warning Error.Error a


patternMatches :: Module.Interfaces -> Module.Canonical -> Result wrn DT.VariantDict
patternMatches interfaces (Module.Module name _ info) =
  let
    arityDict =
      toArityDict interfaces name (Module.unions info)

    variantDict =
      Map.map (Map.map _size) arityDict
  in
    pure variantDict
      <* checkExpression arityDict (Module.program info)


checkExpression :: ArityDict -> C.Expr -> Result wrn ()
checkExpression arityDict (A.A region expression) =
  let
    go =
      checkExpression arityDict

    go2 a b =
      go a <* go b
  in
  case expression of
    C.Literal _ ->
        Result.ok ()

    C.Var _ ->
        Result.ok ()

    C.List listExprs ->
        F.traverse_ go listExprs

    C.Binop _ leftExpr rightExpr ->
        go2 leftExpr rightExpr

    C.Lambda pattern@(A.A patRegion _) body ->
        checkPatterns arityDict patRegion Error.Arg [pattern]
        <* go body

    C.App func arg ->
        go2 func arg

    C.If branches finally ->
        F.traverse_ (uncurry go2) branches
        <* go finally

    C.Let defs body ->
        go body
          <* F.traverse_ goDef defs
      where
        goDef (C.Def _ pattern@(A.A patRegion _) expr _) =
            checkPatterns arityDict patRegion Error.LetBound [pattern]
            <* go expr

    C.Case expr branches ->
        go expr
        <* checkPatterns arityDict region Error.Case (map fst branches)
        <* F.traverse_ (go . snd) branches

    C.Ctor _ctor exprs ->
        F.traverse_ go exprs

    C.Access record _field ->
        go record

    C.Update record fields ->
        go record
        <* F.traverse_ (go . snd) fields

    C.Record fields ->
        F.traverse_ (go . snd) fields

    C.Cmd _ ->
        Result.ok ()

    C.Sub _ ->
        Result.ok ()

    C.OutgoingPort _ _ ->
        Result.ok ()

    C.IncomingPort _ _ ->
        Result.ok ()

    C.Program _ _ ->
        error "DANGER - Program AST nodes should not be in Nitpick.PatternMatches."

    C.SaveEnv _ _ ->
        Result.ok ()

    C.GLShader _ _ _ ->
        Result.ok ()



-- CHECK PATTERNS


checkPatterns :: ArityDict -> Region.Region -> Error.Origin -> [Pattern.Canonical] -> Result wrn ()
checkPatterns arityDict region origin patterns =
  do  matrix <- checkRedundant arityDict region [] patterns
      case isExhaustive arityDict matrix 1 of
        [] ->
          Result.ok ()

        badPatterns ->
          Result.throw region (Error.Incomplete origin (map head badPatterns))



-- EXHAUSTIVE PATTERNS

# Does this pattern matrix cover every possible value of this type?
# This implements algorithm I in part two of the paper
isExhaustive :: ArityDict -> [[Pattern]] -> Int -> [[Pattern]]
isExhaustive arityDict matrix n =
  case (matrix, n) of
    ([], _) ->
      [replicate n Anything]

    (_, 0) ->
      []

    (_, _) ->
      let
        ctorSet =
          List.foldl' isCompleteHelp Set.empty matrix

        actual =
          Set.size ctorSet
      in
        if actual == 0 then
          (:) Anything
            <$> isExhaustive arityDict (Maybe.mapMaybe toDefault matrix) (n - 1)

        else
          let
            (ArityInfo expected info) =
              getArityInfo (Set.findMin ctorSet) arityDict
          in
            if actual < expected then
              (:)
                <$> Maybe.mapMaybe (isMissing ctorSet) info
                <*> isExhaustive arityDict (Maybe.mapMaybe toDefault matrix) (n - 1)

            else
              let
                isExhaustiveHelp (name, arity) =
                  recoverCtor name arity <$>
                  isExhaustive
                    arityDict
                    (Maybe.mapMaybe (specialize name arity) matrix)
                    (arity + n - 1)
              in
                concatMap isExhaustiveHelp info


isMissing :: Set.Set Var.Canonical -> (Var.Canonical, Int) -> Maybe Pattern
isMissing ctorSet (name, arity) =
  if Set.member name ctorSet then
    Nothing

  else
    Just (Ctor name (replicate arity Anything))


recoverCtor :: Var.Canonical -> Int -> [Pattern] -> [Pattern]
recoverCtor name arity patterns =
  let
    (args, rest) =
      splitAt arity patterns
  in
    Ctor name args : rest



-- REDUNDANT PATTERNS


checkRedundant
  :: ArityDict
  -> Region.Region
  -> [[Pattern]]
  -> [Pattern.Canonical]
  -> Result wrn [[Pattern]]
checkRedundant arityDict region checked unchecked =
  case unchecked of
    [] ->
      Result.ok checked

    nextPattern@(A.A patRegion _) : rest ->
      let
        next =
          # Turn an AST into a Pattern (Ctor _ _, Anything, or Literal _)
          [fromCanonicalPattern nextPattern]
      in
        # This pattern is useful if isUseful(previous_patterns, this_pattern) == true
        if isUseful arityDict checked next then
          # Add this pattern to the useful patterns list, and recurse with the remaining patterns
          checkRedundant arityDict region (next : checked) rest
        else
          Result.throw region (Error.Redundant patRegion (length checked + 1))

# ArityDict is mapping from constructor to arity
# isUseful is U (usefulness) equation in paper

# matrix is the matrix of patterns already determined to be useful
# vector is the pattern to be added to the useful matrix
isUseful :: ArityDict -> [[Pattern]] -> [Pattern] -> Bool
isUseful arityDict matrix vector =
  case (matrix, vector) of
    # Base case of empty matrix
    ([], _) ->
      True

    # Base case of no columns, but some rows
    (_, []) ->
      False

    # Grab the first pattern in the pattern vector
    (_, firstPattern : patterns) ->
      case firstPattern of
        # Case 1: constructed pattern (generic, tuple, record)
        Ctor name args ->
          let
            # `specialize` is the S equation, taking a constructor, arity, and a row in the matrix
            # The new matrix is built by calling `specialize` on each row
            newMatrix =
              Maybe.mapMaybe (specialize name (length args)) matrix
          in
            # Recurse with the (specialized matrix) and (constructor args ++ remaining patterns)
            isUseful arityDict newMatrix (args ++ patterns)

        # Case 2: wildcard
        Anything ->
          # Check if 
          case isComplete arityDict matrix of
            # Not complete
            Nothing ->
              # Recurse with the default matrix and the remaining patterns vector
              isUseful arityDict (Maybe.mapMaybe toDefault matrix) patterns

            # Complete
            Just arityInfo ->
              let
                isUsefulHelp (ctor, arity) =
                  isUseful
                    arityDict
                    (Maybe.mapMaybe (specialize ctor arity) matrix)
                    (replicate arity Anything ++ patterns)
              in
                # Recurse isUseful with (matrix specialized with each constructor) and (arity*wildcards ++ remaining patterns)
                # True is any of the constructors in a recurse is true
                # ASIDE: this, in effect, replaces something like `_` with `(_, _, _)` when matching against a 3-tuple
                any isUsefulHelp arityInfo

        # Another case: literal (I'm guessing or-pattern case isn't implemented)
        Literal literal ->
          let
            newMatrix =
              Maybe.mapMaybe (specializeLiteral literal) matrix
          in
            isUseful arityDict newMatrix patterns



-- SPECIALIZE MATRICES

# Specialize a single row in the matrix
# Given a constructor name, number of args, and a row of patterns in the matrix
# Returns either nothing or a specialized row
# Basically returns the same row with the first element replaced by arity-n patterns
specialize :: Var.Canonical -> Int -> [Pattern] -> Maybe [Pattern]
specialize ctorName arity row =
  case row of
    [] ->
      error "Compiler error! Empty matrices should not get specialized."

    firstPattern : patterns ->
      case firstPattern of
        # If the first element is the same as the constructor we're specialize against
        # Then replace it with args ++ rest of patterns
        # Else don't add the row back
        Ctor name args ->
          if name == ctorName then Just (args ++ patterns) else Nothing

        # If the first element is a wildcard, then replace the first element with arity-n wildcards
        Anything ->
          Just (replicate arity Anything ++ patterns)

        Literal _ ->
          error $
            "Compiler bug! After type checking, constructors and literals\
            \ should never align in pattern match exhaustiveness checks."


# Specialize a row in the matrix with a literal
specializeLiteral :: L.Literal -> [Pattern] -> Maybe [Pattern]
specializeLiteral literal row =
  case row of
    [] ->
      error "Compiler error! Empty matrices should not get specialized."

    firstPattern : patterns ->
      case firstPattern of
        # If the first element in this row is the same as the literal we're specializing against, then remove just the element and return the remaining patterns
        # Else don't add the row back
        Literal lit ->
          if lit == literal then Just patterns else Nothing

        # If the first element is a wildcard, then just add the remaining patterns
        Anything ->
          Just patterns

        Ctor _ _ ->
          error $
            "Compiler bug! After type checking, constructors and literals\
            \ should never align in pattern match exhaustiveness checks."


# Builds a row of the default matrix
# Basically, only rows that start with a wildcard are returned
# The wildcard is popped from the front and the rest of the patterns are returned
toDefault :: [Pattern] -> Maybe [Pattern]
toDefault row =
  case row of
    [] ->
      Nothing

    Ctor _ _ : _ ->
      Nothing

    Anything : patterns ->
      Just patterns

    Literal _ : _ ->
      Nothing



-- ALL CONSTRUCTORS ARE PRESENT?


isComplete :: ArityDict -> [[Pattern]] -> Maybe [(Var.Canonical, Int)]
isComplete arityDict matrix =
  let
    # The set of all constructors for this type in the first column of matrix
    ctorSet =
      List.foldl' isCompleteHelp Set.empty matrix

    actual =
      Set.size ctorSet
  in
    if actual == 0 then
      # Not complete
      Nothing

    else
      let
        # Fetch the total number of constructors for this type
        (ArityInfo expected info) =
          getArityInfo (Set.findMin ctorSet) arityDict
      in
        # Return info (meaning it's complete) if the first column of the matrix contains a complete list of this type's constructors
        # Else nothing (meaning not complete)
        if expected == actual then Just info else Nothing


isCompleteHelp :: Set.Set Var.Canonical -> [Pattern] -> Set.Set Var.Canonical
isCompleteHelp ctors row =
  case row of
    Ctor name _ : _ ->
      Set.insert name ctors

    _ ->
      ctors



-- ARITY DICT


type ArityDict =
  Map.Map Var.Home (Map.Map Text ArityInfo)


data ArityInfo =
  ArityInfo
    { _size :: !Int
    , _info :: [(Var.Canonical, Int)]
    }


toArityDict :: Module.Interfaces -> ModuleName.Canonical -> Module.Unions -> ArityDict
toArityDict interfaces localName localUnions =
  interfaces
    |> Map.mapWithKey (\name iface -> toArityEntry name (Module.iUnions iface))
    |> Map.mapKeysMonotonic Var.Module
    |> Map.insert (Var.Module localName) (toArityEntry localName localUnions)
    |> Map.union builtinDict


builtinDict :: ArityDict
builtinDict =
  let
    listInfo =
      ArityInfo 2 [ (Var.builtin "::", 2), (Var.builtin "[]", 0) ]

    boolInfo =
      ArityInfo 2 [ (Var.builtin "True", 0), (Var.builtin "False", 0) ]
  in
    Map.singleton Var.BuiltIn $ Map.fromList $
      [ ("::", listInfo)
      , ("[]", listInfo)
      , ("True", boolInfo)
      , ("False", boolInfo)
      ]
      ++ map makeTupleInfo [0..8]


makeTupleInfo :: Int -> ( Text, ArityInfo )
makeTupleInfo n =
  let
    name =
      Text.pack ("_Tuple" ++ show n)
  in
    ( name, ArityInfo 1 [ (Var.builtin name, n) ] )


toArityEntry :: ModuleName.Canonical -> Module.Unions -> Map.Map Text ArityInfo
toArityEntry name unions =
  Map.fromList (concatMap (toArityEntryHelp name) (Map.elems unions))


toArityEntryHelp :: ModuleName.Canonical -> Module.UnionInfo Text -> [(Text, ArityInfo)]
toArityEntryHelp name (_tvars, ctors) =
  let
    arityInfo =
      ArityInfo (length ctors) (map (Var.fromModule name *** length) ctors)
  in
    map (second (\_ -> arityInfo)) ctors


getArityInfo :: Var.Canonical -> ArityDict -> ArityInfo
getArityInfo var@(Var.Canonical home name) arityDict =
  case Map.lookup name =<< Map.lookup home arityDict of
    Just arityInfo ->
      arityInfo

    Nothing ->
      if Help.isTuple name then
        ArityInfo 1 [ (var, read (drop 6 (Text.unpack name))) ]
      else
        error
          "Since the Nitpick phase happens after canonicalization and type\
          \ inference, it is impossible that a pattern in a case cannot be\
          \ found."
