module Optimizer() where
    import qualified Ast as A
    import qualified Parser as P
    import Data.Maybe (catMaybes, mapMaybe)


    optimize :: A.FrogNode -> Maybe A.FrogNode -- Optimizes the AST for both compilation and execution
    optimize n = Just n

    optimizeConditionals :: A.FrogNode -> Maybe A.FrogNode

    -- Unpack sequences
    optimizeConditionals (A.Sequence seq) = Just (A.Sequence (mapMaybe optimizeConditionals seq))
    -- Stripping empty if statements.
    optimizeConditionals A.IfStatement { A.body=[], A.next=Nothing } = Nothing
    -- Stripping if statements that are always true
    optimizeConditionals A.IfStatement { A.condition=A.BooleanLiteral True, A.body=b } = optimizeConditionals (A.Sequence b)
    -- Stripping if statements that are always false and have an else
    optimizeConditionals A.IfStatement { A.condition=A.BooleanLiteral False, A.next=Just n } = optimizeConditionals n
    -- Stripping if statements that are always false and have no else statement
    optimizeConditionals A.IfStatement { A.condition=A.BooleanLiteral False, A.next=Nothing } = Nothing
    -- Strip while statements that are always false.
    optimizeConditionals A.WhileStatement { A.condition=A.BooleanLiteral false } = Nothing
    -- Unpack blocks
    optimizeConditionals A.Block{A.open=o, A.childeren=ch, A.close=c} = case optimizeConditionals (A.Sequence ch) of
        Just (A.Sequence childs) -> Just A.Block{A.open=o, A.childeren=childs, A.close=c}
        Just child -> Just A.Block{A.open=o, A.childeren=[child], A.close=c}
        Nothing -> Nothing
    -- Unpack function definitions.
    optimizeConditionals A.FunctionDefinition {A.name=n, A.params=p, A.body=b} = case optimizeConditionals (A.Sequence b) of 
        Just (A.Sequence body) -> Just A.FunctionDefinition {A.name=n, A.params=p, A.body=body}
        Just body -> Just A.FunctionDefinition {A.name=n, A.params=p, A.body=[body]}
        Nothing -> Nothing 
    -- Unpack routine definitions.
    optimizeConditionals A.RoutineDefinition {A.name=n, A.params=p, A.body=b} = case optimizeConditionals (A.Sequence b) of 
        Just (A.Sequence body) -> Just A.RoutineDefinition {A.name=n, A.params=p, A.body=body}
        Just body -> Just A.RoutineDefinition {A.name=n, A.params=p, A.body=[body]}
        Nothing -> Nothing

    -- Else, just return
    optimizeConditionals a = Just a

    -- Count references to a given variable.
    countRefs :: String -> A.FrogNode -> Integer
    -- If sequence is input, check if one of the items in the sequence contains the reference
    countRefs target (A.Sequence nodes) = sum (map (countRefs target) nodes)
    countRefs target A.FunctionDefinition{A.name = n, A.params=p, A.body=b} = countRefs target (A.Sequence b) + (if n == target then 1 else 0)
    -- countRefs target (P.FunctionCall{P.name=n, P.}) = if n == target then 1 else 0 + sum(map (\x -> ) )

    -- If variable is found, check if name is the same as the ref target.
    countRefs target (A.Variable name) = if target == name then 1 else 0
    countRefs _ _ = 0


    optimizeEquations ::  A.FrogNode -> A.FrogNode
    optimizeEquations op@A.InfixCall{A.lhs=l, A.target="==", A.rhs=r } 
        | l == r   = A.BooleanLiteral True
        | A.isLiteral l && A.isLiteral r = A.BooleanLiteral False
        | otherwise = op

    optimizeEquations op@A.InfixCall{A.lhs=l, A.target="!=", A.rhs=r } 
        | l /= r   = A.BooleanLiteral True
        | A.isLiteral l && A.isLiteral r = A.BooleanLiteral False
        | otherwise = op
    


    
            
            
    
    
        


    


    -- optimizeValues :: P.FrogNode -> Maybe P.FrogNode
