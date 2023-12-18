% End Clause for CheckSingularParity.
checkSingularParity([],Tents, [], Tents):-!.

% Passing Clause for CheckSingularParity.
%
% (Checks if a tree has only one tent associated with it).
checkSingularParity([Tree|Trees], Tents, RemainingTrees, RemainingTents):-
    % Remove neighbours from Tents
    vizinhanca(Tree, TreeNeigh),
    removeFromList(Tents,TreeNeigh,LeftOverTents),
    % Verify if only one Tent was removed with this (remove tent-tree pair).
    length(Tents, TentsAmount),
    length(LeftOverTents, LeftOverAmount),
    TentsAmount-1 =:= LeftOverAmount,
    checkSingularParity(Trees, LeftOverTents, RemainingTrees, RemainingTents).

% Failing Clause for CheckSingularParity.
checkSingularParity([Tree|Trees], Tents, RemainingTrees, RemainingTents):-
    checkSingularParity(Trees, Tents, RemainingTreesSol, RemainingTents),
    append(RemainingTreesSol,[Tree],RemainingTrees).

% Controller function for CheckSingularParity, so it stops once it stagnates.
checkSingularParityController(Trees, Tents, RemainingTrees, RemainingTents):-
    % Call the function we are controlling the outputs of.
    checkSingularParity(Trees, Tents, LeftOverTrees, LeftOverTents),
    % Check if the outputs are the same as inputs
    sort(LeftOverTrees, SortedLeftOverTrees),
    sort(LeftOverTents, SortedLeftOverTents),
    sort(Trees, SortedTrees),
    sort(Tents, SortedTents),
    % Exit if it stagnates
    \+ (SortedTrees == SortedLeftOverTrees, SortedTents == SortedLeftOverTents),
    checkSingularParityController(LeftOverTrees, LeftOverTents, RemainingTrees, RemainingTents).

% End Clause for the controller, so it saves the values in the variables.
checkSingularParityController(Trees, Tents, RemainingTrees, RemainingTents):-
    checkSingularParity(Trees, Tents, LeftOverTrees, LeftOverTents),
    % Check if the outputs are the same as inputs
    sort(LeftOverTrees, SortedLeftOverTrees),
    sort(LeftOverTents, SortedLeftOverTents),
    sort(Trees, SortedTrees),
    sort(Tents, SortedTents),
    SortedTrees == SortedLeftOverTrees,
    SortedTents == SortedLeftOverTents,
    % Save the Leftovers
    RemainingTrees = SortedLeftOverTrees,
    RemainingTents = SortedLeftOverTents,
    % Prevent Backtracking.
    !.


% Controller of CheckCircularChain
checkCircularChainController(TreesList,TentsList):-
    % Remove the tree-tent pairs from the list that are unique.
    checkSingularParityController(TreesList,TentsList,RemainingTrees,RemainingTents),
    % Check if the remaining lists are empty and succeed
    ((RemainingTrees = [], RemainingTents = []) ;
    % Else Check if the remaining coordinates are a circular chain
    % Remove the first tree and a tent from its neighborhood
    % And try again till stagnate
    (RemainingTrees = [FirstTree|LeftOverTrees],
    vizinhanca((FirstTree), TreeNeigh),
    intersection(TreeNeigh, RemainingTents, [FirstTent|_]),
    removeFromList(RemainingTents, [FirstTent], LeftOverTents),
    checkSingularParityController(LeftOverTrees, LeftOverTents, FinalTrees, FinalTents),
    % Exit if it stagnates
    \+ (LeftOverTrees == FinalTrees, LeftOverTents == FinalTents),
    checkCircularChainController(FinalTrees,FinalTents))).

valida(TreesList, TentsList):-
    % Check the size of lists to make sure theres a 1-1 relationship.
    length(TreesList,TreesAmount),
    length(TentsList, TentsAmount),
    TreesAmount =:= TentsAmount,
    % Check if there are any intersections between trees and tents coords.
    intersection(TreesList, TentsList, Intersections),
    length(Intersections,IntersectionsAmount),
    IntersectionsAmount =:= 0,
    % Remove the tree-tent pairs from the list that are unique,
    % And Check for circular Chains.
    checkCircularChainController(TreesList,TentsList).  