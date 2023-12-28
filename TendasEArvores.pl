% Author: Diogo Santos
% Num:    110262
% Date:   28/12/2023
:- use_module(library(clpfd)). % para poder usar transpose/2
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % ver listas completas
:- ["puzzlesAcampar.pl"]. % Ficheiro dado. No Mooshak tera mais puzzles.
% Atencao: nao deves copiar nunca os puzzles para o teu ficheiro de codigo
% Segue-se o codigo

/* Aux Functions */

/* Get information functions */

% Get the Amount of an element per line.
getCountsInLine(Board, LineCounts, Occupation):-
    findall(
        LineCount,
        (
            nth1(_, Board, ExtLine),
            % Add [] to make the list inside of the list 
            % so it works with todasCelulas/3.
            todasCelulas([ExtLine], Cells, Occupation),
            length(Cells, LineCount)
        ),
        LineCounts
    ).

% Get the cell of the board.
getCell(Board, (Line, Col), Cell):-
    nth1(Line, Board, ExtLine),
    nth1(Col, ExtLine, Cell).

% Get the cells of the board.
getCells(Board, Coords, Cells):-
    findall(
        Cell,
        (
            member(Coord, Coords),
            getCell(Board, Coord, Cell)
        ),
        Cells
    ).

% Helper predicate to count the number of variables in a nested list.
count_vars(List, Count) :-
    flatten(List, FlatList),
    include(var, FlatList, Vars),
    length(Vars, Count).


/* Check Bounds Functions */

% Verify if a pair of coordinates is within bounds.
withinBounds(MinLine, MaxLine, MinCol, MaxCol, (Line, Col)) :-
    Line >= MinLine, Line =< MaxLine, 
    Col >= MinCol, Col =< MaxCol.

% Verify if cell is inside board's bounds.
withinBoard(Board, Coord):-
    % Make this work for any board size.
    length(Board, LineSize),
    nth1(1, Board, FirstRow),
    length(FirstRow, ColSize),
    withinBounds(1, LineSize, 1, ColSize, Coord).

/* List operations */

% Aux function to sum lists together for each value.
sum_lists(L1, L2, Values):-
    findall(
        Value,
        (
            % Get the values where the index is the same.
            nth1(Index, L1, Value1),
            nth1(Index, L2, Value2),
            % Add them together.
            Value is Value1 + Value2
        ), 
        Values
    ).


% Remove List2 elements from List1.
removeFromList(List1, List2, FilteredList):-
    findall(
        FilteredItem,
        (
            % Find members of List1 that don't exist in List2.
            member(FilteredItem, List1),
            \+ member(FilteredItem, List2)
        ),
        FilteredList).

% Pair two lists together.
pairLists(L1, L2, Pairs):-
    findall(
        Pair,
        (
            % Find Elements of each list where index is the same.
            nth1(I, L1, L1Element),
            nth1(I, L2, L2Element),
            % Format them as a pair.
            Pair = (L1Element, L2Element) 
        ),
        Pairs    
    ).

% Intersect the values between two lists.
intersection(L1, L2, Intersections):-
    findall(
        Intersection,
        (
            % Find the elements that exist in both L1 And L2.
            member(Intersection, L1),
            member(Intersection, L2)
        ),
        Duplicates
    ),
    % Make sure the intersections are sorted.
    sort(Duplicates, Intersections).


/* Filtering Functions */

% Get the Coordinates of the cells
% between two coordinates (Horizontally or Vertically)
%
% Copilot taught me about the min and max Functions.
filterCoords(Board, (Line1, Col1), (Line2, Col2), FilteredCoords):-
    % Find Max and Mins so it works with either.
    MinLine is min(Line1, Line2),
    MaxLine is max(Line1, Line2),
    MinCol is min(Col1, Col2),
    MaxCol is max(Col1, Col2),
    % Filter the cells we want.
    todasCelulas(Board, CellCoords),
    include(
        withinBounds(MinLine, MaxLine, MinCol, MaxCol),
        CellCoords,
        FilteredCoords
    ).

% Aux function to find all Coords of full lines/Cols (No tents to place).
findSameWithCoords(LineCounts, CheckLineCounts, LineLength, LineCoords):-
    findall(
        FullLineCoord, 
        (
            nth1(Index, LineCounts, LineCount),
            nth1(Index, CheckLineCounts, LineCount),
            % Create pairs of coordinates (start,end).
            FullLineCoord = [(Index, 1), (Index, LineLength)]
        ),
        LineCoords
    ).

% Filter a list of coordinates with valid coordinates.
filterValidCoords(Board, Coords, SortedValidCoords):-
    findall(
        ValidCoord,
        (
            member(Coord, Coords),
            withinBoard(Board, Coord),
            % Store the remaining Coords.
            ValidCoord = Coord
        ),
        UnsortedValidCoords
    ),
    % Make sure the List is sorted when returned.
    sort(UnsortedValidCoords, SortedValidCoords).


/* Pairs Operations */

% Return the pairs with the elements requested.
getPairsWithElements(Pairs, Element, RequestedPairs):-
    findall(
        RequestedPair,
        (
            % Get Pair from Pairs
            member((Element1, Element2), Pairs),
            (
                % If element is var.
                var(Element) -> (var(Element1) ; var(Element2)) ;
                % If element is an occupation.
                (Element1 == Element ; Element2 == Element)
            ),
            % Save the element requested
            RequestedPair = (Element1, Element2)
        ),
        RequestedPairs
    ).

% End Clause for CheckSingularParity.
checkSingularParity([], Tents, [], Tents):-!.

% Passing Clause for CheckSingularParity.
%
% (Checks if a tree has only one tent associated with it).
checkSingularParity([Tree|Trees], Tents, RemainingTrees, RemainingTents):-
    % Remove neighbors from Tents
    vizinhanca(Tree, TreeNeigh),
    removeFromList(Tents, TreeNeigh, LeftOverTents),
    % Verify if only one Tent was removed with this (remove tent-tree pair).
    length(Tents, TentsAmount),
    length(LeftOverTents, LeftOverAmount),
    TentsAmount-1 =:= LeftOverAmount,
    checkSingularParity(Trees, LeftOverTents, RemainingTrees, RemainingTents).

% Failing Clause for CheckSingularParity.
checkSingularParity([Tree|Trees], Tents, RemainingTrees, RemainingTents):-
    checkSingularParity(Trees, Tents, RemainingTreesSol, RemainingTents),
    % Save the Trees that were skipped.
    append([Tree], RemainingTreesSol, RemainingTrees).


/* Split Auxiliar Functions */

% Auxiliary function to split the tuple into two coordinates for the function.
insereObjectoEntrePosicoesAux(Board, Occupation, [StartCoord, EndCoord]) :-
    insereObjectoEntrePosicoes(Board, Occupation, StartCoord, EndCoord).


/* Strategy Functions */

% Apply All strategies in the following order.
applyStrategies(P):-
    relva(P),
    aproveita(P),
    unicaHipotese(P),
    limpaVizinhancas(P).


/* Function Controllers for Recursions */

% Controller function for ApplyStrategies, so it stops once it stagnates.
applyStrategiesController(P):-
    P = (Board, _, _),
    % Count the vars before and after strategies
    count_vars(Board, CountBefore),
    applyStrategies(P),
    count_vars(Board, CountAfter),
    % Evaluate if the puzzle stagnated by checking the vars amount.
    (
        % If var Amount is the same, leave function.
        (CountBefore =:= CountAfter) ;
        % Else, continue.
        (applyStrategiesController(P))
    ),
    % Prevent Backtracking.
    !.

% Controller function for CheckSingularParity, so it stops once it stagnates.
checkSingularParityController(Trees, Tents, RemainingTrees, RemainingTents):-
    % Call the function we are controlling the outputs of.
    checkSingularParity(Trees, Tents, LeftOverTrees, LeftOverTents),
    % If it stagnates, Leave Function.
    \+ (Trees == LeftOverTrees, Tents == LeftOverTents),
    % Else, Continue.
    checkSingularParityController(
            LeftOverTrees, 
            LeftOverTents, 
            RemainingTrees, 
            RemainingTents
        ).

% End Clause for the controller, so it saves the values in the variables.
checkSingularParityController(Trees, Tents, LeftOverTrees, LeftOverTents):-
    checkSingularParity(Trees, Tents, LeftOverTrees, LeftOverTents).

% Controller of CheckCircularChain
checkCircularChainController(TreesList, TentsList):-
    % Remove the Tree-Tent pairs from the list that only have one solution.
    checkSingularParityController(TreesList, TentsList, RemainingTrees, RemainingTents),
    % If RemainingLists are empty, Succeed.
    ((RemainingTrees = [], RemainingTents = []) ;
    % Else, Forcibly remove the first Tree-Tent pair and try again.
    (RemainingTrees = [FirstTree|LeftOverTrees],
    vizinhanca(FirstTree, TreeNeigh),
    intersection(TreeNeigh, RemainingTents, [FirstTent|_]),
    removeFromList(RemainingTents, [FirstTent], LeftOverTents),
    checkSingularParityController(LeftOverTrees, LeftOverTents, FinalTrees, FinalTents),
    % If it stagnates, Leave Function (Nothing left to be done).
    \+ (LeftOverTrees == FinalTrees, LeftOverTents == FinalTents),
    % Else, Continue.
    checkCircularChainController(FinalTrees, FinalTents))).


/* Vizinhanca */

% Get the neighborhood of the coordinate.
%
% Copilot helped me with the #= operator for non instantiated variables
% so my code can be poli-modal.
vizinhanca((Line, Col), Coordinates):-
    % Defining the variables.
    Line1 #= Line-1,
    Line2 #= Line+1,
    Col1 #= Col-1,
    Col2 #= Col+1,
    Coordinates = [
        (Line1, Col),    % Top.
        (Line, Col1),    % Left.
        (Line, Col2),    % Right.
        (Line2, Col)     % Bottom.
    ].

% Get the Enlarged neighborhood of the coordinate.
vizinhancaAlargada((Line, Col), Coordinates):-
    % Defining the variables.
    Line1 #= Line-1,
    Line2 #= Line+1,
    Col1 #= Col-1,
    Col2 #= Col+1,
    Coordinates = [
        (Line1, Col1),   % TopLeft.
        (Line1, Col),    % Top.
        (Line1, Col2),   % TopRight.
        (Line, Col1),    % Left.
        (Line, Col2),    % Right.
        (Line2, Col1),   % BottomLeft.
        (Line2, Col),    % Bottom.
        (Line2, Col2)    % BottomRight.
    ].


/* Todas Celulas */

% Get all coordinates of board.
todasCelulas(Board, Coords):-
    findall(
        Coord, 
        (
            % Cycle through each cell.
            nth1(Line, Board, ExtLine),
            nth1(Col, ExtLine, _),
            % Get the coordinates of each cell (Ordered).
            Coord = (Line, Col)
        ), 
        Coords).

% Get all coordinates of a specific element.
%
% Copilot helped me formulate the if statement.
todasCelulas(Board, Coords, Occupation):-
    findall(
        Coord, 
        (
            nth1(Line, Board, ExtLine),
            nth1(Col, ExtLine, CellContent),
            (
                % If Occupation and CellContent are vars, save the Coord.
                var(Occupation) -> var(CellContent); 
                % Else, Verify Cell occupation and save Coord.
                nonvar(CellContent), CellContent == Occupation
            ),
        Coord = (Line, Col)
        ), 
        Coords
    ).


/* Calcula Objectos Tabuleiro */

% Find the amount of a determined element in the board per line/col.
calculaObjectosTabuleiro(Board, LineCounts, ColCounts, Occupation):-
    % Get the counts of each line.
    getCountsInLine(Board, LineCounts, Occupation),
    % Transpose the Board to get the Columns as lines.
    transpose(Board, TransposedBoard),
    % Get the counts of each Column.
    getCountsInLine(TransposedBoard, ColCounts, Occupation).


/* Celula Vazia */

% Return True if cell is empty or grass, 
% false otherwise (don't fail for outside of board)
% Check if its out of bounds Line-wise.
%
% Copilot gave me the Idea to add a withinBoard Function
% But I decided to use it in a different way.
celulaVazia(Board, (Line, Col)):-
    % Negated And = Or with each element negated (return true if element out of bounds).
    (\+ withinBoard(Board, (Line, Col)); 
    % Test the variable or grass situation.
        getCell(Board, (Line, Col), Cell),
        (var(Cell); Cell = r)
    ),
    % Prevent Backtracking.
    !.


/* Insere Objecto Celula */

% Give a new occupation to the cell if not occupied already.
insereObjectoCelula(Board, Occupation, (Line, Col)):-
    getCell(Board, (Line, Col), Cell),
    (Cell = Occupation; true),
    % Prevent Backtracking.
    !.

% Give a new occupation to the cells if not occupied already.
insereObjectoEntrePosicoes(Board, Occupation, (Line1, Col1), (Line2, Col2)):-
    filterCoords(Board, (Line1, Col1), (Line2, Col2), Coordinates),
    maplist(insereObjectoCelula(Board, Occupation), Coordinates),
    % Prevent Backtracking.
    !.


/* Estrategias */

% Fill all with Grass where col or line reached their limit.
relva((Board, LineCountTents, ColCountTents)):-
    calculaObjectosTabuleiro(Board, LineCounts, ColCounts, t),
    % Get the MaxSize of the Lines/Cols.
    length(LineCountTents, ColLength),
    length(ColCountTents, LineLength),
    % Get the Starting and End coordinates of the Line/Col to be Filled.
    findSameWithCoords(LineCounts, LineCountTents, LineLength, FullLineCoords),
    findSameWithCoords(ColCounts, ColCountTents, ColLength, FullColCoords),
    % Fill the Coordinates with grass where possible.
    maplist(insereObjectoEntrePosicoesAux(Board, r), FullLineCoords),
    transpose(Board, TransposedBoard),
    maplist(insereObjectoEntrePosicoesAux(TransposedBoard, r), FullColCoords).

% Fill all the Inaccessible spots with Grass.
inacessiveis(Board):-
    % Get all neighboring Coordinates to all trees.
    todasCelulas(Board, TreeCoords, a),
    findall(
        TreeNeigh,
        (member(Coord, TreeCoords), vizinhanca(Coord, TreeNeigh)),
        TreeNeighs
    ),
    flatten(TreeNeighs, FlatTreeNeighs),
    % Exclude the Coordinates neighboring trees.
    todasCelulas(Board, AllCoords),
    removeFromList(AllCoords, FlatTreeNeighs, FilteredCoords),
    % Fill the remaining slots with Grass.
    maplist(insereObjectoCelula(Board, r), FilteredCoords).

% Fill with tents where they are required to be placed.
aproveita((Board, LineCountTents, ColCountTents)):-
    % Get the MaxSize of the Lines/Cols.
    length(LineCountTents, ColLength),
    length(ColCountTents, LineLength),
    % Get empty or tent spaces.
    calculaObjectosTabuleiro(Board, TentLineCounts, TentColCounts, t),
    calculaObjectosTabuleiro(Board, EmptyLineCounts, EmptyColCounts, _),
    % Add them together to get the total in each line and column of empty and tents.
    sum_lists(TentLineCounts, EmptyLineCounts, AddedLineCounts),
    sum_lists(TentColCounts, EmptyColCounts, AddedColCounts),
    % Find the Cols and lines that have the same values with what's required.
    findSameWithCoords(LineCountTents, AddedLineCounts, LineLength, LineCoords),
    findSameWithCoords(ColCountTents, AddedColCounts, ColLength, ColCoords),
    % Fill The coordinates with tents where possible.
    maplist(insereObjectoEntrePosicoesAux(Board, t), LineCoords),
    transpose(Board, TransposedBoard),
    maplist(insereObjectoEntrePosicoesAux(TransposedBoard, t), ColCoords).

% Place grass around any tents where possible.
limpaVizinhancas((Board, _, _)):-
    % Get all coordinates of tents.
    todasCelulas(Board, TentCoords, t),
    % Find all extended coordinates around tents.
    findall(
        Coords,
        (
            member(TentCoord, TentCoords),
            vizinhancaAlargada(TentCoord, Coords)
        ), 
        TentNeighCoords
    ),
    % Flatten, sort/remove duplicates and invalids from the neighbors list.
    flatten(TentNeighCoords, FlatNeighCoords),
    filterValidCoords(Board, FlatNeighCoords, ValidNeighCoords),
    % Place grass where possible in the neighbors.
    maplist(insereObjectoCelula(Board, r), ValidNeighCoords).

% Place a tent near a tree if there's only 1 empty slot and no tents.
unicaHipotese((Board, _, _)):-
    % Get all coordinates of Trees.
    todasCelulas(Board, TreeCoords, a),
    % Get all the neighbors of the trees.
    findall(
        Coords,
        (
            member(TreeCoord, TreeCoords),
            vizinhanca(TreeCoord, Coords)
        ), 
        TreesNeighCoords
    ),
    % Find the coords where trees only have 1 free neighbor.
    findall(
        OnlyCoord,
        (
            % Get the cells of all Tree Neighbors.
            member(TreeNeighCoords, TreesNeighCoords),
            filterValidCoords(Board, TreeNeighCoords, ValidTreesNeighCoords),
            getCells(Board, ValidTreesNeighCoords, TreeNeighCells),
            % Create coord/cell pairs and get the pairs containing empty cells.
            pairLists(ValidTreesNeighCoords, TreeNeighCells, Pairs),
            % Make sure there are no tents already near the tree.
            getPairsWithElements(Pairs, t, TentPairs),
            length(TentPairs, TentPairsLength),
            TentPairsLength =:= 0,
            % Check the Length of the valid empty pairs to only be 1.
            getPairsWithElements(Pairs, _, RequestedPairs),
            length(RequestedPairs, PairsLength),
            PairsLength =:= 1,
            % Extract the coordinate from the pair and save it.
            RequestedPairs = [(Coord, _)],
            OnlyCoord = Coord
        ),
        OnlyCoords
    ),
    maplist(insereObjectoCelula(Board, t), OnlyCoords).


/* Final Functions */

% Validate Tree-Tent Pairs.
valida(TreesList, TentsList):-
    % Check the size of lists to make sure theres a 1-1 relationship.
    length(TreesList, TreesAmount),
    length(TentsList, TentsAmount),
    TreesAmount =:= TentsAmount,
    % Check if there are any intersections between trees and tents coords.
    intersection(TreesList, TentsList, Intersections),
    length(Intersections, IntersectionsAmount),
    IntersectionsAmount =:= 0,
    % Remove the tree-tent pairs from the list that are unique,
    % And Check for circular Chains.
    checkCircularChainController(TreesList, TentsList),
    % Prevent Backtracking.
    !.  

% Prolog Trial and error Solving Function
resolve(P):-
    P = (Board, ExpectedLineCounts, ExpectedColCounts),
    % Remove Inaccessible spots (Only needs to be ran once).
    inacessiveis(Board),
    % Check if the puzzle is solved.
    tryTent(P),
    % Verify if LineCounts/ColCounts match (Less resource intensive).
    calculaObjectosTabuleiro(Board, LineCounts, ColCounts, t),
    LineCounts == ExpectedLineCounts,
    ColCounts == ExpectedColCounts,
    todasCelulas(Board, TreeCoords, a),
    todasCelulas(Board, TentCoordstest, t),
    % Validate the play (More resource intensive).
    valida(TreeCoords, TentCoordstest),
    % Prevent Backtracking.
    !.

% Try placing tents, and strategies to solve the puzzle.
tryTent(P):-
    % Apply All the strategies and get the coordinates of possible Plays.
    P = (Board, _, _),
    applyStrategiesController(P),
    % Get all Possible Coordinates
    todasCelulas(Board, PossibleCoords, _),
    ( 
        % If there are no possible coordinates left, return true.
        (length(PossibleCoords, PossibleCoordsAmount), PossibleCoordsAmount =:= 0) ; 
        % Place a tent, and let prolog try all remaining possibilities.
        (
            % Select a random Coordinate and place a tent.
            member(Coord, PossibleCoords),
            insereObjectoCelula(Board, t, Coord),
            % Recursively run this function until solved.
            tryTent(P)
        )
    ).