% Escrever aqui o numero e o nome do aluno
:- use_module(library(clpfd)). % para poder usar transpose/2
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % ver listas completas
:- ["Testes.pl"]. % Ficheiro dado. No Mooshak tera mais puzzles.
% Atencao: nao deves copiar nunca os puzzles para o teu ficheiro de codigo
% Segue-se o codigo

/* Aux Functions */

/* Get information functions */

% Function to reduce code repetition.
getCountsInLine(Board,LineCounts,Occupation):-
    findall(
        LineCount,
        (
            nth1(_,Board,ExtLine),
            % Add [] to make the list inside of the list 
            % so it works with todasCelulas/3.
            todasCelulas([ExtLine],Cells,Occupation),
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

/* Check Bounds Functions */

% Verify if a pair of coordinates is within bounds.
withinBounds(MinLine, MaxLine, MinCol, MaxCol, (Line, Col)) :-
    Line >= MinLine, Line =< MaxLine, 
    Col >= MinCol, Col =< MaxCol.

% Verify if cell is inside board's bounds.
withinBoard(Board,(Line, Col)):-
    Line >= 0,
    Col >= 0,
    length(Board,LineSize),
    nth1(Line, Board, ExtLine),         
    Line =< LineSize,
    length(ExtLine, ColSize),
    Col =< ColSize.


/* List operations */

% Aux function to sum lists together for each value
sum_lists(L1,L2,Values):-
    findall(
        Value,
        (
            nth1(Index,L1,Value1),
            nth1(Index,L2,Value2),
            Value is Value1 + Value2
        ), 
        Values
    ).

% Remove List2 elements from List1.
removeFromList(List1,List2,FilteredList):-
    findall(
        FilteredItem,
        (
            member(FilteredItem,List1),
            \+ member(FilteredItem,List2)
        ),
        FilteredList).

% Pair two lists together.
pairLists(L1,L2,Pairs):-
    findall(
        Pair,
        (
            nth1(I,L1,L1Element),
            nth1(I,L2,L2Element),
            Pair = (L1Element,L2Element) 
        ),
        Pairs    
    ).


/* Filtering Functions */

% Get the Coordinates of the cells
% between two coordinates (Horizontally or Vertically)
%
% Copilot taught me about the min and max Functions.
filterCoords(Board, (Line1, Col1), (Line2, Col2), FilteredCoords):-
    % Find Max and Mins so it works with either.
    MinLine is min(Line1, Line2),
    MaxLine is max(Line1,Line2),
    MinCol is min(Col1,Col2),
    MaxCol is max(Col1,Col2),
    % Filter the cells we want.
    todasCelulas(Board,CellCoords),
    include(
        withinBounds(MinLine, MaxLine, MinCol, MaxCol),
        CellCoords,
        FilteredCoords
    ).

% Aux function to find all Coords of full lines/Cols.
findSameWithCoords(LineCounts,CheckLineCounts,LineLenght,LineCoords):-
    findall(
        FullLineCoord, 
        (
            nth1(Index,LineCounts,LineCount),
            nth1(Index,CheckLineCounts,LineCount),
            % Create pairs of coordinates (start,end).
            FullLineCoord = [(Index,1),(Index,LineLenght)]
        ),
        LineCoords
    ).

% Filter a list of coordinates with valid coordinates.
filterValidCoords(Coords,SortedValidCoords):-
    findall(
        ValidCoord,
        (
            member(Coord,Coords),
            % Fail if any of the following is true.
            \+ (Coord = (0,_) ; Coord = (_,0)),
            % Store the remaining Coords.
            ValidCoord = Coord
        ),
        UnsortedValidCoords
    ),
    sort(UnsortedValidCoords,SortedValidCoords).


/* Pairs Operations */

% Return the pairs with the elements requested.
getPairsWithElements(Pairs, Element, RequestedPairs):-
    findall(
        RequestedPair,
        (
            % Get Pair from Pairs
            member((Element1,Element2),Pairs),
            % Check if Variable or element
            (
                var(Element) -> (var(Element1) ; var(Element2)) ;
                (Element1 == Element ; Element2 == Element)
            ),
            % Save the element requested
            RequestedPair = (Element1,Element2)
        ),
        RequestedPairs
    ).

% End Clause for CheckSingularParity.
checkSingularParity([],Tents, [], Tents):-!.

% Passing Clause for CheckSingularParity.
%
% (Checks if a tree has only one tent associated with it).
checkSingularParity([Tree|Trees], Tents, RemainingTrees, RemainingTents):-
    vizinhanca(Tree, TreeNeigh),
    removeFromList(Tents,TreeNeigh,LeftOverTents),
    length(Tents, TentsAmmount),
    length(LeftOverTents, LeftOverAmmount),
    TentsAmmount-1 =:= LeftOverAmmount,
    checkSingularParity(Trees, LeftOverTents, RemainingTrees, RemainingTents).

% Failing Clause for CheckSingularParity.
checkSingularParity([Tree|Trees], Tents, RemainingTrees, RemainingTents):-
    checkSingularParity(Trees, Tents, RemainingTreesSol, RemainingTents),
    append(RemainingTreesSol,[Tree],RemainingTrees).


/* Split Auxiliar Functions */

% Auxiliary function to split the tuple into two coordinates for the function.
insereObjectoEntrePosicoesAux(Board, Occupation, [StartCoord,EndCoord]) :-
    insereObjectoEntrePosicoes(Board, Occupation, StartCoord, EndCoord).


/* Function Controllers for Recursions */

% Controller function for CheckSingularParity, so it stops once it stagnates.
checkSingularParityController(Trees, Tents, RemainingTrees, RemainingTents):-
    % Call the function we are controlling the outputs of.
    checkSingularParity(Trees, Tents, LeftOverTrees, LeftOverTents),
    % Check if the outputs are the same as inputs
    \+ (Trees == LeftOverTrees, Tents == LeftOverTents),
    % Call the function again if the outputs arent stagnated yet.
    checkSingularParityController(LeftOverTrees, LeftOverTents, RemainingTrees, RemainingTents).

% End Clause for the controller, so it saves the values in the variables.
checkSingularParityController(Trees, Tents, RemainingTrees, RemainingTents):-
    checkSingularParity(Trees, Tents, LeftOverTrees, LeftOverTents),
    Trees == LeftOverTrees,
    Tents == LeftOverTents,
    RemainingTrees = LeftOverTrees,
    RemainingTents = LeftOverTents,
    !.


/* Vizinhança */

% Get the neighboorhood of the coordinate.
%
% Copilot helped me with the #= operator for non instanciated variables
% so my code can be polimodal.
vizinhanca((Line,Col),Coordinates):-
    % Defining the variables.
    Line1 #= Line-1,
    Line2 #= Line+1,
    Col1 #= Col-1,
    Col2 #= Col+1,
    Coordinates = [
        (Line1,Col),    % Top.
        (Line,Col1),    % Left.
        (Line,Col2),    % Right.
        (Line2,Col)     % Bottom.
    ].

% Get the Enlarged neighboorhood of the coordinate.
vizinhancaAlargada((Line,Col),Coordinates):-
    % Defining the variables.
    Line1 #= Line-1,
    Line2 #= Line+1,
    Col1 #= Col-1,
    Col2 #= Col+1,
    Coordinates = [
        (Line1,Col1),   % TopLeft.
        (Line1,Col),    % Top.
        (Line1,Col2),   % TopRight.
        (Line,Col1),    % Left.
        (Line,Col2),    % Right.
        (Line2,Col1),   % BottomLeft.
        (Line2,Col),    % Bottom.
        (Line2,Col2)    % BottomRight.
    ].


/* Todas Celulas */

% Get all coordinates of board.
todasCelulas(Board,Coords):-
    findall(
        Coord, 
        (nth1(Line,Board,ExtLine),nth1(Col,ExtLine,_),Coord = (Line,Col)), 
        Coords).

% Get all coordinates of a specific element.
%
% Copilot helped me formulate the if statement.
todasCelulas(Board,Coords,Occupation):-
    findall(
        Coord, 
        (
            nth1(Line,Board,ExtLine),
            nth1(Col,ExtLine,CellContent),
            (
                % If var -> Get cell coord, check cell and save coord.
                var(Occupation) -> var(CellContent); 
                % Else check Cell for occupation and save coord.
                nonvar(CellContent), CellContent == Occupation
            ),
        Coord = (Line,Col)
        ), 
        Coords
    ).


/* Calcula Objectos Tabuleiro */

% find the ammount of a determined element in the board per line/col.
calculaObjectosTabuleiro(Board, LineCounts, ColCounts, Occupation):-
    % Get the counts of each line.
    getCountsInLine(Board,LineCounts,Occupation),
    % Transpose the Board to get the Columns as lines.
    transpose(Board, TransposedBoard),
    % Get the counts of each Collumn.
    getCountsInLine(TransposedBoard,ColCounts,Occupation).


/* Celula Vazia */

% Return True if cell is empty or grass, 
% false otherwise (dont fail for outside of board)
% Check if its out of bounds Linewise.
%
% Copilot gave me the Idea to add a withinBoard Function
% But I decided to use it in a different way.
celulaVazia(Board, (Line, Col)):-
    % And Negated = Or with each element negated (return true if element out of bounds).
    (\+ withinBoard(Board,(Line, Col)); 
    % Test the variable or grass situation.
        getCell(Board, (Line, Col), Cell),
        (var(Cell); Cell = r)
    ),!.


/* Insere Objecto Celula */

% Give a new occupation to the cell if not occupied already.
insereObjectoCelula(Board, Occupation, (Line, Col)):-
    getCell(Board, (Line, Col), Cell),
    (Cell = Occupation; true),!.

% Give a new occupation to the cells if not occupied already.
insereObjectoEntrePosicoes(Board, Occupation, (Line1, Col1), (Line2, Col2)):-
    filterCoords(Board,(Line1, Col1),(Line2,Col2), Coordinates),
    maplist(insereObjectoCelula(Board, Occupation), Coordinates),!.


/* Estrategias */

% Fill all with Grass where col or line reached their limit.
relva((Board, LineCountTents, ColCountTents)):-
    calculaObjectosTabuleiro(Board, LineCounts, ColCounts, t),
    % Get the MaxSize of the Lines/Cols.
    length(LineCountTents, ColLength),
    length(ColCountTents, LineLenght),
    % Get the Starting and End coordinates of the Line/Col to be Filled.
    findSameWithCoords(LineCounts,LineCountTents,LineLenght,FullLineCoords),
    findSameWithCoords(ColCounts,ColCountTents,ColLength,FullColCoords),
    % Fill the Coordinates with grass if possible.
    maplist(insereObjectoEntrePosicoesAux(Board, r),FullLineCoords),
    transpose(Board, TransposedBoard),
    maplist(insereObjectoEntrePosicoesAux(TransposedBoard, r),FullColCoords).

% Fill all the invalid spots with Grass.
inacessiveis(Board):-
    % Get all neighboaring Coordinates to all trees.
    todasCelulas(Board,TreeCoords,a),
    findall(
        TreeNeigh,
        (member(Coord,TreeCoords),vizinhanca(Coord,TreeNeigh)),
        TreeNeighs
    ),
    flatten(TreeNeighs, FlatTreeNeighs),
    % Exclude the Coordinates neighboaring trees.
    todasCelulas(Board,AllCoords),
    removeFromList(AllCoords,FlatTreeNeighs,FilteredCoords),
    % Fill the remaining slots with Grass.
    maplist(insereObjectoCelula(Board, r),FilteredCoords).

% Fill with tents where they are required to be placed.
aproveita((Board, LineCountTents, ColCountTents)):-
    % Get the MaxSize of the Lines/Cols.
    length(LineCountTents, ColLength),
    length(ColCountTents, LineLenght),
    % Get empty or tent spaces.
    calculaObjectosTabuleiro(Board, TentLineCounts, TentColCounts, t),
    calculaObjectosTabuleiro(Board, EmptyLineCounts, EmptyColCounts, _),
    % Add them together to get the total in each line and column of empty and tents.
    sum_lists(TentLineCounts,EmptyLineCounts,AddedLineCounts),
    sum_lists(TentColCounts,EmptyColCounts,AddedColCounts),
    % Find the Cols and lines that have the same values with what's required.
    findSameWithCoords(LineCountTents,AddedLineCounts,LineLenght,LineCoords),
    findSameWithCoords(ColCountTents,AddedColCounts,ColLength,ColCoords),
    % Fill The coordinates with tents where possible.
    maplist(insereObjectoEntrePosicoesAux(Board, t),LineCoords),
    transpose(Board,TransposedBoard),
    maplist(insereObjectoEntrePosicoesAux(TransposedBoard, t),ColCoords).

% Place grass around any tents where possible.
limpaVizinhancas((Board, _, _)):-
    % get all coordinates of tents
    todasCelulas(Board,TentCoords,t),
    % find all extended coordinates around tents.
    findall(
        Coords,
        (
            member(TentCoord,TentCoords),
            vizinhancaAlargada(TentCoord,Coords)
        ), 
        TentNeighCoords
    ),
    % Flatten, sort/remove duplicates and invalids from the neighboors list.
    flatten(TentNeighCoords,FlatNeighCoords),
    filterValidCoords(FlatNeighCoords,ValidNeighCoords),
    % Place grass where possible in the neighboors.
    maplist(insereObjectoCelula(Board, r),ValidNeighCoords).

% Place a tent near a tree if there's only 1 empty slot and no tents.
unicaHipotese((Board, _, _)):-
    % get all coordinates of Trees.
    todasCelulas(Board,TreeCoords,a),
    % get all the neighboors of the trees.
    findall(
        Coords,
        (
            member(TreeCoord,TreeCoords),
            vizinhanca(TreeCoord,Coords)
        ), 
        TreesNeighCoords
    ),
    % Find only the empty cells coordinates if there's only one.
    findall(
        OnlyCoord,
        (
            % For all neighbours of trees get the cells.
            member(TreeNeighCoords, TreesNeighCoords),
            getCells(Board,TreeNeighCoords,TreeNeighCells),
            % Create coord/cell pairs and get the pairs containing empty cells.
            pairLists(TreeNeighCoords,TreeNeighCells,Pairs),
            getPairsWithElements(Pairs, _, RequestedPairs),
            % Check the Lenght of the pairs to only be 1.
            length(RequestedPairs, PairsLenght),
            PairsLenght =:= 1,
            % Extract the coordinate from the pair and save it.
            RequestedPairs = [(Coord,_)],
            OnlyCoord = Coord
        ),
        OnlyCoords
    ),
    % Flatten, sort/remove duplicates and invalids from the neighboors list.
    filterValidCoords(OnlyCoords,ValidOnlyCoords),
    % Place the tents.
    maplist(insereObjectoCelula(Board, t),ValidOnlyCoords).


/* Final Functions */

% Verify if each tree has only one tent associated with it.
valida(TreesList, TentsList):-
    % Check the size of lists to make sure theres a 1-1 relationship.
    length(TreesList,TreesAmmount),
    length(TentsList, TentsAmmount),
    TreesAmmount =:= TentsAmmount,
    % Remove the tree-tent pairs from the list that are unique.
    checkSingularParityController(TreesList,TentsList,RemainingTrees,RemainingTents),
    % Check if there are no remaining Trees or Tents.
    length(RemainingTrees, RemainingTreesAmmount),
    length(RemainingTents, RemainingTentsAmmount),
    RemainingTreesAmmount =:= 0,
    RemainingTentsAmmount =:= 0.


start:- puzzle(6-14, P),
relva(P),
aproveita(P),
relva(P),
unicaHipotese(P),write(P).

