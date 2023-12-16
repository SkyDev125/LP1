% Escrever aqui o numero e o nome do aluno
:- use_module(library(clpfd)). % para poder usar transpose/2
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % ver listas completas
:- ["Testes.pl"]. % Ficheiro dado. No Mooshak tera mais puzzles.
% Atencao: nao deves copiar nunca os puzzles para o teu ficheiro de codigo
% Segue-se o codigo


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
                var(Occupation) -> var(CellContent); 
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

% Aux Function to reduce code repetition.
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

% Test if cell is inside board's bounds.
withinBoard(Board,(Line, Col)):-
    Line >= 0,
    Col >= 0,
    length(Board,LineSize),
    nth1(Line, Board, ExtLine),         
    Line =< LineSize,
    length(ExtLine, ColSize),
    Col =< ColSize.


/* Insere Objecto Celula */

% Give a new occupation to the cell if not occupied already.
insereObjectoCelula(Board, Occupation, (Line, Col)):-
    getCell(Board, (Line, Col), Cell),
    (Cell = Occupation; true),!.

% Get the cell of the board.
getCell(Board, (Line, Col), Cell):-
    nth1(Line, Board, ExtLine),
    nth1(Col, ExtLine, Cell).

% Give a new occupation to the cells if not occupied already.
insereObjectoEntrePosicoes(Board, Occupation, (Line1, Col1), (Line2, Col2)):-
    filterCoords(Board,(Line1, Col1),(Line2,Col2), Coordinates),
    maplist(insereObjectoCelula(Board, Occupation), Coordinates),!.

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

% getCells Aux Function for filtering.
withinBounds(MinLine, MaxLine, MinCol, MaxCol, (Line, Col)) :-
    Line >= MinLine, Line =< MaxLine, 
    Col >= MinCol, Col =< MaxCol.


/* Estrategias */

% Fill all with Grass where col or line reached their limit.
relva((Board, LineCountTents, ColCountTents)):-
    calculaObjectosTabuleiro(Board, LineCounts, ColCounts, t),
    % Get the MaxSize of the Lines/Cols.
    length(LineCountTents, ColLength),
    length(ColCountTents, LineLenght),
    % Get the Starting and E    	nd coordinates of the Line/Col to be Filled.
    findSameWithCoords(LineCounts,LineCountTents,LineLenght,FullLineCoords),
    findSameWithCoords(ColCounts,ColCountTents,ColLength,FullColCoords),
    % Fill the Coordinates with grass if possible.
    maplist(insereObjectoEntrePosicoesAux(Board, r),FullLineCoords),
    transpose(Board, TransposedBoard),
    maplist(insereObjectoEntrePosicoesAux(TransposedBoard, r),FullColCoords).


% Aux function to find all Coords of full lines/Cols.
findSameWithCoords(LineCounts,CheckLineCounts,LineLenght,LineCoords):-
    findall(
        FullLineCoord, 
        (
            nth1(Index,LineCounts,LineCount),
            nth1(Index,CheckLineCounts,LineCount),
            FullLineCoord = [(Index,1),(Index,LineLenght)]
        ),
        LineCoords
    ).

% Auxiliary function to split the tuple into two coordinates for the function.
insereObjectoEntrePosicoesAux(Board, Occupation, [StartCoord,EndCoord]) :-
    insereObjectoEntrePosicoes(Board, Occupation, StartCoord, EndCoord).

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

% Remove List2 elements from List1.
removeFromList(List1,List2,FilteredList):-
    findall(
        FilteredItem,
        (
            member(FilteredItem,List1),
            \+ member(FilteredItem,List2)
        ),
        FilteredList).

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

% Place grass around any tents where possible.
limpaVizinhancas((Board, _, _)):-
    % get all coordinates of tents
    todasCelulas(Board,TentCoords,t),
    % find all extended coordinates around tents
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

% Filter a list of coordinates with valid coordinates.
filterValidCoords(Coords,SortedValidCoords):-
    findall(
        ValidCoord,
        (
            member(Coord,Coords),
            \+ (Coord = (0,_) ; Coord = (_,0)),
            ValidCoord = Coord
        ),
        UnsortedValidCoords
    ),
    sort(UnsortedValidCoords,SortedValidCoords).

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
            member(TreeNeighCoords, TreesNeighCoords),
            todasCelulas([TreeNeighCoords],EmptyCoords,_),
            length(EmptyCoords, EmptyNum),
            EmptyNum =:= 1,
            OnlyCoord = EmptyCoords
        ),
        OnlyCoords
    ),
    % Flatten, sort/remove duplicates and invalids from the neighboors list.
    filterValidCoords(OnlyCoords,ValidOnlyCoords),
    % Place the tents
    maplist(insereObjectoCelula(Board, t),ValidOnlyCoords).

start:- 
    puzzle(6-14, P),
    relva(P),
    aproveita(P), 
    relva(P),
    unicaHipotese(P),
    limpaVizinhancas(P),write(P).
