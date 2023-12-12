% Escrever aqui o numero e o nome do aluno
:- use_module(library(clpfd)). % para poder usar transpose/2
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % ver listas completas
:- ["Testes.pl"]. % Ficheiro dado. No Mooshak tera mais puzzles.
% Atencao: nao deves copiar nunca os puzzles para o teu ficheiro de codigo
% Segue-se o codigo

% Get the neighboorhood of the coordinate.
% Copilot helped me with the #= operator for non instanciated variables.
% So my code can be polimodal.
vizinhanca((Line,Col),Coordinates):-
    % Top
    Line1 #= Line-1,
    append([],[[Line1,Col]],Aux1),
    % Left
    Col1 #= Col-1,
    append(Aux1,[[Line,Col1]],Aux2),
    % Right
    Col2 #= Col+1,
    append(Aux2,[[Line,Col2]],Aux3),
    % Bottom
    Line2 #= Line+1,
    append(Aux3,[[Line2,Col]],Coordinates).

% Get the Enlarged neighboorhood of the coordinate.
vizinhancaAlargada((Line,Col),Coordinates):-
    % Defining the variables
    Line1 #= Line-1,
    Line2 #= Line+1,
    Col1 #= Col-1,
    Col2 #= Col+1,
    % TopLeft
    append([],[[Line1,Col1]],Aux1),
    % Top
    append(Aux1,[[Line1,Col]],Aux2),
    % TopRight
    append(Aux2,[[Line1,Col2]],Aux3),
    % Left
    append(Aux3,[[Line,Col1]],Aux4),
    % Right
    append(Aux4,[[Line,Col2]],Aux5),
    % BottomLeft
    append(Aux5,[[Line2,Col1]],Aux6),
    % Bottom
    append(Aux6,[[Line2,Col]],Aux7),
    % BottomRight
    append(Aux7,[[Line2,Col2]],Coordinates).

% Get all coordinates of board.
todasCelulas(Board,Cells):-
    findall(
        Cell, 
        (nth1(Line,Board,ExtLine),nth1(Col,ExtLine,_),Cell = (Line,Col)), 
        Cells).

% Get all coordinates of a specific element
% Copilot helped me formulate the if statement.
todasCelulas(Board,Cells,Occupation):-
    findall(
        Cell, 
        (
            nth1(Line,Board,ExtLine),
            nth1(Col,ExtLine,CellContent),
            (
                var(Occupation) -> var(CellContent); 
                nonvar(CellContent), CellContent == Occupation
            ),
            Cell = (Line,Col)
        ), 
        Cells).

% find the ammount of a determined element in the board.
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

% Return True if cell is empty or grass, 
% false otherwise (dont fail for outside of board)
% Check if its out of bounds Linewise.
celulaVazia(_, (Line, _)):-
    Line =< 0.

celulaVazia(Board, (Line, _)):-
    length(Board, LineHeight),
    Line >= LineHeight.

% Check if its out of bounds ColWise.
celulaVazia(_, (_, Col)):-
    Col =< 0.

celulaVazia(Board, (Line, Col)):-
    length(Board, LineHeight),
    Line >= LineHeight,
    nth1(Line, Board, ExtLine),
    length(ExtLine, ColHeight),
    Col >= ColHeight.

% Check if its var (empty).
celulaVazia(Board, (Line, Col)):-
    length(Board, LineHeight),
    Line >= LineHeight,
    nth1(Line, Board, ExtLine),
    length(ExtLine, ColHeight),
    Col >= ColHeight,
    nth1(Col, ExtLine, Cell),
    var(Cell).

% Check if its r (grass)
celulaVazia(Board, (Line, Col)):- 
    length(Board, LineHeight),
    Line >= LineHeight,
    nth1(Line, Board, ExtLine),
    length(ExtLine, ColHeight),
    Col >= ColHeight,
    nth1(Col, ExtLine, Cell),
    Cell = r.

start :- puzzle(6-13, (T, _, _)), celulaVazia(T, (0, 5)), write(T).
