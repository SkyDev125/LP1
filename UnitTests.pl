:- consult("TendasEArvores").

% Test for vizinhanca/2
:- begin_tests(vizinhanca).
test(vizinhanca_1) :-
    vizinhanca((3, 4), L),
    assertion(L == [(2,4),(3,3),(3,5),(4,4)]).

test(vizinhanca_2) :-
    vizinhanca((3, 1), L),
    assertion(L == [(2,1),(3,0),(3,2),(4,1)]).

:- end_tests(vizinhanca).

% Test for vizinhancaAlargada/2
:- begin_tests(vizinhancaAlargada).
test(vizinhancaAlargada_1) :-
    vizinhancaAlargada((3, 4), L),
    assertion(L == [(2,3),(2,4),(2,5),(3,3),(3,5),(4,3),(4,4),(4,5)]).

:- end_tests(vizinhancaAlargada).

% Test for todasCelulas/2
:- begin_tests(todasCelulas).
test(todasCelulas_1) :-
    puzzle(6-13, (T, _, _)),
    todasCelulas(T, TodasCelulas),
    assertion(TodasCelulas == [(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),
                    (2,1),(2,2),(2,3),(2,4),(2,5),(2,6),
                    (3,1),(3,2),(3,3),(3,4),(3,5),(3,6),
                    (4,1),(4,2),(4,3),(4,4),(4,5),(4,6),
                    (5,1),(5,2),(5,3),(5,4),(5,5),(5,6),
                    (6,1),(6,2),(6,3),(6,4),(6,5),(6,6)]).

:- end_tests(todasCelulas).

% Test for todasCelulas/3
:- begin_tests(todasCelulas_with_object).
test(todasCelulas_with_object_1) :-
    puzzle(6-13, (T, _, _)),
    todasCelulas(T, TodasCelulas, a),
    assertion(TodasCelulas == [(1,5),(2,1),(2,6),(3,4),(4,5),(5,3),(6,3)]).

:- end_tests(todasCelulas_with_object).

% Test for calculaObjectosTabuleiro/4
:- begin_tests(calculaObjectosTabuleiro).
test(calculaObjectosTabuleiro_1) :-
    puzzle(6-13, (T, _, _)),
    calculaObjectosTabuleiro(T, CLinhas, CColunas, a),
    assertion(CLinhas == [1,2,1,1,1,1]),
    assertion(CColunas == [1,0,2,1,2,1]).

test(calculaObjectosTabuleiro_2) :-
    puzzle(6-13, (T, _, _)),
    calculaObjectosTabuleiro(T, CLinhas, CColunas, X),
    assertion(CLinhas == [5,4,5,5,5,5]),
    assertion(CColunas == [5,6,4,5,4,5]).

:- end_tests(calculaObjectosTabuleiro).

% Test for celulaVazia/2
:- begin_tests(celulaVazia).
test(celulaVazia_1) :-
    puzzle(6-13, (T, _, _)),
    assertion(celulaVazia(T, (1, 2))).

test(celulaVazia_2) :-
    puzzle(6-13, (T, _, _)),
    assertion(\+ celulaVazia(T, (1, 5))). % Negate the function call to ensure the test fails.

test(celulaVazia_3) :-
    puzzle(6-13, (T, _, _)),
    assertion(celulaVazia(T, (0, 5))).

test(celulaVazia_4) :-
    puzzle(6-13, (T, _, _)),
    assertion(celulaVazia(T, (1, 7))).

:- end_tests(celulaVazia).

% Test for insereObjectoCelula/3
:- begin_tests(insereObjectoCelula).
test(insereObjectoCelula_1) :-
    T = [[_, _, a, _], [_, _, _, _], [a, a, a, a], [_, _, a, _]],
    insereObjectoCelula(T, r, (1,1)),
    assertion(T = [[r, X, a, Y], [Z, W, V, U], [a, a, a, a], [S, R, a, Q]]),
    assertion(maplist(var, [X, Y, Z, W, V, U, S, R, Q])).

test(insereObjectoCelula_2) :-
    T = [[_, _, a, _], [_, _, _, _], [a, a, a, a], [_, _, a, _]],
    insereObjectoCelula(T, r, (1,3)),
    assertion(T = [[X, Y, a, Z], [W, V, U, J], [a, a, a, a], [S, R, a, Q]]),
    assertion(maplist(var, [X, Y, Z, W, V, U, J, S, R, Q])).

:- end_tests(insereObjectoCelula).

% Test for insereObjectoEntrePosicoes/4
:- begin_tests(insereObjectoEntrePosicoes).
test(insereObjectoEntrePosicoes_1) :-
    T = [[_, _, a, _], [_, _, _, _], [a, a, a, a], [_, _, a, _]],
    insereObjectoEntrePosicoes(T, r, (1,1), (1,4)),
    T = [[r, r, a, r], [X, Y, Z, W], [a, a, a, a], [V, U, a, J]],
    assertion((maplist(var, [X, Y, Z, W, V, U, J]),T)).

:- end_tests(insereObjectoEntrePosicoes).

% Test for relva/1
:- begin_tests(relva).
test(relva_1) :-
    puzzle(6-14, P),
    relva(P),
    P = (
            [
                [X, a, Y, a, Z, r],
                [a, r, r, r, r, r],
                [A, B, C, D, E, r],
                [F, G, a, a, H, r],
                [I, J, K, L, M, r],
                [N, a, O, U, a, r]
            ],
            [3, 0, 1, 1, 1, 1],
            [2, 1, 1, 1, 2, 0]
        ),
    assertion((maplist(var, [X, Y, Z, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, U]),P)).

:- end_tests(relva).

% Test for inacessiveis/1
:- begin_tests(inacessiveis).
test(inacessiveis_1) :-
    puzzle(6-14, (T, _, _)),
    inacessiveis(T),
    T = [
            [X, a, Y, a, Z, r],
            [a, A, r, B, r, r],
            [C, r, D, E, r, r],
            [r, F, a, a, G, r],
            [r, H, I, J, K, r],
            [L, a, M, N, a, O]
        ],
    assertion((maplist(var, [X, Y, Z, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O]),T)).

:- end_tests(inacessiveis).

% Test for aproveita/1
:- begin_tests(aproveita).
test(aproveita_1) :-
    puzzle(6-14, P),
    relva(P),
    aproveita(P),
    P = (
            [
                [t, a, t, a, t, r],
                [a, r, r, r, r, r],
                [X, Y, Z, W, V, r],
                [U, T, a, a, S, r],
                [R, Q, E, O, N, r],
                [M, a, L, K, a, r]
            ],
            [3, 0, 1, 1, 1, 1],
            [2, 1, 1, 1, 2, 0]
        ),
    assertion((maplist(var, [X, Y, Z, W, V, U, T, S, R, Q, E, O, N, M, L, K]),P)).

:- end_tests(aproveita).

% Test for unicaHipotese/1
:- begin_tests(unicaHipotese).
test(unicaHipotese_1) :-
    puzzle(6-14, P),
    relva(P),
    aproveita(P),
    relva(P),
    unicaHipotese(P),
    P = (
            [
                [t, a, t, a, t, r],
                [a, r, r, r, r, r],
                [X, Y, r, Z, W, r],
                [V, t, a, a, U, r],
                [F, S, r, R, Q, r],
                [L, a, r, O, a, r]
            ],
            [3, 0, 1, 1, 1, 1],
            [2, 1, 1, 1, 2, 0]
        ),
    assertion((maplist(var, [X, Y, Z, W, V, U, F, S, R, Q, L, O]),P)).

:- end_tests(unicaHipotese).

% Test for limpaVizinhancas/1
:- begin_tests(limpaVizinhancas).
test(limpaVizinhancas_1) :-
    puzzle(6-14, P),
    relva(P),
    aproveita(P),
    relva(P),
    unicaHipotese(P),
    limpaVizinhancas(P),
    P = (
            [
                [t, a, t, a, t, r],
                [a, r, r, r, r, r],
                [r, r, r, X, Y, r],
                [r, t, a, a, Z, r],
                [r, r, r, A, B, r],
                [C, a, r, D, a, r]
            ],
            [3, 0, 1, 1, 1, 1],
            [2, 1, 1, 1, 2, 0]
        ),
    assertion((maplist(var, [X, Y, Z, A, B, C, D]),P)).

:- end_tests(limpaVizinhancas).

% Test for puzzle/1
:- begin_tests(puzzle).
test(puzzle_6_14) :-
    puzzle(6-14, P), resolve(P),
    assertion(P == (
        [
            [t,a,t,a,t,r],
            [a,r,r,r,r,r],
            [r,r,r,t,r,r],
            [r,t,a,a,r,r],
            [r,r,r,r,t,r],
            [t,a,r,r,a,r]
        ],
        [3,0,1,1,1,1],
        [2,1,1,1,2,0]
    )).

:- end_tests(puzzle).

% Test for valida/2
:- begin_tests(valida).
test(valida_1) :-
    ValidPositions = [(1,2),(1,4),(2,1),(4,3),(4,4),(6,2),(6,5)],
    InvalidPositions = [(1,1),(1,3),(1,5),(3,4),(4,2),(5,5),(6,1)],
    assertion(valida(ValidPositions, InvalidPositions)).

test(valida_2) :-
    ValidPositions = [(1,1),(1,3)],
    InvalidPositions = [(1,2),(1,4)],
    assertion(valida(ValidPositions, InvalidPositions)).

:- end_tests(valida).

:- begin_tests(puzzle).
test(puzzle_6_13) :-
    puzzle(6-13, P),
    resolve(P),
    sol(6-13, P).

test(puzzle_6_14) :-
    puzzle(6-14, P),
    resolve(P),
    sol(6-14, P).


test(puzzle_8_1) :-
    puzzle(8-1, P),
    resolve(P),
    sol(8-1, P).

:- end_tests(puzzle).