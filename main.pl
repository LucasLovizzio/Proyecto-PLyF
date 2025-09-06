% Ejecutar:
% swipl -q -s run.pl -- --products=products.json --cart=cart.json --discounts=discounts.json [--coupon=SAVE10]

:- initialization(main).
:- use_module(library(http/json)).
:- use_module(library(lists)).

:- dynamic product/5.        % product(Id, Name, Category, Brand, Price)
:- dynamic cart_item/3.      % cart_item(Index, ProductId, Qty)
:- dynamic rule_line/1.      % guarda dicts de descuentos de línea
:- dynamic rule_cart/1.      % guarda dicts de descuentos de carrito
:- dynamic coupon/1.         % coupon(Code) si se pasó por CLI
:- dynamic cart_index/1.

% ------------------- Carga de archivos -------------------
read_json_dict(File, Dict) :-
  open(File, read, S), json_read_dict(S, Dict), close(S).

load_products(File) :-
  read_json_dict(File, List),
  retractall(product(_,_,_,_,_)),
  forall(member(D, List),
    ( IdS = D.get(id), Name = D.get(name), Cat = D.get(category),
      Brand = D.get(brand), Price = D.get(price),
      atom_string(Id, IdS),
      atom_string(CatA, Cat),
      atom_string(BrandA, Brand),
      assertz(product(Id, Name, CatA, BrandA, Price))
    )).

load_cart(File) :-
  read_json_dict(File, List),
  retractall(cart_item(_,_,_)),
  retractall(cart_index(_)),
  asserta(cart_index(1)),
  forall(member(D, List),
    ( DId=D.get(product_id), Qty=D.get(qty),
      cart_index(I), atom_string(Pid, DId),
      assertz(cart_item(I, Pid, Qty)),
      retract(cart_index(I)), I2 is I+1, asserta(cart_index(I2))
    )).

load_discounts(File) :-
  read_json_dict(File, D),
  retractall(rule_line(_)), retractall(rule_cart(_)),
  ( get_dict(line_discounts, D, Line) -> true ; throw(error(no_line_discounts, File)) ),
  ( get_dict(cart_discounts, D, Cart) -> true ; throw(error(no_cart_discounts, File)) ),
  forall(member(L, Line), assertz(rule_line(L))),
  forall(member(C, Cart), assertz(rule_cart(C))).

% ------------------- Cálculos base -------------------
line_subtotal(Pid, Qty, Sub) :- product(Pid, _, _, _, Price), Sub is Price * Qty.

subtotal(S) :-
  findall(Sub, (cart_item(_,Pid,Qty), line_subtotal(Pid,Qty,Sub)), Subs),
  sum_list(Subs, S).

% ------------------- Descuentos de línea -------------------
% Soporta:
%  - {"type":"percent_category","category":"electronics","percent":10,"label":"10% electrónica"}
%  - {"type":"percent_brand","brand":"brand_a","percent":5,"label":"5% Brand A"}
%  - {"type":"bogo_category","category":"snacks","buy":3,"pay":2,"label":"3x2 snacks"}

line_rule_amount(RuleDict, Amount) :-
  Type = RuleDict.get(type),
  ( Type = "percent_category" ->
      atom_string(Cat, RuleDict.get(category)),
      Perc = RuleDict.get(percent),
      findall(A,
        ( cart_item(_,Pid,Qty),
          product(Pid, _, CatPid, _, _),
          ( CatPid = Cat
            -> line_subtotal(Pid,Qty,Sub), A is round(Sub * Perc / 100)
            ;  A = 0 )
        ), As),
      sum_list(As, Amount)
  ; Type = "percent_brand" ->
      atom_string(Brand, RuleDict.get(brand)),
      Perc = RuleDict.get(percent),
      findall(A,
        ( cart_item(_,Pid,Qty),
          product(Pid, _, _, BrandPid, _),
          ( BrandPid = Brand
            -> line_subtotal(Pid,Qty,Sub), A is round(Sub * Perc / 100)
            ;  A = 0 )
        ), As),
      sum_list(As, Amount)
  ; Type = "bogo_category" ->
      atom_string(Cat, RuleDict.get(category)),
      Buy = RuleDict.get(buy), Pay = RuleDict.get(pay),
      FreeEach is Buy - Pay,
      findall(A,
        ( cart_item(_,Pid,Qty),
          product(Pid, _, CatPid, _, Price),
          ( CatPid = Cat
            -> Free is (Qty // Buy) * FreeEach, A is Free * Price
            ;  A = 0 )
        ), As),
      sum_list(As, Amount)
  ; Amount = 0 ).

applied_line_discounts(AppliedList, TotalLine) :-
  findall(app(LineLabel, Amt),
    ( rule_line(R),
      ( get_dict(label, R, LineLabel) -> true ; LineLabel = R.get(type) ),
      line_rule_amount(R, Amt),
      Amt > 0 ),
    AppliedList),
  findall(A, member(app(_,A), AppliedList), LAs),
  sum_list(LAs, TotalLine).

% ------------------- Descuentos de carrito (política: único) -------------------
% Soporta:
%  - {"type":"money_off","amount":1000,"min_total":8000,"label":"ARS 1000 off si >=8000","exclusive":false,"priority":60}
%  - {"type":"percent_cart","percent":10,"min_total":3000,"label":"SAVE10 10%","code":"SAVE10","exclusive":false,"priority":50}
%  - {"type":"tiered_cart","tiers":[{"threshold":5000,"percent":5},{"threshold":10000,"percent":12}],"label":"Escalonado","exclusive":true,"priority":70}

eligible_cart_rule(S0, _S1, R) :-
  get_dict(type, R, Type),
  ( Type = "money_off" ->
      get_dict(min_total, R, Min), S0 >= Min
  ; Type = "percent_cart" ->
      get_dict(min_total, R, Min),
      ( ( get_dict(code, R, Code), coupon(C), Code = C )
        ; \+ get_dict(code, R, _) ),
      S0 >= Min
  ; Type = "tiered_cart" ->
      get_dict(tiers, R, Tiers),
      ( member(T, Tiers), get_dict(threshold, T, Thr), S0 >= Thr )
  ).

cart_rule_amount(S0, S1, R, Amount) :-
  get_dict(type, R, Type),
  ( Type = "money_off" ->
      get_dict(amount, R, Amount)
  ; Type = "percent_cart" ->
      get_dict(percent, R, Perc), Amount is round(S1 * Perc / 100)
  ; Type = "tiered_cart" ->
      % tomar el mayor % cuyo threshold <= S0
      get_dict(tiers, R, Tiers),
      findall(P,
              ( member(T, Tiers),
                get_dict(threshold, T, Thr),
                get_dict(percent,   T, P),
                S0 >= Thr ),
              Ps),
      ( Ps = [] -> Amount = 0
      ; max_list(Ps, BestP), Amount is round(S1 * BestP / 100) )
  ; Amount = 0).

best_cart_discount(S0, S1, BestLabel, BestAmt) :-
  findall(cand(Label, Amt, Prio),
    ( rule_cart(R),
      eligible_cart_rule(S0, S1, R),
      cart_rule_amount(S0, S1, R, Amt),
      Amt > 0,
      ( get_dict(label, R, Label) -> true ; Label = R.get(type) ),
      ( get_dict(priority, R, Prio) -> true ; Prio = 1000 )
    ),
    Cands),
  ( Cands = [] ->
      BestLabel = none, BestAmt = 0
  ; % elegir por mayor ahorro; desempate por prioridad más baja
    findall(A, member(cand(_,A,_), Cands), Amts),
    max_list(Amts, MaxAmt),
    include({MaxAmt}/[cand(_,A,_) ]>>(A =:= MaxAmt), Cands, Ties),
    findall(P, member(cand(_,MaxAmt,P), Ties), Ps),
    min_list(Ps, BestPrio),
    member(cand(BestLabel, MaxAmt, BestPrio), Ties),
    BestAmt = MaxAmt
  ).

% ------------------- Impresión -------------------
line_sep :- format('~`-t~*|~n', [96]).
pad_right(S, W) :- format('~w~t~*|', [S, W]).
pad_money(N, W) :- format('ARS ~d~t~*|', [N, W]).

print_header :-
  nl, writeln('Carrito:'), nl,
  format('~|~`0t~w~3+ ~w~22+~w~30+~w~42+~w~54+~n', [
    'N°', 'Producto', 'Cantidad', 'Precio Unitario', 'Subtotal'
  ]),
  line_sep.

print_items :-
  forall(
    cart_item(I,Pid,Qty),
    ( product(Pid, Name, _, _, Price),
      line_subtotal(Pid, Qty, Sub),
      format('~|~`0t~d~3+ ~w~22+~d~30+ARS ~d~42+ARS ~d~54+~n', [I, Name, Qty, Price, Sub])
    )
  ).

print_ticket :-
  subtotal(S0),
  applied_line_discounts(LineApps, Dline),
  S1 is S0 - Dline,
  best_cart_discount(S0, S1, CartLabel, Dcart),
  Sfinal is S1 - Dcart,
  print_header, print_items, line_sep,
  % Imprime el subtotal
  format('Subtotal:~t~*|~d~n', [74, S0]),
  % Línea separadora
  format('~`-t~*|~n', [74]),
  % Imprime los descuentos de línea: nombre a la izquierda, valor alineado con el subtotal y el - justo antes del número
  forall(member(app(Label,Amt), LineApps),
    format('~w~t-~d~*|~n', [Label, Amt, 75])),
  % Imprime el total intermedio
  format('~nTotal intermedio:~t~*|~d~n~n', [74, S1]),
  % Si hay descuento de carrito, igual formato
  ( CartLabel \= none ->
      format('~w~t-~d~*|~n', [CartLabel, Dcart, 75])
    ; true ),
  nl, line_sep, format('TOTAL:~t~*|~d~n', [74, Sfinal]).

% ------------------- CLI & main -------------------
parse_args([]).
parse_args([Arg|T]) :-
  ( sub_atom(Arg, 0, _, _, '--coupon=') ->
      sub_atom(Arg, 9, _, 0, C), retractall(coupon(_)), assertz(coupon(C))
  ; true ),
  parse_args(T).

get_opt(Args, Key, Value, Default) :-
  atom_concat('--', Key, Pre),
  ( member(Arg, Args), sub_atom(Arg, 0, _, _, Pre), atomic_list_concat([_,V], '=', Arg)
    -> Value = V
    ;  Value = Default ).

% filepath: c:\Users\llovi\Code\proyecto-PLyF\main.pl

% Predicado para ejecutar múltiples iteraciones y calcular estadísticas
benchmark(Iterations) :-
  format('Ejecutando ~d iteraciones...~n', [Iterations]),
  findall(Time,
    ( between(1, Iterations, I),
      ( I mod 10 =:= 0 -> format('Completadas ~d/~d iteraciones~n', [I, Iterations]) ; true ),
      statistics(cputime, T0),
      % Tu código principal sin halt
      run_main_logic,
      statistics(cputime, T1),
      Time is (T1 - T0) * 1000 % convertir a milisegundos
    ),
    Times),
  calculate_stats(Times, Iterations).

run_main_logic :-
  current_prolog_flag(argv, Args),
  parse_args(Args),
  get_opt(Args, 'products', ProductsFile, 'products.json'),
  get_opt(Args, 'cart', CartFile, 'cart.json'),
  get_opt(Args, 'discounts', DiscountsFile, 'discounts.json'),
  load_products(ProductsFile),
  load_cart(CartFile),
  load_discounts(DiscountsFile),
  % Solo calcular, no imprimir para benchmark
  subtotal(S0),
  applied_line_discounts(_, Dline),
  S1 is S0 - Dline,
  best_cart_discount(S0, S1, _, Dcart),
  _Sfinal is S1 - Dcart.

calculate_stats(Times, Iterations) :-
  sort(Times, SortedTimes),
  length(SortedTimes, Len),
  nth1(1, SortedTimes, Min),
  last(SortedTimes, Max),
  sum_list(SortedTimes, Sum),
  Avg is Sum / Len,
  
  % Mediana
  MedianIndex is Len // 2 + 1,
  nth1(MedianIndex, SortedTimes, Median),
  
  % P95
  P95Index is ceiling(Len * 0.95),
  nth1(P95Index, SortedTimes, P95),
  
  format('~n=== ESTADÍSTICAS DE RENDIMIENTO ===~n'),
  format('Iteraciones: ~d~n', [Iterations]),
  format('Tiempo mínimo: ~2f ms~n', [Min]),
  format('Tiempo máximo: ~2f ms~n', [Max]),
  format('Tiempo promedio: ~2f ms~n', [Avg]),
  format('Mediana: ~2f ms~n', [Median]),
  format('P95: ~2f ms~n', [P95]).

% Modificar main para soportar benchmark
main :-
  catch(
    main_safe,
    Error,
    (format('Error en main: ~w~n', [Error]), halt(1))
  ).

main_safe :-
  format('Iniciando programa...~n'),
  current_prolog_flag(argv, Args),
  format('Args: ~w~n', [Args]),
  
  parse_args(Args),
  format('Args parseados correctamente~n'),
  
  get_opt(Args, 'products', ProductsFile, 'products.json'),
  get_opt(Args, 'cart', CartFile, 'cart.json'),
  get_opt(Args, 'discounts', DiscountsFile, 'discounts.json'),
  format('Archivos: ~w, ~w, ~w~n', [ProductsFile, CartFile, DiscountsFile]),
  
  % Verificar que existen los archivos
  ( exists_file(ProductsFile) -> true ; (format('Error: No existe ~w~n', [ProductsFile]), fail) ),
  ( exists_file(CartFile) -> true ; (format('Error: No existe ~w~n', [CartFile]), fail) ),
  ( exists_file(DiscountsFile) -> true ; (format('Error: No existe ~w~n', [DiscountsFile]), fail) ),
  
  format('=== MIDIENDO TIEMPO DE CARGA ===~n'),
  time((
    load_products(ProductsFile),
    load_cart(CartFile),
    load_discounts(DiscountsFile)
  )),
  
  format('~n=== MIDIENDO TIEMPO DE PROCESAMIENTO Y IMPRESIÓN ===~n'),
  time(print_ticket),
  
  halt.