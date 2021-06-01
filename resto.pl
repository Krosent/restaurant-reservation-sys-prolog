:- use_module(library(clpfd)).

% DCG Parser
% #### 
%##### We have several variations of sentences, each has different structure. 
% #### 
sentence(Date, NumberOfSeats, Menu, Time) --> variation(Date, NumberOfSeats, Menu, Time). 
% s0, s -> s0, s1
% s1, s2 -> s2, s3

% We represent different structures of sentenses by variations. 
variation(Date, NumberOfSeats, 2, Time) --> start_phrase, num_of_seats_phrase(NumberOfSeats), time_phrase(Time), date_phrase(Date).
variation(Date, NumberOfSeats, 2, Time) --> start_phrase, num_of_seats_phrase(NumberOfSeats), date_phrase(Date), time_phrase(Time).              
variation(Date, NumberOfSeats, Menu, Time) --> start_phrase, num_of_seats_phrase(NumberOfSeats), menu_selection(Menu), date_phrase(Date).
variation(Date, NumberOfSeats, Menu, Time) --> start_phrase, num_of_seats_phrase(NumberOfSeats), date_phrase(Date), menu_selection(Menu), time_phrase(Time).
variation(Date, NumberOfSeats, 2, Time) --> num_of_seats_phrase(NumberOfSeats), date_phrase(Date).
variation(Date, NumberOfSeats, Menu, Time) --> start_phrase, date_phrase(Date), num_of_seats_phrase(NumberOfSeats), menu_selection(Menu).
variation(Date, NumberOfSeats, Menu, Time) --> start_phrase, time_phrase(Time), num_of_seats_phrase(NumberOfSeats), date_phrase(Date), menu_selection(Menu).

% Start phrase represent the begining of sentence. Usually we have some words that are tight together and they represented below. 
start_phrase --> [table].
start_phrase --> [please, can, we, have, a, table].
start_phrase --> [we, would, like, a, table].
start_phrase --> [reserve, us, a, table].
start_phrase --> [book].
start_phrase --> [reservation].
start_phrase --> [can, i, book, a, table].

% The part of sentence that responsible for the number of seats represents here. 
num_of_seats_phrase(NumberOfSeats) --> preposition, number_of_seats(NumberOfSeats).
num_of_seats_phrase(NumberOfSeats) --> preposition, number_of_seats(NumberOfSeats), postposition.
num_of_seats_phrase(NumberOfSeats) --> number_of_seats(NumberOfSeats), postposition.
num_of_seats_phrase(NumberOfSeats) --> number_of_seats(NumberOfSeats), postposition, postposition, postposition.

% Time representation
time_phrase(Time) --> preposition, time(Time).
time_phrase(Time) --> possibility, preposition, time(Time).

% Date representation can be different hence we have several clauses for that. 
date_phrase(Date) --> preposition, date_numbr(Date), month_str.
date_phrase(Date) --> preposition, month_str, date_numbr(Date).
date_phrase(Date) --> preposition, month_str, date_numbr(Date), dateposition.
date_phrase(Date) --> preposition, date_numbr(Date).
date_phrase(Date) --> preposition, date_numbr(Date), dateposition, postposition, month_str.
date_phrase(Date) --> preposition, preposition, date_numbr(Date), dateposition, postposition, month_str.

% Keyword that used to select preferably date or menu.
possibility --> [preferably].

% By preposition we mean words that come before important phrase, often occured before date and time phrases.  
preposition --> [at].
preposition --> [for].
preposition --> [on].
preposition --> [for, a, party, of].
preposition --> [the].

% Similar to preposition but in the opposite meaning. Comes after important phrase or in the middle.
postposition --> [people].
postposition --> [of].
postposition --> [us].
postposition --> [in].

% The date that represented as 18th and in similar way and has additional linguistic construct represented here. 
dateposition --> [th].

% Different menu selection representations.
menu_selection(Menu) --> menu_theatre(Menu).
menu_selection(Menu) --> menu_standard(Menu), ask_word.
menu_selection(Menu) --> menu_standard(Menu).
menu_selection(Menu) --> possibility, menu_standard(Menu).

% standard -> 2; theatre -> 1
menu_standard(2) --> [for, the, standard, menu].
menu_standard(2) --> [for, standard, menu].
menu_theatre(1) --> [for, theatre, menu].
menu_theatre(1) --> [for, the, theatre, menu].

ask_word --> [please].

% Time representation. 
time(X) --> [X, ':', Y], {integer(X), integer(Y)}.
time(X) --> [X, pm], {integer(X)}.
time(X) --> [X, oclock], {integer(X)}.

% Number value for seats.
number_of_seats(NumberOfSeats) --> [NumberOfSeats], {integer(NumberOfSeats)}.

% Number values for dates.
date_numbr(Date) --> [Date], {integer(Date)}.
date_numbr(Date) --> [Date, '/', Y], {integer(Date)}.

% Months atomic representation.
month_str --> [march].

% Display Results in a human readible way is here: 
%%%%%% Starts %%%%%%
display_results(DayPlan):-
    write("Welcome to our resto\n"),
    nth0(0, DayPlan, Time19),
    nth0(1, DayPlan, Time20),
    nth0(2, DayPlan, Time21),
    nth0(3, DayPlan, Time22),
    nth0(4, DayPlan, Time23),


% A dummy sample of day plan in order to test display_results predicate.
dayplanSample([
               [[1, 2, 4], [2, 1, 3], [0, 0, 0]],
               [[1, 2, 4], [0, 0, 0], [0, 0, 0]],
               [[0, 0, 0], [3, 2, 3], [0, 0, 0]],
               [[0, 0, 0], [0, 0, 0], [0, 0, 0]],
               [[0, 0, 0], [0, 0, 0], [0, 0, 0]]
            ]).


%%%%%% Ends %%%%%%

% Restaraunt terms

% The list of orders. Translated from natural text into the list. We use this list to apply our DCG grammar to extract data.
orders([    [table, for, 4, at, 20, :, 00, on, 18, march],
            [book, 2, of, us, in, on, 18, march, at, 21, :, 00],
            [3, people, on, 18,th, of, march],
            [reserve,us,a,table,on,march,18,for,a,party,of,2,for,the,theatre,menu]
        ]).

order0([9, people, on, 18,th, of, march]).
order1([reserve,us,a,table,on,march,18,for,a,party,of,2,for,the,theatre,menu]).
order2([3, people, on, 18,th, of, march]).
order3([book, 2, of, us, in, on, 18, march, at, 21, :, 00]).
order4([table, for, 3, at, 20, :, 00, on, 18, march]).

% Predicate used to get seats from list of orders.
seats([], _).
seats([Order], [NumberOfSeats|Seats]):-
    sentence(Date, NumberOfSeats, Menu, Time, Order, []).
seats([Order|RestOrders], [NumberOfSeats|Seats]):-
    sentence(Date, NumberOfSeats, Menu, Time, Order, []),
    seats(RestOrders, Seats).

% Predicate used to get list of durations for lunch. 
durations([], _).
durations([Order], [Menu|Durations]):-
    sentence(Date, NumberOfSeats, Menu, Time, Order, []).
durations([Order|RestOrders], [Menu|Durations]):-
    sentence(Date, NumberOfSeats, Menu, Time, Order, []),
    durations(RestOrders, Durations).

% Predicate to get dates from orders.
dates([], _).
dates([Order], [Date|Dates]):-
    sentence(Date, NumberOfSeats, Menu, Time, Order, []).
dates([Order|RestOrders], [Date|Dates]):-
    sentence(Date, NumberOfSeats, Menu, Time, Order, []),
    dates(RestOrders, Dates).

% Predicate to get reservation time. If times is not specified the value is -1.
times([], _).
times([Order], [Time|Times]):-
    sentence(Date, NumberOfSeats, Menu, Time, Order, []).
times([Order|RestOrders], [Time|Times]):-
    sentence(Date, NumberOfSeats, Menu, Time, Order, []),
    times(RestOrders, Times).

% Generate orders indexes. 
% Inspired by solution: https://stackoverflow.com/a/10209665/8709654
generate_idxs(Orders, Indexes):-
    length(Orders, OrdersLength),
    findall(Num, between(1, OrdersLength, Num), Indexes).

% Order id, duration for order pair.
generate_idx_duration_pair(Orders, IdxDurationPair):-
    generate_idxs(Orders, Indexes),
    durations(Orders, Durations),
    pairs_keys_values(IdxDurationPair, Indexes, Durations).

% Order id, number of seats pair.
generate_idx_seats_pair(Orders, IdxSeatsPair):-
    generate_idxs(Orders, Indexes),
    seats(Orders, Seats),
    pairs_keys_values(IdxSeatsPair, Indexes, Seats).

    % Order id, time pair.
generate_idx_times_pair(Orders, IdxTimesPair):-
    generate_idxs(Orders, Indexes),
    times(Orders, Times),
    pairs_keys_values(IdxTimesPair, Indexes, Times).


% Test predicate that is used to get data from orders in a list format for each type of data. 
test_orders_conversion(Seats, Durations, Dates, Times, Indexes):-
    orders(Orders),
    seats(Orders, Seats),
    durations(Orders, Durations),
    dates(Orders, Dates),
    times(Orders, Times),
    generate_idxs(Orders, Indexes).

% Test predicate that is used to test how pairs creation works. 
test_pairs_generation(IdxDurationPairs, IdxSeatsPairs, IdxTimesPairs):-
    orders(Orders),
    generate_idx_duration_pair(Orders, IdxDurationPairs),
    generate_idx_seats_pair(Orders, IdxSeatsPairs),
    generate_idx_times_pair(Orders, IdxTimesPairs).


% day schedule is a list of lists. [[Menu, Time, Date, Tables]].
% Tables = [Table1, Table2, Table3].
constraint_schedule(DaySchedule):-
    orders(Orders),
    % get the number of orders
    length(Orders, NoOfOrders),
    % our schedule contains the same number of elements as the number of orders. 
    length(DaySchedule, NoOfOrders),
    constraint_orders(DaySchedule, Orders),
    collect_order_time(DaySchedule, OrderTimeList),
    exclude(ground, OrderTimeList, OnlyVars),
    label_order_time(OnlyVars).

constraint_orders([], []).
constraint_orders([Order], [ClientOrder]):-
    constraint_order(Order, ClientOrder).
constraint_orders([Order|Orders], [ClientOrder|ClientOrders]):-
    constraint_order(Order, ClientOrder),
    constraint_orders(Orders, ClientOrders).

% table 1 -> 2 seats, table 2 -> 3 seats, table 3 -> 4 seats
constraint_order([Menu, Time, Date, [Table1, Table2, Table3]], ClientOrder):-
    sentence(Date, NumberOfSeats, Menu, Time, ClientOrder, []),
    Menu in 1..2, 
    Date in 1..31,
    constraint_time(Time, Menu),
    constraint_tables([Table1, Table2, Table3], NumberOfSeats).

constraint_time(Time, Menu):-
    Time in 19..23,
    (Time + Menu #=<23).

constraint_tables([Table1, Table2, Table3], NumberOfSeats):-
    (NumberOfSeats #= 2 #==> Table1 #= 1 #/\ Table2 #= 0 #/\ Table3 #=0),
    (NumberOfSeats #= 3 #==> Table1 #= 0 #/\ Table2 #= 1 #/\ Table3 #=0),
    (NumberOfSeats #= 4 #==> Table1 #= 0 #/\ Table2 #= 0 #/\ Table3 #=1),
    (NumberOfSeats #= 5 #==> Table1 #= 1 #/\ Table2 #= 1 #/\ Table3 #=0),
    (NumberOfSeats #= 6 #==> Table1 #= 1 #/\ Table2 #= 0 #/\ Table3 #=1),
    (NumberOfSeats #= 7 #==> Table1 #= 0 #/\ Table2 #= 1 #/\ Table3 #=1),
    (NumberOfSeats #= 8 #==> Table1 #= 1 #/\ Table2 #= 1 #/\ Table3 #=1),
    (NumberOfSeats #= 9 #==> Table1 #= 1 #/\ Table2 #= 1 #/\ Table3 #=1).


collect_order_time([], TimeRest).
collect_order_time([DaySchedule], [ThisTime|TimeRest]):-
    nth0(1, DaySchedule, ThisTime).
    %var(ThisTime).
collect_order_time([DaySchedule|DaySchedules], [ThisTime|TimeRest]):-
    nth0(1, DaySchedule, ThisTime),
    collect_order_time(DaySchedules, TimeRest).

run_prog(DaySchedule):-
    constraint_schedule(DaySchedule).

label_order_time(Vars):-
    labeling([ff], Vars).

% The idea is to have a matrix of orders where we have rows that represent time and columns that represent tables.
% The intersection of row_x_column gives us an order id. By knowing the order we should now which order at what time is reserved and for 
% what duration.

% % days represented as rows.
% constraint_days(DayPlan):-
%     length(DayPlan, 5).

% % tables represented as columns.
% constraint_tables(DayPlan):-
%     transpose(DayPlan, Tables), % get columns.
%     length(Tables, 3). % each hour has only 3 tables available. 

% % intersection between column and row is a cell or order.
% constraint_orders(DayPlan):-
%     length(DayPlan, NumberOfOrders),
%     append(DayPlan, AllOrders),
%     constraint_orders_recursion(AllOrders, NumberOfOrders).
%     %maplist(constraint_order, AllOrders).
%     % AllOrders ins [OrderId, Duration].

% % this is a intermediate predicate that is used inside constrain_orders/1 to recursively constraint all orders.
% constraint_orders_recursion([], _).
% constraint_orders_recursion([Order], NumberOfOrders):-
%     constraint_order(Order, NumberOfOrders).
% constraint_orders_recursion([Order|Orders], NumberOfOrders):-
%     constraint_order(Order, NumberOfOrders),
%     constraint_orders_recursion(Orders, NumberOfOrders).

% % constraint individual order.
% constraint_order([OrderId, Duration, Persons], NumberOfOrders):-
%     length(DayPlan, NumberOfOrders),
%     OrderId in 1..NumberOfOrders,
%     Duration in 1..2.

% constraint_orders_byseats(DayPlan, Seats):-
%     % get different tables for hours. 
    
%     % transpose(DayPlan, [Table4Seats, Table3Seats, Table2Seats])
%     transpose(DayPlan, [Table4Seats, Table3Seats, Table2Seats]),
%     % find all orders where we have 4 seats
%     %seats([Order|RestOrders], [NumberOfSeats|Seats]),
%     orders(Orders),
%     generate_idx_seats_pair(Orders, IdxSeats),
%     pairs_keys_values(IdxSeats, Idx, Seats).
    
% testik(Idx):-    
%     valid_resto_plan(Plan),
%     constraint_orders_byseats(Plan, Idx).

% valid_resto_plan([
%     [[Order01, Duration01, Persons01], [OrderId02, Duration02, Persons02], [OrderId03, Duration03, Persons03]],
%     [[OrderId11, Duration11, Persons11], [OrderId12, Duration12, Persons12], [OrderId13, Duration13, Persons13]],
%     [[OrderId21, Duration21, Persons21], [OrderId22, Duration22, Persons22], [OrderId3, Duration23, Persons23]],
%     [[OrderId31, Duration31, Persons31], [OrderId32, Duration32, Persons32], [OrderId33, Duration33, Persons33]],
%     [[OrderId41, Duration41, Persons41], [OrderId2, Duration42, Persons42], [OrderId3, Duration43, Persons43]]
%     ]).    

% constraint_resto_orders(DayPlan, [], [], []).
% constraint_resto_orders(DayPlan, [IdxDurationPair], [IdxSeatsPair], [IdxTimesPair]):-
%     constraint_resto_order(DayPlan, IdxDurationPair, IdxSeatsPair, IdxTimesPair).
% constraint_resto_orders(DayPlan, [IdxDurationPair|IdxDurationPairs], [IdxSeatsPair|IdxSeatsPairs], [IdxTimesPair|IdxTimesPairs]):-
%     constraint_resto_order(DayPlan, IdxDurationPair, IdxSeatsPair, IdxTimesPair),
%     constraint_resto_orders(DayPlan, IdxDurationPairs, IdxSeatsPairs, IdxTimesPairs).

% % Constraint orders against users orders.  
% constraint_resto_order(DayPlan, Id-Duration, Id-Seats, Id-Time):-
%     % Reference to all data we have from orders. May be used for building constraints
%     test_pairs_generation(IdxDurationPairs, IdxSeatsPairs, IdxTimesPairs),
%     orders(Orders),
%     times(Orders, Times).
    % Means if the time was mentioned occupy the place with such order. 
    %( (Time #> 0) #==> (element(TimeInd, Times, Time) #/\ element(TimeInd, DayPlan, TheDateList) #/\ element(0, TheDateList, Ord) #/\ Ord #= [Id, Duration, Seats]) ).
    % ( Time #> 0 #==> ConstrainedTime1 #= Time1 ).
    

% Entry point predicate. 
resto(DayPlan, Day):-
    valid_resto_plan(DayPlan),
    constraint_days(DayPlan),
    constraint_tables(DayPlan),
    constraint_orders(DayPlan),
    test_pairs_generation(IdxDurationPairs, IdxSeatsPairs, IdxTimesPairs).
    % constraint_resto_orders(DayPlan, IdxDurationPairs, IdxSeatsPairs, IdxTimesPairs).


% constraint_seats(DayPlan):-
%     length(DayPlan, 5),
%     generate_idx_seats_pair(Orders, IdxSeatsPairs),
%     pairs_keys_values(?Pairs, ?Keys, ?Values).








% ========================================================================================================
% ========================================================================================================
% ========================================================================================================

% Special thanks for this resource that helped to clarify some interesting parts of Prolog:
% ref: https://github.com/Anniepoo/swiplclpfd/blob/master/clpfd.adoc

% Warning
% ///// Code that is a prototype of what I was trying to do during development. Has nothing to do with above implementation.  /////

% one version
% app(Resto_Status, SomeOut):-
%     general_constraint(Resto_Status),
%     constraint_seats(Resto_Status, SomeOut).

% constraint_seats(Resto_Status, SomeOut):-
%     element(2, [Time_3, Seats_3], SomeOut).
    
% general_constraint(Resto_Status):-
%     length(Resto_Status, 5),
%     Resto_Status = [
%                     [Time_1, Seats_1],
%                     [Time_2, Seats_2],
%                     [Time_3, Seats_3],
%                     [Time_4, Seats_4],
%                     [Time_5, Seats_5]
%                     ],
%     Time_1 in 19..23,
%     Time_2 in 19..23,
%     Time_3 in 19..23,
%     Time_4 in 19..23,
%     Time_5 in 19..23,
%     Seats_1 in 0..9,
%     Seats_2 in 0..9,
%     Seats_3 in 0..9,
%     Seats_4 in 0..9,
%     Seats_5 in 0..9,
%     all_different([Time_1, Time_2, Time_3, Time_4, Time_5]).
%     %labeling([min(Time_1), max(Time_5)], [Time_1, Time_2, Time_3, Time_4, Time_5]).


% % another version
% orders_constraint([], ConstraintList).
% orders_constraint([Order], [ConstraintedTime|Rest]):-
%     order_constraint(Order, ConstraintedTime).
% orders_constraint([Order|RestOrders], Out):-
%     order_constraint(Order, ConstraintedTime),
%     orders_constraint(RestOrders, [ConstraintedTime|Out]).

% display_order(ConstrainedTime):-
%     format(ConstrainedTime).

% order_constraint(Order, ConstrainedTime, NumberOfSeats, Date):-
%     sentence(Date, NumberOfSeats, Menu, Time, Order, []),
%     NumberOfSeats in 1..9,
%     Date in 1..31,
%     ( Time #< 0 #==> ConstrainedTime in 19..23 ) #/\ ( Time #> 0 #==> ConstrainedTime #= Time ).

% tt(Menu1, Menu2, UpdTime1, UpdTime2, NumberOfSeats1, NumberOfSeats2):-
%     order1(Ord1),
%     order2(Ord2),
%     sentence(Date1, NumberOfSeats1, Menu1, Time1, Ord1, []),
%     sentence(Date2, NumberOfSeats2, Menu2, Time2, Ord2, []),
%     NumberOfSeats1 in 1..9,
%     NumberOfSeats2 in 1..9,
%     (UpdTime1 #= UpdTime2-2 #\/ UpdTime1 #= UpdTime2+2),
%     ( Time1 #< 0 #==> UpdTime1 in 19..23 ) #/\ ( Time1 #> 0 #==> UpdTime1 #= Time1 ),
%     ( Time2 #< 0 #==> UpdTime2 in 19..23 ) #/\ ( Time2 #> 0 #==> UpdTime2 #= Time2 ).

% % third iteration
% experiment3(NumberOfSeats1, NumberOfSeats2, Menu1, Menu2, Time1, Time2, Time0):-
%     order0(Ord0),
%     order1(Ord1),
%     order2(Ord2),
%     order_between_constraint(Ord0, Ord1, NumberOfSeats0, NumberOfSeats1, Menu0, Menu1, Time0, Time1),
%     order_between_constraint(Ord1, Ord2, NumberOfSeats1, NumberOfSeats2, Menu1, Menu2, Time1, Time2).
%     % labeling([min(Time0), max(Time2)], [Time0, Time1, Time2]).
%     %order_between_constraint(Ord1, Ord3, NumberOfSeats1, NumberOfSeats3, Menu1, Menu3, Time1, Time3),
%     %order_between_constraint(Ord2, Ord3, NumberOfSeats2, NumberOfSeats3, Menu2, Menu3, Time2, Time3),
    
% % This predicate sets constraints between two orders. 
% order_between_constraint(Ord1, Ord2, NumberOfSeats1, NumberOfSeats2, Menu1, Menu2, ConstrainedTime1, ConstrainedTime2):-
%     % Get Orders from DCG engine.
%     sentence(Date1, NumberOfSeats1, Menu1, Time1, Ord1, []),
%     sentence(Date2, NumberOfSeats2, Menu2, Time2, Ord2, []),
%     % Constraint range of seats
%     NumberOfSeats1 in 1..9,
%     NumberOfSeats2 in 1..9,
%     Menu1 in 1..2,
%     Menu2 in 1..2,
%     % Set constraint if time was not specified:
%     ( Time1 #< 0 #==> ConstrainedTime1 in 19..22 ) #/\ ( Time1 #> 0 #==> ConstrainedTime1 #= Time1 ),
%     ( Time2 #< 0 #==> ConstrainedTime2 in 19..22 ) #/\ ( Time2 #> 0 #==> ConstrainedTime2 #= Time2 ),
%     (ConstrainedTime1 + Menu1  #=< 23),
%     (ConstrainedTime2 + Menu2  #=< 23),
%     % Constraint time should be in period between 19 and 23 oclock. Also, orders can be made at the same time if enough seats are available.
%     ((NumberOfSeats1 + NumberOfSeats2 #> 9) #==> (ConstrainedTime1 #= ConstrainedTime2 - Menu1) #\/ (ConstrainedTime1 #= ConstrainedTime2 + Menu1)),
%     ((NumberOfSeats1 + NumberOfSeats2 #> 9) #==> (ConstrainedTime2 #= ConstrainedTime1 - Menu2) #\/ (ConstrainedTime2 #= ConstrainedTime1 + Menu2)).


