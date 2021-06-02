#!/usr/local/bin/swipl -q
% (c) Artyom Kuznetsov

:- initialization main.
:- use_module(library(clpfd)).

main:-
    format('Boot program\n', []),
    run_prog,
    format('End program\n', []).

% //////////////////// DEFINITE CLAUSE GRAMMAR IMPLEMENTATION ////////////////////
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

% The list of orders. Translated from natural text into the list. We use this list to apply our DCG grammar to extract data.
orders([    [table, for, 4, at, 20, :, 00, on, 18, march],
            [book, 2, of, us, in, on, 18, march, at, 21, :, 00],
            [3, people, on, 18,th, of, march],
            [reserve,us,a,table,on,march,18,for,a,party,of,9,for,the,theatre,menu]
        ]).

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
    % constraint_tables_between_clients(DaySchedule, DaySchedule),
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

constraint_tables_between_clients([], AllOrders).
constraint_tables_between_clients([Order], AllOrders):-
    constraint_one_table_between_clients(Order, AllOrders).
constraint_tables_between_clients([Order|Orders], AllOrders):-
        constraint_one_table_between_clients(Order, AllOrders),
        constraint_tables_between_clients(Orders, AllOrders).

constraint_one_table_between_clients(CurrentOrder, []).
constraint_one_table_between_clients(CurrentOrder, [Order]):-
    innerpart_of_constraint_one_table_pred(CurrentOrder, Order).
constraint_one_table_between_clients(CurrentOrder, [Order|Orders]):-
    innerpart_of_constraint_one_table_pred(CurrentOrder, Order),
    constraint_one_table_between_clients(CurrentOrder, Orders).

innerpart_of_constraint_one_table_pred(CurrentOrder, OneOrder):-
    CurrentOrder = [Menu1, Time1, Date1, [Table01, Table02, Table03]],
    OneOrder = [Menu2, Time2, Date2, [Table21, Table22, Table23]],
    Order1DinnerTimeStart #= Time1, 
    Order1DinnerTimeEnd #= Time1 + Menu1,
    Order2DinnerTimeStart #= Time2, 
    Order2DinnerTimeEnd #= Time2 + Menu2,
    (Order1DinnerTimeStart in Order2DinnerTimeStart..Order2DinnerTimeEnd #==> 
                    (Table01 #= 1 #/\ Table21 #= 0 #/\ Table02 #=1 #/\ Table22 #= 0 #/\ Table03 #= 1 #/\ Table23 #= 0) #\/ 
                    (Table01 #= 0 #/\ Table21 #= 1 #/\ Table02 #=0 #/\ Table22 #= 1 #/\ Table03 #= 0 #/\ Table23 #= 1)),
    (Order2DinnerTimeStart in Order1DinnerTimeStart..Order1innerTimeEnd #==> 
                    (Table01 #= 1 #/\ Table21 #= 0 #/\ Table02 #=1 #/\ Table22 #= 0 #/\ Table03 #= 1 #/\ Table23 #= 0) #\/ 
                     (Table01 #= 0 #/\ Table21 #= 1 #/\ Table02 #=0 #/\ Table22 #= 1 #/\ Table03 #= 0 #/\ Table23 #= 1)).


collect_order_time([], TimeRest).
collect_order_time([DaySchedule], [ThisTime|TimeRest]):-
    nth0(1, DaySchedule, ThisTime).
collect_order_time([DaySchedule|DaySchedules], [ThisTime|TimeRest]):-
    nth0(1, DaySchedule, ThisTime),
    collect_order_time(DaySchedules, TimeRest).

run_prog:-
    constraint_schedule(DaySchedule),
    display_schedule(DaySchedule), !. % this cut is allowed in the project description, for printout only. 

label_order_time(Vars):-
    labeling([ff], Vars).

display_schedule(Orders):-
    format('Hello to our restaurant! \n', []),
    format('Status for tables represent whether table is occupied. 1 indicates that is occupied, 0 indicates otherwise.\n', []),
    format('------------\n', []),
    format('Orders list:\n', []),
    maplist(printout, Orders),
    format('------------\n', []).
  
printout(Order):-
    Order = [Menu, Time, Date, [Table1, Table2, Table3]],
    format('- Order time: ~d:00 on ~d March, with duration of ~d hours. Status of table for 2 people: ~d, Status of table for 3 people: ~d, Status of table for 4 people: ~d \n',
            [Time, Date, Menu, Table1, Table2, Table3]).


% ========================================================================================================
% ========================================================================================================
% ========================================================================================================

% Special thanks for this resource that helped to clarify some interesting parts of Prolog:
% ref: https://github.com/Anniepoo/swiplclpfd/blob/master/clpfd.adoc
