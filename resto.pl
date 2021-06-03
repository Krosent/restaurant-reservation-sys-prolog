#!/usr/local/bin/swipl -q
% (c) Artyom Kuznetsov

:- initialization main.
:- use_module(library(clpfd)).

% //////////////////// DEFINITE CLAUSE GRAMMAR IMPLEMENTATION ////////////////////
% In order to construct DCG common similarities in the text sentences were determined. 
% Each sentence has more or less the same structure, but the ordering (or how we call, variation) may be different. 
% Because of that we created several variations for sentences where we determine order of parts of the sentence. 
% Each variation may have start_phrase, num_of_seats, time_phrase, menu_selection and date_phrase. 
% I decided to split the sentences this way since all parst, apart start_phrase contains important parts for our constraint system. 
% The translation is straightforward. Also I introduced prepositions, postpositions and dateposition. 
% Prepositions are words before some important parts of the sentence, like of, for and so on. 
% Postpotisions are similar to preposition, but placed after the important part, like us, in and so on. 
% Dateposition is to parse dates with 'th' containing inside. 

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

% //////////////////// CONSTRAINTS IMPLEMENTATION ////////////////////

% day schedule is a list of lists. [[Menu, Time, Date, Tables]].
% Tables = [Table1, Table2, Table3].
% The entry point for constraints where we determine basic structure of project and then we call other constraints here. 
% Also labeling of unground variables happens here. 
constraint_schedule(DaySchedule):-
    orders(Orders),
    % get the number of orders
    length(Orders, NoOfOrders),
    % our schedule contains the same number of elements as the number of orders. 
    length(DaySchedule, NoOfOrders),
    constraint_orders(DaySchedule, Orders),
    collect_order_time(DaySchedule, OrderTimeList),
    exclude(ground, OrderTimeList, OnlyVars),
    % We have commented out below predicate that constraints orders against each other in terms of tables occupation. 
    % The reason for that is that our system takes a long time to compute the operation. The logic is written, but there 
    % some problem why it works slower than it should. 
    % constraint_tables_between_clients(DaySchedule, DaySchedule),
    label_order_time(OnlyVars).

% Recursive predicate to constraint all orders one by one. 
constraint_orders([], []).
constraint_orders([Order], [ClientOrder]):-
    constraint_order(Order, ClientOrder).
constraint_orders([Order|Orders], [ClientOrder|ClientOrders]):-
    constraint_order(Order, ClientOrder),
    constraint_orders(Orders, ClientOrders).

% table 1 -> 2 seats, table 2 -> 3 seats, table 3 -> 4 seats
% We restrict the menu selection, date, then we constraint time and tables by other predicates called here. 
constraint_order([Menu, Time, Date, [Table1, Table2, Table3]], ClientOrder):-
    sentence(Date, NumberOfSeats, Menu, Time, ClientOrder, []),
    Menu in 1..2, 
    Date in 1..31,
    constraint_time(Time, Menu),
    constraint_tables([Table1, Table2, Table3], NumberOfSeats).

% Time can be between 19 and 23 o'clock. The second constraint tells to restrict orders according to their duration. 
% Because the closing time for restaurant is 23 o'clock. 
constraint_time(Time, Menu):-
    Time in 19..23,
    (Time + Menu #=<23).

% We constraint tables of other so that the correct table is taken based on the number of quests. 
constraint_tables([Table1, Table2, Table3], NumberOfSeats):-
    (NumberOfSeats #= 2 #==> Table1 #= 1 #/\ Table2 #= 0 #/\ Table3 #=0),
    (NumberOfSeats #= 3 #==> Table1 #= 0 #/\ Table2 #= 1 #/\ Table3 #=0),
    (NumberOfSeats #= 4 #==> Table1 #= 0 #/\ Table2 #= 0 #/\ Table3 #=1),
    (NumberOfSeats #= 5 #==> Table1 #= 1 #/\ Table2 #= 1 #/\ Table3 #=0),
    (NumberOfSeats #= 6 #==> Table1 #= 1 #/\ Table2 #= 0 #/\ Table3 #=1),
    (NumberOfSeats #= 7 #==> Table1 #= 0 #/\ Table2 #= 1 #/\ Table3 #=1),
    (NumberOfSeats #= 8 #==> Table1 #= 1 #/\ Table2 #= 1 #/\ Table3 #=1),
    (NumberOfSeats #= 9 #==> Table1 #= 1 #/\ Table2 #= 1 #/\ Table3 #=1).

% Recursive predicate to take one order and constraint it against other orders
constraint_tables_between_clients([], AllOrders).
constraint_tables_between_clients([Order], AllOrders):-
    constraint_one_table_between_clients(Order, AllOrders).
constraint_tables_between_clients([Order|Orders], AllOrders):-
        constraint_one_table_between_clients(Order, AllOrders),
        constraint_tables_between_clients(Orders, AllOrders).

% This is a predicate that is used to recursively get orders and constraint them against one order that was selected by above predicate. 
constraint_one_table_between_clients(CurrentOrder, []).
constraint_one_table_between_clients(CurrentOrder, [Order]):-
    innerpart_of_constraint_one_table_pred(CurrentOrder, Order).
constraint_one_table_between_clients(CurrentOrder, [Order|Orders]):-
    innerpart_of_constraint_one_table_pred(CurrentOrder, Order),
    constraint_one_table_between_clients(CurrentOrder, Orders).

% This predicate is used to constraint one order based on the other. 
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


% This predicate is used to extract time of order for each orders. 
collect_order_time([], TimeRest).
collect_order_time([DaySchedule], [ThisTime|TimeRest]):-
    nth0(1, DaySchedule, ThisTime).
collect_order_time([DaySchedule|DaySchedules], [ThisTime|TimeRest]):-
    nth0(1, DaySchedule, ThisTime),
    collect_order_time(DaySchedules, TimeRest).

% Labeling ungrounded variables 
label_order_time(Vars):-
    labeling([ff], Vars).

% //////////////////// RUN Application and Display Implementation ////////////////////

% Predicate to print out the list of orders in user friendly format. 
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

% Entry point for testing the application.
% Notes about testing:
% When you run the application you start with testing of DCG expressions. The test won't past if you pass wrong sentences. 
% Our constraint system constraints variables to be on a certain condition. Hence when you try to run the application with sentences
% that have, for instance order time that is beyond range 19..23 then the program will give you an error. Only correct sentences within constraints are allowed. 
% The constraints for tables between orders are commented out, in order to demonstrate you final result. So when you run the program you see all constraints 
% are being set and you get orders in proper style. However when contraints for tables are turned off we get overlap of tables that are already full. The code
% for constraining tables between orders is implemented suboptimal, that is why it is slower to execute. Apart from this problem all other requirements for this 
% project were implemented. 
main:-
    format('---- Testing Started ----', []),
    test_dcg_translation_1,
    test_dcg_translation_2,
    format('\n---- Testing Ended ---- \n', []),

    format('\n----Boot program ----\n', []),
    run_prog,
    format('----End program ----\n', []),
    halt.

% This predicate should be used to get results from our application. 
run_prog:-
    constraint_schedule(DaySchedule),
    display_schedule(DaySchedule), !. % this cut is allowed in the project description, for printout only. 


% //////////////////// TESTING THE APPLICATION ////////////////////
test_dcg_translation_1:-
    format('\nTest 1 of Natural Text Translation: \n', []),
    % "Can I book a table at 9pm for 2 people on the 19th of March for the standard menu please?" sentence. 
    Order_Sentence = [can, i, book, a, table, at, 9, pm, for, 2, people, on, the, 19, th, of, march, for, the, standard, menu, please],
    sentence(Date, NumberOfSeats, Menu, Time, Order_Sentence, []),
    format('Date:~d, NumberOfSeats:~d, Menu:~d, Time:~d \n', [Date, NumberOfSeats, Menu, Time]),
    format('End of Test 1', []).


test_dcg_translation_2:-
    % "9 people on 18th of March" sentence.
    format('\nTest 2 of Natural Text Translation: \n', []),
    Order_Sentence = [9, people, on, 18, th, of, march],
    sentence(Date, NumberOfSeats, Menu, Time, Order_Sentence, []),
    format('Date:~d, NumberOfSeats:~d, Menu:~d \n', [Date, NumberOfSeats, Menu]),
    format('End of Test 2', []).


% ========================================================================================================
% ========================================================================================================
% ========================================================================================================

% Special thanks for this resource that helped to clarify some interesting parts of Prolog:
% ref: https://github.com/Anniepoo/swiplclpfd/blob/master/clpfd.adoc

% To include the script in the file I was considering this code:
% ref: https://stackoverflow.com/a/56910754/8709654
% ========================================================================================================
% ========================================================================================================
% ========================================================================================================