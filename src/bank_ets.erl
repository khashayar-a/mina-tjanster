-module(bank_ets).

-compile(export_all).

-include("records.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%===================================================================
%%% API
%%%===================================================================

init_bank_accounts() ->
    ets:new(bank_accounts, [set, public, named_table, {read_concurrency, true}]).

init_credit_cards() ->
    ets:new(credit_cards, [set, public, named_table, {read_concurrency, true}]).

load_bank_accounts_transaction(Number, Owner, FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    Transactions = parse_all_transactions(Device, []),
    ets:insert(bank_accounts, {
		 Number, %% Key
		 #bank_account{number = Number, owner = Owner, transactions = Transactions} %% Value
		}).

load_credit_cards_transaction(Number, Name, FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    Transactions = parse_all_transactions(Device, []),
    ets:insert(credit_cards, {
		 Number, %% Key
		 #credit_card{number = Number, name = Name, transactions = Transactions} %% Value
		}).

fetch_bank_account(Number) ->
    case ets:lookup(bank_accounts, Number) of
	[] ->
	    not_found;
	[{_, Val}] ->
	    Val
    end.

insert_bank_account(Number, Owner, Transactions) ->
    ets:insert(bank_accounts, {
		 Number, %% Key
		 {Number, Owner, Transactions} %% Value
		}).


fetch_all_outgoing_transactions() ->
    fetch_all_outgoing_transactions(ets:first(bank_accounts), []).

fetch_outgoing_transactions(Number) ->
    case ets:lookup(bank_accounts, Number) of
	[] ->
	    not_found;	
	[{_, Account}] ->
	    Transactions = lists:filter(fun(T) ->
					       case is_record(T, payment) of
						   true ->
						       T#payment.amount < 0;
						   _ ->
						       T#transaction.amount < 0
					       end
				       end, Account#bank_account.transactions),
	    Account#bank_account{transactions = Transactions}
    end.

calculate_bank_account_or_credit_card_balance(Number) ->
    case ets:lookup(bank_accounts, Number) of
	[] ->
	    case ets:lookup(credit_cards, Number) of
		[] ->
		    no_such_account_or_credit_card;
		[{_, Card}] ->
		    {card, calculate_balance(Card#credit_card.transactions)}
	    end;
	[{_, Account}] ->
	    {bank, calculate_balance(Account#bank_account.transactions)}
    end.


detect_time_interval(Account_Number, Text) ->
    case ets:lookup(bank_accounts, Account_Number) of
	[] ->
	    account_not_found;	
	[{_, Account}] ->
	    find_intervals(Account#bank_account.transactions, Text)
    end.
    


%%%===================================================================
%%% Internal functions
%%%===================================================================

parse_all_transactions(Device, Parsed) ->
    case io:get_line(Device, "") of
        eof  -> 
	    file:close(Device),
	    Parsed;
        Line -> 
	    case length(string:tokens(Line, ",")) of
		4 ->
		    [Date, Text, _, Amount] = string:tokens(Line,","),
		    Transaction = #transaction{date = parse_date(Date), 
					       text = Text,
					       amount = element(1,string:to_integer(Amount))},
		    parse_all_transactions(Device, Parsed ++ [Transaction]);
		5 ->
		    [Date, Text, _, Recipient, Amount] = string:tokens(Line,","),
		    Payment = #payment{date = parse_date(Date), 
				       text = Text,
				       recipient = Recipient,
				       amount = element(1,string:to_integer(Amount))},
		    parse_all_transactions(Device, Parsed ++ [Payment])
	    end
    end.

fetch_all_outgoing_transactions('$end_of_table', Buffer) ->
    Buffer;
fetch_all_outgoing_transactions(Key, Buffer) ->
    Account = fetch_outgoing_transactions(Key),
    fetch_all_outgoing_transactions(ets:next(bank_accounts, Key), Buffer ++ [Account]).

calculate_balance(Transactions) ->
    lists:foldl(fun(T, Sum) -> element(tuple_size(T), T) + Sum end, 0, Transactions).

find_intervals(Transactions, Text) ->
    find_intervals(Transactions, Text, [], []).

find_intervals([Payment = #payment{text = Text}| Tail], Text, [], []) ->
    find_intervals(Tail, Text, [Payment], [Payment]);
find_intervals([Payment = #payment{date = Date, text = Text}| Tail], Text, Monthly, Biweekly) ->
    case abs(calendar:date_to_gregorian_days(Date) - 
		 calendar:date_to_gregorian_days((hd(Biweekly))#payment.date)) of
	14 ->
	    find_intervals(Tail, Text, Monthly, [Payment] ++ Biweekly); 
	_ ->
	    case abs(calendar:date_to_gregorian_days(Date) - 
			 calendar:date_to_gregorian_days((hd(Monthly))#payment.date)) of
		30 ->
		    find_intervals(Tail, Text, [Payment] ++ Monthly, Biweekly); 
		31 ->
		    find_intervals(Tail, Text, [Payment] ++ Monthly, Biweekly);
		_ ->
		    find_intervals(Tail, Text, Monthly, Biweekly)
	    end
    end;
find_intervals([Transaction = #transaction{text = Text}| Tail], Text, [], []) ->
    find_intervals(Tail, Text, [Transaction], [Transaction]); 
find_intervals([Transaction = #transaction{date = Date, text = Text}| Tail], Text, Monthly, Biweekly) ->     
    case abs(calendar:date_to_gregorian_days(Date) - 
		 calendar:date_to_gregorian_days((hd(Biweekly))#transaction.date)) of
	14 ->
	    find_intervals(Tail, Text, Monthly, [Transaction] ++ Biweekly); 
	_ ->
	    case abs(calendar:date_to_gregorian_days(Date) - 
			 calendar:date_to_gregorian_days((hd(Monthly))#transaction.date)) of
		30 ->
		    find_intervals(Tail, Text, [Transaction] ++ Monthly, Biweekly); 
		31 ->
		    find_intervals(Tail, Text, [Transaction] ++ Monthly, Biweekly);
		_ ->
		    find_intervals(Tail, Text, Monthly, Biweekly)
	    end
    end;
find_intervals([], _, Monthly, Biweekly) -> 
    case length(Biweekly) > 2 of
	true ->
	    biweekly;
	false ->
	    case length(Monthly) > 2 of
		true ->
		    monthly;
		false ->
		    no_interval
	    end
    end;
find_intervals([_| Tail], Text, Monthly, Biweekly) -> 
    find_intervals(Tail, Text, Monthly, Biweekly).

generate_sample_bank_accounts() ->
   case ets:info(bank_accounts) of
	undefined ->
	   bank_ets:init_bank_accounts(),
	   Account = #bank_account{number = 1, owner = "Khash",
				   transactions = [#payment{date = "2016-08-01", text = "Gym",
							    recipient = "123456", amount = -200},
						   #transaction{date = "2016-07-23", text = "Video streaming",
								amount = -99},
						   #transaction{date = "2016-06-25", text = "Salary",
								amount = 1337}]},
	   Account2 = #bank_account{number = 2, owner = "Clobbe",
				    transactions = [#payment{date = "2016-08-01", text = "Gym",
							     recipient = "123456", amount = -200},
						    #payment{date = "2016-07-01", text = "Gym",
							     recipient = "123456", amount = -200},
						    #transaction{date = "2016-06-27", text = "Video streaming",
								 amount = -99},
						    #transaction{date = "2016-06-25", text = "Salary",
								 amount = 2500}]},
	   ets:insert(bank_accounts, {
			1, %% Key
			Account %% Value
		       }),
	   ets:insert(bank_accounts, {
			2, %% Key
			Account2 %% Value
		       });
       _ ->
	   ok
    end.

generate_sample_credit_cards() ->
   case ets:info(credit_cards) of
	undefined ->
	   bank_ets:init_credit_cards(),
	   Card = #credit_card{number = 3, name = "Khash",
			       transactions = [#transaction{date = "20160801", text = "Gym",
							    amount = 200},
					       #transaction{date = "20160723", text = "Video streaming",
							    amount = 99},
					       #transaction{date = "20160625", text = "Gym",
							    amount = 200}]},
	   Card2 = #credit_card{number = 4, name = "Clobbe",
				transactions = [#transaction{date = "20160627", text = "Video streaming",
							     amount = 99},
						#transaction{date = "20160625", text = "Gym",
							     amount = 200}]},
	   ets:insert(credit_cards, {
			3, %% Key
			Card %% Value
		       }),
	   ets:insert(credit_cards, {
			4, %% Key
			Card2 %% Value
		       });
       _ ->
	   ok
    end.

parse_date(Date) ->
    [Y,M,D] = string:tokens(Date, "-"),
    {element(1,string:to_integer(Y)), element(1,string:to_integer(M)), element(1,string:to_integer(D))}.

%% ===================================================================
%% Tests
%% ===================================================================

-ifdef(TEST).

all_outgoing_test() ->
    generate_sample_bank_accounts(),
    Accounts = bank_ets:fetch_all_outgoing_transactions(),
    lists:foreach(fun(Account) ->
			  Outgoing = lists:filter(fun(T) ->
							  case is_record(T, payment) of
							      true ->
								  T#payment.amount >= 0;
							      _ ->
								  T#transaction.amount >= 0
							  end
						  end, Account#bank_account.transactions),
			  ?assertEqual(0, length(Outgoing))
		  end, Accounts),
    ets:delete(bank_accounts).

outgoing_test() ->
    generate_sample_bank_accounts(),
    Account = bank_ets:fetch_outgoing_transactions(1),
    Outgoing = lists:filter(fun(T) ->
				   case is_record(T, payment) of
				       true ->
					   T#payment.amount >= 0;
				       _ ->
					   T#transaction.amount >= 0
				   end
			   end, Account#bank_account.transactions),
    ?assertEqual(0, length(Outgoing)),
    ets:delete(bank_accounts).

calculate_balance_test() ->
    generate_sample_bank_accounts(),
    generate_sample_credit_cards(),
    Credit_Balance = calculate_bank_account_or_credit_card_balance(3),
    ?assertEqual({card, 499}, Credit_Balance),

    Account_Balance = calculate_bank_account_or_credit_card_balance(1),
    ?assertEqual({bank, 1038}, Account_Balance),
    ets:delete(bank_accounts),
    ets:delete(credit_cards).


detect_interval_test() ->
    load_bank_accounts_transaction(2, "Khash", "../include/sample_bank_account.txt"),

    Biweekly = bank_ets:detect_time_interval(2, "Gym"),
    ?assertEqual(biweekly, Biweekly),

    Monthly = bank_ets:detect_time_interval(2, "Video streaming"),
    ?assertEqual(monthly, Monthly),

    No_Interval = bank_ets:detect_time_interval(2, "Salary"),
    ?assertEqual(no_interval, No_Interval),
    ets:delete(bank_accounts).

-endif.
