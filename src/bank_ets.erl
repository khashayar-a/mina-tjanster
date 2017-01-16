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
    ets:new(bank_accounts, [set, protected, named_table, {read_concurrency, true}]).

init_credit_cards() ->
    ets:new(credit_cards, [set, protected, named_table, {read_concurrency, true}]).

load_bank_accounts_transaction(Number, Owner, FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    Transactions = parse_all_transactions(Device, []),
    ets:insert(bank_accounts, {
		 Number, %% Key
		 #bank_account{number = Number, owner = Owner, transactions = Transactions} %% Value
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
		    Transaction = #transaction{date = Date, 
					       text = Text,
					       amount = element(1,string:to_integer(Amount))},
		    parse_all_transactions(Device, Parsed ++ [Transaction]);
		5 ->
		    [Date, Text, _, Recipient, Amount] = string:tokens(Line,","),
		    Payment = #payment{date = Date, 
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

generate_sample_accounts() ->
   case ets:info(bank_accounts) of
	undefined ->
	   bank_ets:init_bank_accounts(),
	   Account = #bank_account{number = 1, owner = "Khash",
				   transactions = [#payment{date = "20160801", text = "Gym",
							    recipient = "123456", amount = -200},
						   #transaction{date = "20160723", text = "Video streaming",
								amount = -99},
						   #transaction{date = "20160625", text = "Salary",
								amount = 1337}]},
	   Account2 = #bank_account{number = 2, owner = "Clobbe",
				    transactions = [#payment{date = "20160801", text = "Gym",
							     recipient = "123456", amount = -200},
						    #payment{date = "20160701", text = "Gym",
							     recipient = "123456", amount = -200},
						    #transaction{date = "20160627", text = "Video streaming",
								 amount = -99},
						    #transaction{date = "20160625", text = "Salary",
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

%% ===================================================================
%% Tests
%% ===================================================================

-ifdef(TEST).

all_outgoing_test() ->
    generate_sample_accounts(),
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
    generate_sample_accounts(),
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

-endif.
