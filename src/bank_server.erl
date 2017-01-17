%%%-------------------------------------------------------------------
%%% @author Khashayar 
%%% @copyright (C) 2017, Khashayar
%%% @doc
%%%
%%% @end
%%% Created : 16 Jan 2017 by Khashayar 
%%%-------------------------------------------------------------------
-module(bank_server).

-behaviour(gen_server).

%% API
-export([start_link/0, outgoing_transactions/0, 
	 calculate_balance/1, detect_interval/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2]).

-define(SERVER, ?MODULE).

-record(state, {}).
-include("records.hrl").

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

outgoing_transactions() ->
    gen_server:call(?SERVER, outgoing).

calculate_balance(Number) ->
    gen_server:cast(?SERVER, {balance, Number}).

detect_interval(Account_Number, Text) ->
    gen_server:cast(?SERVER, {interval, Account_Number, Text}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    bank_ets:init_bank_accounts(),
    bank_ets:init_credit_cards(),
    {ok, #state{}}.

%%--------------------------------------------------------------------

handle_call(outgoing, _, State) ->
    Reply = bank_ets:fetch_all_outgoing_transactions(),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------

handle_cast({balance, Number}, State) ->
    Balance = bank_ets:calculate_bank_account_or_credit_card_balance(Number),
    case Balance of
	{card, Amount} ->
	    io:format("Credit card balance is: ~p~n", [Amount]);
	{bank, Amount} ->
	    io:format("Bank account balance is: ~p~n", [Amount]);
	no_such_account_or_credit_card ->
	    io:format("No account or credit card found for ID(~p)~n", [Number])
    end,
    {noreply, State};
handle_cast({interval, Account_Number, Text}, State) ->
    Interval = bank_ets:detect_time_interval(Account_Number, Text),
    case Interval of
	account_not_found ->
	    io:format("No account found for ID(~p)~n", [Account_Number]);
	no_interval ->
	    io:format("~p has no interval on account(~p)~n ", [Text, Account_Number]);
	Interval ->
	    io:format("~p occurs ~p on account(~p)~n", [Text, Interval, Account_Number])
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------

terminate(_Reason, _State) ->
    ok.

