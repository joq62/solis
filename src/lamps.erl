%%%-------------------------------------------------------------------
%%% @author $author
%%% @copyright (C) $year, $company
%%% @doc
%%%
%%% @end
%%% Created : $fulldate
%%%-------------------------------------------------------------------
-module(lamps).  

-behaviour(gen_statem).

%% API
-export([start/0]).
-export([start_link/0]).
-export([
	 status/0,
	 switch_on/1,
	 switch_off/1
	]).
%% gen_statem callbacks
-export([
         callback_mode/0,
         init/1,
         format_status/2,
         state_name/3,
         handle_event/4,
         terminate/3,
         code_change/4
        ]).

-export([
	 are_on/0,
	 are_off/0,
	 lamps_on/3,
	 lamps_off/3
	 
	]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% HW detects that the button is released
%%
%% @up() -> void
%% @end
%%--------------------------------------------------------------------
switch_on(Button) ->
    gen_statem:cast(?MODULE, {switch_on,Button}). 
%%--------------------------------------------------------------------
%% @doc
%% HW detects that the button is pushed down
%%
%% @down() -> void
%% @end
%%--------------------------------------------------------------------
switch_off(Button) ->
    gen_statem:cast(?MODULE, {switch_off,Button}). 


%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start()->
    start_link().

start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Define the callback_mode() for this callback module.
%%
%% @spec callback_mode() -> state_functions |
%%                          handle_event_function |
%%                          [state_functions, state_enter] |
%%                          [handle_event_function, state_enter]
%% @end
%%--------------------------------------------------------------------
callback_mode() ->
    state_functions.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, State, Data} |
%%                     {ok, State, Data, Actions} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
 %   InitState=case are_on() of
%		  true->
%		      lamps_on;
%		  false->
%		      lamps_off
%	      end,
    InitState=lamps_on,
    {ok, InitState, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
%%
%% @spec format_status(Opt, [PDict, State, Data]) -> Status
%% @end
%%--------------------------------------------------------------------
format_status(_Opt, [_PDict, State, Data]) ->
    [{data, [{"State", {State, Data}}]}].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name.  If callback_mode is statefunctions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.
%%
%% @spec state_name(Event, OldState, Data) ->
%%                   {next_state, NextState, NewData} |
%%                   {next_state, NextState, NewData, Actions} |
%%                   {keep_state, NewData} |
%%                   {keep_state, NewData, Actions} |
%%                   keep_state_and_data |
%%                   {keep_state_and_data, Actions} |
%%                   {repeat_state, NewData} |
%%                   {repeat_state, NewData, Actions} |
%%                   repeat_state_and_data |
%%                   {repeat_state_and_data, Actions} |
%%                   stop |
%%                   {stop, Reason} |
%%                   {stop, Reason, NewData} |
%%                   {stop_and_reply, Reason, Replies} |
%%                   {stop_and_reply, Reason, Replies, NewData}
%% @end
%%--------------------------------------------------------------------
lamps_on(cast,{switch_on,"switch_lamps"},_Data) ->
    io:format("lamps_on: switch_on ~p~n",[{?MODULE,?LINE}]),
    {keep_state_and_data};
lamps_on(cast,{switch_off,"switch_lamps" },Data) ->
    spawn(fun()->tradfri_bulb_E14_ws_candleopal_470lm:turn_off("lamp_hall_strindberg") end),	    
    spawn(fun()->tradfri_bulb_e27_ww_806lm:turn_off("lamp_livingroom_small_board") end),
    spawn(fun()->tradfri_bulb_e27_ww_806lm:turn_off("lamp_livingroom_floor") end),  
    io:format("lamps_on: switch_off ~p~n",[{?MODULE,?LINE}]),
    {next_state, lamps_off, Data}.
lamps_off(cast,{switch_on,"switch_lamps"},Data) ->
    spawn(fun()->tradfri_bulb_E14_ws_candleopal_470lm:turn_on("lamp_hall_strindberg",78,443) end),	    
    spawn(fun()->tradfri_bulb_e27_ww_806lm:turn_on("lamp_livingroom_small_board",78) end),
    spawn(fun()->tradfri_bulb_e27_ww_806lm:turn_on("lamp_livingroom_floor",78) end),
    io:format("lamps_off: switch_on -> on ~p~n",[{?MODULE,?LINE}]),
    {next_state, lamps_on, Data};
lamps_off(cast,{switch_off,"switch_lamps" },_Data) ->
    io:format("lamps_off: switch_off -> off ~p~n",[{?MODULE,?LINE}]),
    {keep_state_and_data}.

state_name({call, Caller}, _Msg, Data) ->
    {next_state, state_name, Data, [{reply, Caller, ok}]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextState, NewData} |
%%                   {next_state, NextState, NewData, Actions} |
%%                   {keep_state, NewData} |
%%                   {keep_state, NewData, Actions} |
%%                   keep_state_and_data |
%%                   {keep_state_and_data, Actions} |
%%                   {repeat_state, NewData} |
%%                   {repeat_state, NewData, Actions} |
%%                   repeat_state_and_data |
%%                   {repeat_state_and_data, Actions} |
%%                   stop |
%%                   {stop, Reason} |
%%                   {stop, Reason, NewData} |
%%                   {stop_and_reply, Reason, Replies} |
%%                   {stop_and_reply, Reason, Replies, NewData}
%% @end
%%--------------------------------------------------------------------
%handle_event(cast,{down,Button}, Data) ->
 %   io:format("down,Button ~p~n",[{Button,?MODULE,?LINE}]),    
  %  {keep_state, Data};


handle_event(cast, Msg,State, Data) ->
    io:format("unmatched cast ~p~n",[{Msg,State,Data,?MODULE,?LINE}]),       
    {next_state, State, Data};

handle_event({call, From}, Msg, State, Data) ->
    io:format("unmatched call ~p~n",[{Msg,State,Data,From,?MODULE,?LINE}]),       
    {next_state, State, Data, [{reply, From, ok}]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State, Data) -> Ignored
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State, _Data) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, OldState, OldData, Extra) ->
%%                   {ok, NewState, NewData} |
%%                   Reason
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

status()->
    L1={"lamp_hall_strindberg",tradfri_bulb_E14_ws_candleopal_470lm:is_on("lamp_hall_strindberg")},	    
    L2={"lamp_livingroom_small_board",tradfri_bulb_e27_ww_806lm:is_on("lamp_livingroom_small_board")},
    L3={"lamp_livingroom_floor",tradfri_bulb_e27_ww_806lm:is_on("lamp_livingroom_floor")},
    [L1,L2,L3].

are_on()->
    case [DeviceId||{DeviceId,false}<-status()] of
	[]->
	    true;
	_->
	    false
    end.
are_off()->
    case [DeviceId||{DeviceId,true}<-status()] of
	[]->
	    true;
	_->
	    false
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================
