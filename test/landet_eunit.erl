%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point  
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(landet_eunit).    
 
-export([start/0]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->

%    ok=brytar_test("brytare_1"),
%    ok=brytar_test("brytare_2"),
    
 %   timer:sleep(2000),
    % Needs to initiate first time
    toggle(on),
%    spawn(fun()->tradfri_bulb_E14_ws_candleopal_470lm:turn_off("lamp_hall_strindberg") end),	    
%    spawn(fun()->tradfri_bulb_e27_ww_806lm:turn_off("lamp_livingroom_small_board") end),
 %   spawn(fun()->tradfri_bulb_e27_ww_806lm:turn_off("lamp_livingroom_floor") end),
    
  %  timer:sleep(2000),
  %  % Needs to initiate first time
  
 %   spawn(fun()->tradfri_bulb_E14_ws_candleopal_470lm:turn_off("lamp_hall_strindberg") end),	    
 %   spawn(fun()->tradfri_bulb_e27_ww_806lm:turn_off("lamp_livingroom_small_board") end),
 %   spawn(fun()->tradfri_bulb_e27_ww_806lm:turn_off("lamp_livingroom_floor") end),
    
 %   timer:sleep(2000),
    % Needs to initiate first time
  
 %   spawn(fun()->tradfri_bulb_E14_ws_candleopal_470lm:turn_off("lamp_hall_strindberg") end),	    
 %   spawn(fun()->tradfri_bulb_e27_ww_806lm:turn_off("lamp_livingroom_small_board") end),
 %   spawn(fun()->tradfri_bulb_e27_ww_806lm:turn_off("lamp_livingroom_floor") end),

    ok=test_switch("switch_lamps",wait_on),
%    ok=lampa_test("vrum_bordslampa"),
%    ok=hall_lampa_test(),
%    ok=lampa_test("vrum_dim_golv_soffa"),    



 %   ok=test1(),

 %   ok=test_lumi_sensor_motion_aq2(),
  %  ok=test_tradfri_bulb_E14_ws_candleopal_470lm(),

%    ok=test_tradfri_control_outlet(),
%    ok=test_tradfri_motion(),  
  %  ok=test_lumi_weather(),
  %  ok=test_lumi_switch(),
  %  ok=test2(),
  % 

    init:stop(),
    ok.



toggle(on)->
    spawn(fun()->tradfri_bulb_E14_ws_candleopal_470lm:turn_on("lamp_hall_strindberg",78,443) end),	    
    spawn(fun()->tradfri_bulb_e27_ww_806lm:turn_on("lamp_livingroom_small_board",78) end),
    spawn(fun()->tradfri_bulb_e27_ww_806lm:turn_on("lamp_livingroom_floor",78) end),
    timer:sleep(10*1000),
    toggle(off);
toggle(off) ->
    spawn(fun()->tradfri_bulb_E14_ws_candleopal_470lm:turn_off("lamp_hall_strindberg") end),	    
    spawn(fun()->tradfri_bulb_e27_ww_806lm:turn_off("lamp_livingroom_small_board") end),
    spawn(fun()->tradfri_bulb_e27_ww_806lm:turn_off("lamp_livingroom_floor") end),
        timer:sleep(10*1000),
    toggle(on).


test_switch("switch_lamps",wait_off)->
    io:format("test_switch(switch_lamps,wait_off)  ~p~n",[?LINE]),
    true=pushed_off("switch_lamps",true), 
    io:format("true=pushed_off(switch_lamps,true), ~p~n",[?LINE]),
    spawn(fun()->tradfri_bulb_E14_ws_candleopal_470lm:turn_off("lamp_hall_strindberg") end),	    
    spawn(fun()->tradfri_bulb_e27_ww_806lm:turn_off("lamp_livingroom_small_board") end),
    spawn(fun()->tradfri_bulb_e27_ww_806lm:turn_off("lamp_livingroom_floor") end),
    test_switch("switch_lamps",wait_on);

test_switch("switch_lamps",wait_on)->
    io:format("test_switch(switch_lamps,wait_on)  ~p~n",[?LINE]),
    true=pushed_on("switch_lamps",false),
    io:format("true=pushed_on(switch_lamps,false), ~p~n",[?LINE]),
    spawn(fun()->tradfri_bulb_E14_ws_candleopal_470lm:turn_on("lamp_hall_strindberg",78,443) end),	    
    spawn(fun()->tradfri_bulb_e27_ww_806lm:turn_on("lamp_livingroom_small_board",78) end),
    spawn(fun()->tradfri_bulb_e27_ww_806lm:turn_on("lamp_livingroom_floor",78) end),
    test_switch("switch_lamps",wait_off);

test_switch("brytare_2",wait_on) ->

    ok.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
lampa_test(Id)->
    
    R10=tradfri_bulb_e27_ww_806lm:turn_off(Id),
    io:format("R10  ~p~n",[R10]),

 
    R10=tradfri_bulb_e27_ww_806lm:turn_on(Id,100),
    io:format("R10  ~p~n",[R10]),
    
    timer:sleep(4000),

    tradfri_bulb_e27_ww_806lm:turn_off(Id),

   ok.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
hall_lampa_test()->
    true=tradfri_bulb_E14_ws_candleopal_470lm:reachable("hallen_byran_lampa"),
    R2=tradfri_bulb_E14_ws_candleopal_470lm:turn_off("hallen_byran_lampa"),
    io:format("R2  ~p~n",[R2]),
    R3=tradfri_bulb_E14_ws_candleopal_470lm:turn_on("hallen_byran_lampa",78,443),
    io:format("R3  ~p~n",[R3]),
    timer:sleep(5000),
    tradfri_bulb_E14_ws_candleopal_470lm:turn_off("hallen_byran_lampa"),


     ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
brytar_test(Id)->
    io:format("push button on  ~p~n",[Id]),
    true=pushed_on(Id,false),
    io:format("push button off  ~p~n",[Id]),
    true=pushed_off(Id,true),
   
    ok.

pushed_on(_Id,true)->
    true;
pushed_on(Id,false) ->
    ButtonPushedOn=tradfri_on_off_switch:is_on(Id),
    timer:sleep(500),
    pushed_on(Id,ButtonPushedOn).
	     
    
pushed_off(_Id,false)->
    true;
pushed_off(Id,true) ->
    ButtonPushedOff=tradfri_on_off_switch:is_on(Id),
    timer:sleep(500),
    pushed_off(Id,ButtonPushedOff).
	     
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
all_info_test()->
    [{"brytare_1","On/off switch for all lamps"},
     {"brytare_2","On/Off switch TV set"},
     {"hallen_byran_lampa","lampa on cupboard at entrance  "},
     {"huset_innertemp","Tempsensor inside house"},
     {"landet_outlet_1","Outlet for the TV set"},
     {"motion_entrance","Motion detector outdoor entrance door"},
     {"motion_kitchen","Motion detector near kitchen"},
     {"vrum_bordslampa","Lamp on the table in livingroom "},
     {"vrum_dim_golv_soffa","Floor lamp by the sofa livingroom"}] =lists:sort(landet_device:all_info()),
    ok.



setup()->

   ok.
