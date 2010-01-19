-module(libra_monitor).
-include("libra.hrl").
-behaviour(gen_server).

-export([link/1, unlink/1]).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).


-record(state, {timer,processes,interval}).


%%% External API
link(Pid) ->
  gen_server:cast(libra_monitor, {link, Pid, self()}).

unlink(Pid) ->
  gen_server:cast(libra_monitor, {unlink, Pid}).


%%% Start the server
start_link(Interval) ->
  gen_server:start_link({local, libra_monitor}, ?MODULE, [Interval], []).


%%% Initialize the server
init([Interval]) ->
  {ok, Timer} = timer:send_interval(Interval, self(), collect_now),
  Table = ets:new(libra_registerd_processes, [set, private]),
  {ok, #state{timer=Timer,processes=Table,interval=Interval}}.


%%% Handle call messages
handle_call(_Request, _From, State) ->
  {reply, ok, State}.


%%% Handle cast messages
handle_cast({link, Pid, Watcher}, #state{processes=Table}=State) ->
  ets:insert(Table, {Pid, Watcher}),
  {noreply, State};

handle_cast({unlink, Pid}, #state{processes=Table}=State) ->
  ets:delete(Table, Pid),
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.


%%% Handle generic messages
handle_info(collect_now, State) ->
  Stats = libra_sys:ps_aux(),
  notify_processes(Stats, State),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.


%%% Before stopping the server
terminate(_Reason, #state{timer=Timer,processes=Table}) ->
  timer:cancel(Timer),
  ets:delete(Table),
  ok.


%%% Code Changes
code_change(_OldVsn, #state{timer=Timer,interval=Internal}=State, _Extra) ->
  timer:cancel(Timer),
  {ok, Timer1} = timer:send_interval(Internal, self(), collect_now),
  {ok, State#state{timer=Timer1}}.


%%% Internal API
notify_processes(Stats, #state{processes=Table}) ->
  ets:foldl(fun({Pid, Watcher}, Stats1) ->
    case collect_stats_for(Pid, Stats1, not_found, [], []) of
    {not_found, _Children, Unchecked} ->
      Watcher ! {process_down},
      Unchecked;
    {Master, Children, Unchecked} ->
      Watcher ! {stats, Master, Children},
      Unchecked
    end
  end, Stats, Table).

collect_stats_for(_, [], Master, Children, Unchecked) -> {Master, Children, Unchecked};
collect_stats_for(MasterPid, [Stats|Rest], Master, Children, Unchecked) ->
  #stats{pid=Pid, ppid=PPid}=Stats,
  case MasterPid of
  Pid   -> collect_stats_for(MasterPid, Rest, Stats, Children, Unchecked);
  PPid  -> 
    {_, Children1, _} = collect_stats_for(Pid, Rest++Unchecked, Stats, Children, []),
    collect_stats_for(MasterPid, Rest, Master, [Stats|Children1], Unchecked);
  _Else -> collect_stats_for(MasterPid, Rest, Master, Children, [Stats|Unchecked])
  end.
