-module (procload_lib).

%% @TODO write comments!
%% @TODO proc function shall spawn a worker process imemdiately instead of savings a dict of {int(),[{fun(),term()}]}

-include_lib("eunit/include/eunit.hrl").
-export ([build_loader/2,build_loader/3, balance_load/2, balance_load/3]).

p_test() ->
	io:format("parallel test~n"),
	
	F = fun(I) -> I*2 end,
	FSum = fun(I,Acc) -> Acc + I end,
	
	L1 = [1,2,3,4,5,5,2,3,35,6,6,7,7,3,2],
	L2 = [3,5,1,6,7,2,22,4,5,5,2,4,64,55],
	
	ExpSum1 = lists:foldr( FSum,0,lists:map(F,L1) ),
	ExpSum2 = lists:foldr( FSum,0,lists:map(F,L2) ),
	
	Builder1 = build_loader(F,length(L1)),
	Builder2 = build_loader(F,length(L2)),
	
	[Builder1(I) || I <- L1],
	[Builder2(I) || I <- L2],
	
	ActL1 = Builder1(collect),
	ActL2 = Builder2(collect),
	
	ActSum1 = lists:foldr( FSum,0,ActL1 ),
	ActSum2 = lists:foldr( FSum,0,ActL2 ),
	
	?assert(ActSum1 == ExpSum1),
	?assert(ActSum2 == ExpSum2).
	
sum_test() ->
	io:format("sum test~n"),
	%io:format("Module: ~p~n, Send results to: ~p~n",[?MODULE,self()]),
	
	%mapping function
	F = fun(I) -> I*2 end,
	
	%sum function
	FSum = fun(I,Acc) -> Acc + I end,
	
	%tested input list
	L = [1,2,3,4,5,6,7,8,9,10],
	
	%doubled list
	L1 = lists:map(F,L),
	
	%expected result
	ExpectSum = lists:foldr(FSum,0,L1),
	
	%load balancer function
	SpawnFun = build_loader(F,3,length(L)),
	
	%put inputs
	[SpawnFun(I) || I <- L],
	
	%..then wait for the results
	R = SpawnFun(collect),
	
	%sum the results
	Sum = lists:foldr(FSum,0,R),
	io:format("~nResults: ~p~nSum: ~p~nExpected: ~p~n",[R,Sum,ExpectSum]),
	
	?assert(Sum == ExpectSum).

balance_load(F,Inputs) ->
	balance_load(F,5,Inputs).
balance_load(F,FunPerProc,Inputs) ->
	io:format("building loader..."),
	Builder = build_config_loader(F,FunPerProc,Inputs),
	Builder(collect).

%build_config_loader(F,Inputs) 
%when is_function(F) andalso is_list(Inputs) ->
%	build_config_loader(F,5,Inputs).
build_config_loader(F,FunPerProc,Inputs) ->
	Builder = build_loader(F,FunPerProc,length(Inputs)),
	[ Builder(I) || I <- Inputs ],
	Builder.
	
build_loader(F,TotFuns) ->
	build_loader(F,5,TotFuns).
build_loader(F,FunPerProc,TotFuns) ->
	S = self(),
	Proc = spawn_link(fun() -> proc(dict:new(),FunPerProc,FunPerProc,0,TotFuns,TotFuns,S) end),
	
	fun(collect) ->
			receive
				{Proc,results,R} -> R
			end;
		(abort) ->
			Proc ! {abort};
		(I) ->
			Proc ! {item,F,I}
	end.

proc    (Funs,_,         _,         ProcListIndex,  0,         TotalFunNum,To) ->
	io:format("balance load ~p work items in ~p processes~n",[TotalFunNum,ProcListIndex+1]),
	exec(dict:to_list(Funs),self(),[]),
	collect([],To,TotalFunNum);
proc    (Funs,FunPerProc,0,         ProcListIndex,  AllCounter,TotalFunNum,To) ->
	proc(Funs,FunPerProc,FunPerProc,ProcListIndex+1,AllCounter,TotalFunNum,To);
proc    (Funs,FunPerProc,Counter,   ProcListIndex,  AllCounter,TotalFunNum,To) ->
	receive
		{item,Fun,In} ->
			L = case dict:is_key(ProcListIndex,Funs) of
				true  -> dict:fetch(ProcListIndex,Funs);
				false -> []
			end,
			proc(dict:store(ProcListIndex,[{Fun,In}|L],Funs),FunPerProc,Counter-1,ProcListIndex,AllCounter-1,TotalFunNum,To);
		{abort} -> io:format("Proc abort received, exiting~n"), exit(abort);
		Any -> io:format("Proc ignored message: ~p~n",[Any]), proc(Funs,FunPerProc,Counter,ProcListIndex,AllCounter,TotalFunNum,To)
	end.

exec([],_,Pids) -> Pids;
exec([{_,Funs}|T],To,Spawned) ->
	Newly = spawn_link(
		fun() -> 
			%[ To ! {result,F(I)} || {F,I} <- Funs ]
			Partial = [ F(I) || {F,I} <- Funs ],
			%io:format("Partial: ~p~n",[Partial]),
			To ! {result,Partial}
		end
	),
	exec(T,To,[Newly|Spawned]).

collect(Acc,To,0) ->
	To ! {self(),results,Acc};%lists:flatten(Acc)};
collect(Acc,To,Counter) ->
	receive
		{result,R} ->  collect(lists:append(R,Acc),To,Counter-length(R)); %collect(lists:append(R,Acc),To,Counter-length(R));
		{abort} -> io:format("Collect abort received, exiting~n"), exit(abort);
		Any -> io:format("Collect ignored message: ~p~n",[Any]),collect(Acc,To,Counter)
	end.

%append([],Out) -> io:format("Appended output: ~p~n",[Out]), Out;
%append([H|T],Out) -> append(T,[H|Out]).

