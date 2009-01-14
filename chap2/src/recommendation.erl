-module (recommendation).
-include_lib("eunit/include/eunit.hrl").

-export ([sim_distance/2, sim_pearson/2, sim_tanimoto/2]).

sim_distance_test() ->
	D1 = dict:from_list( [{a,1},{b,1}] ),
	D2 = dict:from_list( [{a,1},{b,1}] ),
	ActualScore = sim_distance(D1,D2),
	%io:format("Score: ~p~n",[ActualScore]).
	?assert( 1 == ActualScore ).
	
shared_keys_test() ->
	%eunit:debug({'shared keys test'}),
	D1 = dict:from_list([{a,1},{b,1},{d,1}]),
	D2 = dict:from_list([{a,1},{c,1},{b,1}]),
	Keys = shared_keys(D1,D2),
	?assert( length(Keys) == 2 ),
	?assert( true == search(a,Keys) ),
	?assert( true == search(b,Keys) ),
	?assert( false == search(c,Keys) ),
	?assert( false == search(d,Keys) ).

search(_Term,[]) -> false;
search(Term,[Term|_T]) -> true;
search(Term,[_|T]) -> search(Term,T).

%% @spec sim_tanimoto(dict(), dict()) -> float()
%% doc calculate the tanimoto score between two dicts (@see sim_distance/2)
sim_tanimoto(D1,D2) ->
	Shrd = shared_keys(D1,D2),
	case length(Shrd) of
		0 -> 0;
		_ ->
			AB = lists:sum( [ dict:fetch(K,D1) * dict:fetch(K,D2) || K <- Shrd ] ),
			SqSumF = fun(_,I,Acc) -> Acc + (I*I) end,
			SqA = dict:fold(SqSumF,0,D1),
			SqB = dict:fold(SqSumF,0,D2),
			Den = (SqA + SqB - AB),
			if Den /= 0 -> (AB) / Den;
				true -> 0
			end
	end.

%% @spec sim_pearson(dict(), dict()) -> float()
%% @doc calculate the pearson score between two dicts (@see sim_distance/2)
sim_pearson(D1,D2) ->
	Shrd = shared_keys(D1,D2),
	case length(Shrd) of
		0	-> 0;
		Tot	->
			Rs = [{dict:fetch(K,D1),dict:fetch(K,D2)} || K <- Shrd],
			{Sum1,Sum2,Sum1Sq,Sum2Sq,PSum} = lists:foldl(
				fun({X,Y},{TotX,TotY,TotSqX,TotSqY,TotPSum}) ->
					{TotX+X, TotY+Y, TotSqX+(X*X), TotSqY+(Y*Y), TotPSum+(X*Y)} end,
			{0,0,0,0,0}, Rs),
			Num = PSum - ( (Sum1*Sum2) / Tot),
			Den = math:sqrt( (Sum1Sq-(Sum1*Sum1)/Tot) * (Sum2Sq-(Sum2*Sum2)/Tot) ),
			case Den of
				0	-> 0;
				_	-> Num/Den
			end
	end.

%% @spec sim_distance(dict(), dict()) -> float()
%% @doc calculate the euclidean score between two dicts.<br/>
%% the dicts can have a general term() as key but they needs to have a float() or int() for the values.
sim_distance(D1,D2) ->
	Shrd = shared_keys(D1,D2),
	case length(Shrd) of
		0 -> 0;
		_ ->
			SquaresSum = lists:foldl( fun(K,Acc) -> Acc + math:pow( dict:fetch(K,D1) - dict:fetch(K,D2) , 2 ) end, 0, Shrd ),
			(1 / (1 + math:sqrt(SquaresSum)))
	end.
		
%% @private
%% @spec shared_keys(dict(), dict()) -> [term()]
%% @doc search the shared keys between two dictionaries
shared_keys(D1,D2) ->
	lists:filter( fun(K) -> dict:is_key(K,D2) end, dict:fetch_keys(D1)).
