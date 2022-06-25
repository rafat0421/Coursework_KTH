%% @author rafat
%% @doc @todo Add description to vect.

-module(vect).

-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

zero() -> #{}.

inc(Name, Time) ->
    Current = maps:get(Name, Time, 0),
    Time#{Name => Current + 1}.

merge(TimeA, TimeB) ->
    % Necessary step, as merge simply takes from B if keys intersect
    UpdatedB = maps:map(fun(Key, OldValue) ->
        max(maps:get(Key, TimeA, 0), OldValue)
    end, TimeB),
    maps:merge(TimeA, UpdatedB).

leq(TimeA, TimeB) ->
    maps:fold(fun(Key, Value, Acc) ->
        case Acc of
            false ->
                false;
            _ ->
                Value =< maps:get(Key, TimeB, 0)
        end
    end, true, TimeA).

clock(_) -> #{}.

update(From, Time, Clock) ->
    #{From := TElement} = Time,
    Clock#{From => TElement}.

safe(Time, Clock) -> leq(Time, Clock).