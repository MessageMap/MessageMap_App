-module(throttling_middleware).
-behavior(cowboy_middleware).
-export([execute/2]).
execute(Req, Env) ->
  {{IP, _}, Req2} = cowboy_req:peer(Req),
  case throttle:check(my_api_rate, IP) of
    {limit_exceeded, _, _} ->
      lager:warning("IP ~p exceeded api limit", [IP]),
      Req3 = cowboy_req:reply(429, Req2),
      {stop, Req3};
    _ ->
      {ok, Req2, Env}
   end.
