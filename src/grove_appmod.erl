-module(grove_appmod).
-author('john.m.bender@gmail.com').

-include_lib("yaws/include/yaws_api.hrl").
-export([out/1]).

%%-----------------------------------------------------------------------------------------------
%% Function:    out
%% Description: if the request is a GET, an Object and addtional uri elements are required
%%              if the request is a POST, an Object and the 'query' post variable are required
%%-----------------------------------------------------------------------------------------------
out(Args) ->
    Method = (Args#arg.req)#http_request.method,
    Uri = yaws_api:request_url(Args),
    UriTokens = string:tokens(Uri#url.path, "/"),
    case {Method, UriTokens} of
	{'GET', [_ModName,Object,Action|Params]} -> 
	    grove:get_query(Object, Action, Params);
	{'POST', [_ModName,Object]} ->
	    case  yaws_api:postvar(Args, 'query') of
		{ok, JSON} -> grove:post_query(Object, JSON);
		undefined -> grove:string_response("Query post variable not defined'")
	    end;
	{_Method, _UriTokens} -> {status, 400} % grove:error_status().
    end.



