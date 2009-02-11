-module(grove_appmod).
-export([out/1]).

-include_lib("yaws/include/yaws_api.hrl").

-define(JSON_MIMETYPE, "application/json").
-define(JSON_ROOT_ELEMENT, "{\"result\":~s}").

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
	{'GET', [_modName,Object,Action|Params]} -> 
	    json_response(grove:get_query(Object, Action, Params));
	{'POST', [_modName,Object]} ->
	    case  yaws_api:postvar(Args, 'query') of
		{ok, JSON} -> json_response(grove:post_query(Object, JSON));
		undefined -> json_response("\"Query post variable not defined\"")
	    end;
        _other -> {status, 400} % grove:error_status().
    end.

%%-----------------------------------------------------------------------------------------------
%% Description: pulls the content response together 
%%-----------------------------------------------------------------------------------------------
json_response(JSON) -> {content, ?JSON_MIMETYPE, grove_util:sfrmt( ?JSON_ROOT_ELEMENT, [JSON])}.





