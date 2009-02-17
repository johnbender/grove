%% The MIT License

%% Copyright (c) <year> <copyright holders>

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

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
    Result = case {Method, UriTokens} of
		 {'GET', [_modName,Object,Action|Params]} -> 
		     grove:get_query(Object, Action, Params);
		 {'POST', [_modName,Object]} ->
		     case  yaws_api:postvar(Args, 'query') of
			 {ok, JSON} -> grove:post_query(Object, JSON);
			 undefined -> "\"Query post variable not defined\""
		     end;
		 _other -> {status, 400} % grove:error_status().
	     end,
    json_response(Result).
%%-----------------------------------------------------------------------------------------------
%% Description: pulls the content response together 
%%-----------------------------------------------------------------------------------------------
json_response(JSON) -> {content, ?JSON_MIMETYPE, grove_util:sfrmt( ?JSON_ROOT_ELEMENT, [JSON])}.





