%%%-------------------------------------------------------------------
%%% File    : callingname_package.erl
%%% @author   Stephane Alnet <stephane@shimaore.net>
%%% @doc      CNAM Query implementation.
%%%
%%% @since     22 January 2009 by Stephane Alnet <stephane@shimaore.net>
%%% @end
%%%-------------------------------------------------------------------
-module(callingname_package).

-behaviour(event_package).

%%--------------------------------------------------------------------
%%% Standard YXA Event package exports
%%--------------------------------------------------------------------
-export([
	 init/0,
	 request/4,
	 is_allowed_subscribe/7,
	 notify_content/4,
	 package_parameters/2,
	 subscription_behaviour/3,

	 test/0
	]).


%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("siprecords.hrl").
-include("event.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(my_state, {}).

%%--------------------------------------------------------------------
%% Types
%%--------------------------------------------------------------------

%% @type event_pkg() = string().
%%           Event package string - "calling-name" for this package.
-define(EVENT_PKG, "calling-name").

%%====================================================================
%% Behaviour functions
%% Standard YXA Event package callback functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    () ->
%%            none | {append, SupSpec}
%%
%%            SupSpec = term() "OTP supervisor child specification. Extra processes this event package want the sipserver_sup to start and maintain."
%%
%% @doc     YXA event packages export an init/0 function.
%% @hidden
%% @end
%%--------------------------------------------------------------------
init() ->
    none.

%%--------------------------------------------------------------------
%% @spec    (PkgS::event_pkg(), Request, YxaCtx, SIPuser) ->
%%            void() "but return 'ok' or {error, Reason} for now"
%%
%%            Request = #request{} "the SUBSCRIBE request"
%%            YxaCtx  = #yxa_ctx{}
%%            Ctx     = #event_ctx{} "context information for request."
%%
%% @doc     YXA event packages must export a request/7 function. See
%%          the eventserver.erl module description for more
%%          information about when this function is invoked.
%% @end
%%--------------------------------------------------------------------

%% We do not perform authentication.

request(?EVENT_PKG, #request{method = "NOTIFY"} = Request, YxaCtx, Ctx) ->
    #yxa_ctx{app_logtag   = LogTag,
	     thandler = THandler
	    } = YxaCtx,

    #event_ctx{presentity = Presentity
	      } = Ctx,

    logger:log(normal, "~s: callingname event package: Processing NOTIFY ~s (presentity : ~p)",
	       [LogTag, sipurl:print(Request#request.uri), Presentity]),

    Response = parse_body(Request#request.body),

    case Response of
    {ok, Status, Party, Indicator} ->
        logger:log(normal, "~s: callingname event package: Calling-Name-Status ~p, Calling-Party ~p, Presentation-Indicator ~p",
    	       [LogTag, Status, Party, Indicator]),
        
	    transactionlayer:send_response_handler(THandler, 200, "Ok");
	{error, bad_format} ->
	    transactionlayer:send_response_handler(THandler, 400, "Invalid Response Format")
    end,
    Response;

request(?EVENT_PKG, _Request, YxaCtx, _Ctx) ->
    #yxa_ctx{logstr       = LogStr,
	    app_logtag   = LogTag,
	    thandler = THandler
	    } = YxaCtx,

    logger:log(normal, "~s: callingname event package: ~s -> '501 Not Implemented'",
	       [LogTag, LogStr]),
    transactionlayer:send_response_handler(THandler, 501, "Not Implemented"),
    ok.


%%--------------------------------------------------------------------
%% @spec    (PkgS::event_pkg(), Num, Request, YxaCtx, SIPuser,
%%          Presentity, PkgState) ->
%%            {error, need_auth} |
%%            {ok, SubState, Status, Reason, ExtraHeaders, NewPkgState}  |
%%            {siperror, Status, Reason, ExtraHeaders}
%%
%%            Num        = integer() "the number of subscribes we have received on this dialog, starts at 1"
%%            Request    = #request{} "the SUBSCRIBE request"
%%            YxaCtx     = #yxa_ctx{}
%%            SIPuser    = undefined | string() "undefined if request originator is not not authenticated, and string() if the user is authenticated (empty string if user could not be authenticated)"
%%            Presentity = {user, User}       |
%%                         {address, Address} |
%%                         undefined
%%            PkgState   = undefined | #my_state{}
%%
%%            SubState     = active | pending
%%            Status       = integer() "SIP status code to respond with"
%%            Reason       = string() "SIP reason phrase"
%%            ExtraHeaders = [{Key, ValueList}] "headers to include in the response to the SUBSCRIBE"
%%            Body         = binary() | list() "body of response"
%%            NewPkgState  = #my_state{}
%%
%% @doc     YXA event packages must export an is_allowed_subscribe/8
%%          function. This function is called when the event server
%%          receives a subscription request for this event package,
%%          and is the event packages chance to decide wether the
%%          subscription should be accepted or not. It is also called
%%          for every time the subscription is refreshed by the
%%          subscriber.
%% @end
%%--------------------------------------------------------------------
%%
%% SIPuser = undefined
%%
is_allowed_subscribe(?EVENT_PKG, _Num, _Request, YxaCtx, _SIPuser, _Presentity,
		     _PkgState) ->
     logger:log(normal, "~s: callingname event package: Subscribe not supported, answering '500 Not implemented'",
 	       [YxaCtx#yxa_ctx.app_logtag]),
     {siperror, 500, "Not implemented", []}.

%%--------------------------------------------------------------------
%% @spec    (PkgS::event_pkg(), Presentity, LastAccept, PkgState) ->
%%            {ok, Body, ExtraHeaders, NewPkgState} |
%%            {error, Reason}
%%
%%            Presentity = {users, UserList} | {address, AddressStr}
%%            UserList   = [string()] "SIP usernames"
%%            AddressStr = string() "parseable with sipurl:parse/1"
%%            LastAccept = [string()] "Accept: header value from last SUBSCRIBE"
%%            PkgState   = #my_state{}
%%
%%            Body         = io_list()
%%            ExtraHeaders = [{Key, ValueList}] "headers to include in the NOTIFY request"
%%            Reason       = string() | atom()
%%            NewPkgState  = #my_state{}
%%
%% @doc     YXA event packages must export a notify_content/3
%%          function. Whenever the subscription requires us to
%%          generate a NOTIFY request, this function is called to
%%          generate the body and extra headers to include in the
%%          NOTIFY request.
%% @end
%%--------------------------------------------------------------------
notify_content(?EVENT_PKG, _Presentity, _LastAccept, PkgState) when is_record(PkgState, my_state) ->
    {siperror, 500, "Not implemented", []}.


%%--------------------------------------------------------------------
%% @spec    package_parameters(PkgS::event_pkg(), Param) ->
%%            Value | undefined
%%
%%            Param = atom()
%%
%%            Value = term()
%%
%% @doc     YXA event packages must export a package_parameters/2
%%          function. 'undefined' MUST be returned for all unknown
%%          parameters.
%% @end
%%--------------------------------------------------------------------

%% @clear

%%--------------------------------------------------------------------
%% @spec    (PkgS::event_pkg(), notification_rate_limit) ->
%%            MilliSeconds
%%
%%            MilliSeconds = integer()
%%
%% @doc     The minimum amount of time that should pass between
%%          NOTIFYs we send about this event packages events.
%% @end
%%--------------------------------------------------------------------
package_parameters(?EVENT_PKG, notification_rate_limit) ->
    %% RFC4235 #3.10 (Rate of Notifications)
    1000;  %% 1000 milliseconds, 1 second

%%--------------------------------------------------------------------
%% @spec    (PkgS::event_pkg(), request_methods) ->
%%            Methods
%%
%%            Methods = [string()]
%%
%% @doc     What SIP methods this event packages request/7 function
%%          can handle.
%% @end
%%--------------------------------------------------------------------
package_parameters(?EVENT_PKG, request_methods) ->
    ["NOTIFY"];

%%--------------------------------------------------------------------
%% @spec    (PkgS::event_pkg(), subscribe_accept_content_types) ->
%%            ContentTypes
%%
%%            ContentTypes = [string()]
%%
%% @doc     What Content-Type encodings we should list as acceptable
%%          in the SUBSCRIBEs we send.
%% @end
%%--------------------------------------------------------------------

package_parameters(?EVENT_PKG, subscribe_accept_content_types) ->
    ["application/calling-name-info"];

package_parameters(?EVENT_PKG, _Param) ->
    undefined.


%%--------------------------------------------------------------------
%% @spec
%%    subscription_behaviour(PkgS::event_pkg(), Param, Argument) ->
%%            Value | undefined
%%
%%            Param    = atom()
%%            Argument = term() "depending on Param"
%%
%%            Value = term()
%%
%% @doc     YXA event packages must export a sbuscription_behaviour/2
%%          function. 'undefined' MUST be returned for all unknown
%%          parameters.
%% @end
%%--------------------------------------------------------------------

%% @clear

%%--------------------------------------------------------------------
%% @spec    (PkgS::event_pkg(), bidirectional_subscribe, Request) ->
%%            true
%%
%%            Request = #request{}
%%
%% @doc     When we receive a SUBSCRIBE, should the subscription
%%          handler also SUBSCRIBE to the other side in the same
%%          dialog? For the callingname package, this is always false.
%% @end
%%--------------------------------------------------------------------
subscription_behaviour(?EVENT_PKG, bidirectional_subscribe, Request) when is_record(Request, request) ->
    false;

subscription_behaviour(?EVENT_PKG, _Param, _Argument) ->
    undefined.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (Header) -> [string()]
%%
%%            Header = #keylist{}
%%
%% @doc     Get Accept: header value (or default) from a header.
%% @end
%%--------------------------------------------------------------------
get_accept(Header) ->
    case keylist:fetch('accept', Header) of
	[] ->
	    ["application/calling-name-info"];
	AcceptV ->
	    [string:to_lower(Elem) || Elem <- AcceptV]
    end.

%%--------------------------------------------------------------------
%% @spec    (Body) -> {ok,Status,Party,Indicator} | {error, bad_format}
%%
%%            Header = #keylist{}
%%
%% @doc     Get Accept: header value (or default) from a header.
%% @end
%%--------------------------------------------------------------------

%% Parse the body:
%% Calling-Name-Status: [available|unavailable|out-of-service]
%% Calling-Party: full SIP URI with name
%% Presentation-Indicator: [allowed|restricted|toggled|no indication]

%% All the parsing is already availaible in sippacket.erl, so
%% I simply exported parse_headers/2.
parse_body(Body) ->
    {Content, _} = parse_some_lines(Body),
    parse_body2(Content).

parse_body2(Content) when is_record(Content, keylist) ->
    logger:log(normal, "callingname event package: parse_body2 ~p)",
	       [Content]),
    Status    = keylist:fetch("Calling-Name-Status", Content),
    Party     = keylist:fetch("calling-name", Content),
    Indicator = keylist:fetch("presentation-indicator", Content),
    { ok, Status, Party, Indicator };
parse_body2(_) ->
    { error, bad_format }.


%%
%% Slight variant of sippacket:parse_headers/2 and /3

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(CR, 16#0D).
-define(LF, 16#0A).

parse_some_lines(Bin) ->
    parse_some_lines(Bin, 0, []).

parse_some_lines(Bin, Offset, Res) ->
    case Bin of
	<<_:Offset/binary, ?CR, ?LF, _/binary>> ->
	    %% CRLF, must be header-body separator. We are finished.
	    {sippacket:make_keylist(Bin, lists:reverse(Res)), Offset + 2};
	<<_:Offset/binary, N:8, _/binary>> when N == ?CR; N == ?LF ->
	    %% CR or LF, must be header-body separator (albeit broken). We are finished.
	    {sippacket:make_keylist(Bin, lists:reverse(Res)), Offset + 1};
	<<_:Offset/binary, _, _/binary>> ->
	    case sippacket:parse_one_header(Bin, Offset) of
	        {ok, This, NextOffset} ->
	            parse_some_lines(Bin, NextOffset, [This | Res]);
	        {error, no_end_of_header, This, NextOffset} ->
	            {sippacket:make_keylist(Bin, lists:reverse([This | Res])), NextOffset};
	        Error ->
                    logger:log(normal, "calling name event package: **** Parsing Error: ~p", [Error]),
        	    Error
	    end;
	_ ->
        {sippacket:make_keylist(Bin, lists:reverse(Res)), Offset}
    end.


%%====================================================================
%% Test functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    () -> ok
%%
%% @doc     autotest callback
%% @hidden
%% @end
%%--------------------------------------------------------------------
test() ->

    ok.
