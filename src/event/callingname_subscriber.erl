%%%-------------------------------------------------------------------
%%% File    : callingname_subscriber.erl
%%% @author   Stephane Alnet <stephane@shimaore.net>
%%% @doc      Query Verisign for CNAM data.
%%%
%%% @since    22 January 2009 by Stephane Alnet <stephane@shimaore.net>
%%% @end
%%%-------------------------------------------------------------------
-module(callingname_subscriber).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------

-export([
    start/4
    ]).

-include("siprecords.hrl").
-include("event.hrl").

%%
%% Notifier   : string()
%% Subscriber : string()
%% Number     : string()
%% Timeout    : integer()
%% for example:
%%   callingname_subscriber:start("cnam-notifier.com","cnam-subscriber.com","9876543212",100).
%%

start(Notifier,Subscriber,Number,Timeout) ->
    From_URL = "sip:" ++ Subscriber,
    To_URL   = "sip:" ++ Notifier,
    Contact = From_URL,

    URI = sipurl:parse(To_URL),

    [From] = contact:parse([From_URL]),
    [To] = contact:parse([To_URL]),
    [ParsedContact] = contact:parse([Contact]),

    Presenter = {address, ParsedContact#contact.urlstr},
    Interval = 0,
    Accept = callingname_package:package_parameters("calling-name", subscribe_accept_content_types),
    ContentType = "application/calling-name-info",
    ExtraHeaders = [{"Contact", [Contact]},{"Content-Type", [ContentType]}],
    [Dst | _] = sipdst:url_to_dstlist(URI, 500, URI),
    
    Body = list_to_binary(["Calling-Party: sip:", Number, "@", Subscriber, ";user=phone"]),

    {ok,Pid} = active_subscriber:start(
        callingname_package, "calling-name", Presenter, URI, From, To, Dst, Interval, Accept,
	    ExtraHeaders, "", "calling name", self(), undefined, Body),
 
    %% This is the response sent by active_subscriber:handle_cast({received_notify,...}).
	Result = receive Response ->
	    Response
    after Timeout ->
        {error, timeout}
	end,

	case Result of
	    {ok, [Status], Party, [Indicator]} ->
	        [ParsedParty] = contact:parse(Party),
	        { ok, [Status], ParsedParty#contact.display_name, [Indicator] };
	    _ -> 
	        Result
	end.
