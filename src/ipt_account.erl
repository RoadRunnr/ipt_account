%% Copyright 2015, Travelping GmbH <info@travelping.com>

%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.

-module(ipt_account).

-export([init/1, deinit/1,
	 read_entries/3, handle_usage/1,
	 tables_names/1, free_all_handles/1]).

-include_lib("gen_socket/include/gen_socket.hrl").

-record(ipt_context, {
	  socket     :: gen_socket:socket()
}).

-define(SO_ACCOUNT_BASE_CTL, 70).

-define(IPT_SO_SET_ACCOUNT_HANDLE_FREE,        ?SO_ACCOUNT_BASE_CTL + 1).
-define(IPT_SO_SET_ACCOUNT_HANDLE_FREE_ALL,    ?SO_ACCOUNT_BASE_CTL + 2).

-define(IPT_SO_GET_ACCOUNT_PREPARE_READ,       ?SO_ACCOUNT_BASE_CTL + 4).
-define(IPT_SO_GET_ACCOUNT_PREPARE_READ_FLUSH, ?SO_ACCOUNT_BASE_CTL + 5).
-define(IPT_SO_GET_ACCOUNT_GET_DATA,           ?SO_ACCOUNT_BASE_CTL + 6).
-define(IPT_SO_GET_ACCOUNT_GET_HANDLE_USAGE,   ?SO_ACCOUNT_BASE_CTL + 7).
-define(IPT_SO_GET_ACCOUNT_GET_TABLE_NAMES,    ?SO_ACCOUNT_BASE_CTL + 8).

-define(IPT_ACCOUNT_MIN_BUFSIZE, 4096).
-define(ACCOUNT_TABLE_NAME_LEN, 32).
-define(IPT_ACC_HANDLE_SOCKOPT_LEN, ?ACCOUNT_TABLE_NAME_LEN + 8).
-define(IPT_ACC_IP_LEN, 40).

%% ====================================================================
%% API
%% ====================================================================

init(Opts) ->
    case socket(inet, raw, raw, Opts) of
	{ok, Socket} ->
	    {ok, #ipt_context{socket = Socket}};
	Other ->
	    Other
    end.

deinit(#ipt_context{socket = Socket}) ->
    gen_socket:close(Socket).

round_up(Value, Block) ->
    ((Value + Block - 1) div Block) * Block.

read_entries(Name, Flush, #ipt_context{socket = Socket})
  when (is_binary(Name) orelse is_list(Name))
       andalso is_boolean(Flush) ->

    Cmd = if Flush -> ?IPT_SO_GET_ACCOUNT_PREPARE_READ_FLUSH;
	     true  -> ?IPT_SO_GET_ACCOUNT_PREPARE_READ
	  end,
    Handle0 = handle_sockopt(-1, Name, 0),
    case gen_socket:getsockopt(Socket, ip, Cmd, Handle0) of
	Handle1 when is_binary(Handle1) ->
	    Ret = case handle_sockopt(Handle1) of
		      {_, _, Count} when Count > 0 ->
			  %% BufferSize is (Count + 10%) * ?IPT_ACC_IP_LEN rounded up the next multiple of IPT_ACCOUNT_MIN_BUFSIZE
			  OptLen = max(?IPT_ACCOUNT_MIN_BUFSIZE, round_up((Count + Count div 10) * ?IPT_ACC_IP_LEN, ?IPT_ACCOUNT_MIN_BUFSIZE)),
			  case gen_socket:getsockopt(Socket, ip, ?IPT_SO_GET_ACCOUNT_GET_DATA, Handle1, OptLen) of
			      Data when is_binary(Data) ->
				  split_accounting(Data, Count, []);
			      Other ->
				  Other
			  end;
		      _ ->
			  []
		  end,
	    free_handle(Socket, Handle1),
	    Ret;

	Other ->
	    Other
    end.

handle_usage(#ipt_context{socket = Socket}) ->
    case gen_socket:getsockopt(Socket, ip, ?IPT_SO_GET_ACCOUNT_GET_HANDLE_USAGE, ?IPT_ACC_HANDLE_SOCKOPT_LEN) of
	Handle when is_binary(Handle) ->
	    {_, _, Count} = handle_sockopt(Handle),
	    {ok, Count};

	Other ->
	    Other
    end.

tables_names(#ipt_context{socket = Socket}) ->
    case gen_socket:getsockopt(Socket, ip, ?IPT_SO_GET_ACCOUNT_GET_TABLE_NAMES, ?IPT_ACCOUNT_MIN_BUFSIZE) of
	Data when is_binary(Data) ->
	    [D0|_] = binary:split(Data, <<0,0>>),
	    {ok, binary:split(D0, <<0>>, [global, trim])};

	Other ->
	    Other
    end.

free_all_handles(#ipt_context{socket = Socket}) ->
    gen_socket:setsockopt(Socket, ip, ?IPT_SO_SET_ACCOUNT_HANDLE_FREE_ALL, <<>>).

%% ====================================================================
%% internal helpers
%% ====================================================================

socket(Family, Type, Protocol, Opts) ->
    case proplists:get_value(netns, Opts) of
	undefined ->
	    gen_socket:socket(Family, Type, Protocol);
	NetNs ->
	    gen_socket:socketat(NetNs, Family, Type, Protocol)
    end.

free_handle(Socket, Handle) ->
    gen_socket:setsockopt(Socket, ip, ?IPT_SO_SET_ACCOUNT_HANDLE_FREE, Handle).

to_binary(V) when is_binary(V) -> V;
to_binary(V) when is_list(V)   -> iolist_to_binary(V).

%%
%% pad binary to specific length
%%   -> http://www.erlang.org/pipermail/erlang-questions/2008-December/040709.html
%%
pad_to(Width, Binary) ->
     case (Width - size(Binary) rem Width) rem Width of
         0 -> Binary;
         N -> <<Binary/binary, 0:(N*8)>>
     end.

handle_sockopt(Nr, Name, Count) ->
    BinName = pad_to(?ACCOUNT_TABLE_NAME_LEN, to_binary(Name)),
    <<Nr:32/native-integer, BinName/binary, Count:32/native-integer>>.

handle_sockopt(<<Nr:32/native-integer, BinName:?ACCOUNT_TABLE_NAME_LEN/binary, Count:32/native-integer>>) ->
    {Nr, BinName, Count}.

split_accounting(_Data, 0, Acc) ->
    lists:reverse(Acc);
split_accounting(<<D:8, C:8, B:8, A:8, _:32,
		   SrcPackets:64/native-integer, SrcBytes:64/native-integer,
		   DstPackets:64/native-integer, DstBytes:64/native-integer, Rest/binary>>, Cnt, Acc) ->
    split_accounting(Rest, Cnt - 1, [{{A,B,C,D}, {SrcPackets, SrcBytes}, {DstPackets, DstBytes}}|Acc]).
