%%%-------------------------------------------------------------------
%%% @author Ben Adams
%%% @copyright (C) 2020, MessageMap.io
%%% @doc
%%% Module will be used for creating encryption system
%%% @end
%%% Created : 21. Dec 2020
%%%-------------------------------------------------------------------
-module(mcrypto).
-include_lib("public_key/include/public_key.hrl").
-export([encrypt/2]).
-export([validation/2]).

-define(charSet, " abcdefghijklmnopqrstuvwxyz1234567890QWERTYUIOPASDFGHJKLZXCVBNM,./;[]~'\|`!@#$%^&*(_-=+)").
-define(sortedDataSet, lists:sort([[X] || X <- ?charSet])).
-define(maxChars, length(?charSet)).

encrypt(StringToEncrypt, Salt) ->
  Offset = rand:uniform(?maxChars),
  Steps = rand:uniform(?maxChars),
  {Head, Tail} = lists:split(Steps, ?sortedDataSet),
  StartPosition = lists:append(Tail, Head),
  StartFlip = rand:uniform(2),
  UpdateSalt = Salt++numstr(rand:uniform(99999999)),
  EncryptedString = runEncoding(StringToEncrypt, StartPosition, UpdateSalt, StartFlip, 100, Offset, "", StartFlip),
  "Map"++numstr(Steps)++"$"++numstr(Offset)++numstr(StartFlip)++"$"++EncryptedString.

validation(StringToEncrypt, Hash) ->
  [ Start, OffSetLong | FullEncryption ] = string:tokens(Hash, "$"),
  Offset = list_to_integer(lists:sublist(OffSetLong, 1, length(OffSetLong) - 1)),
  Steps = list_to_integer(lists:sublist(Start, 4, 2)),
  {Head, Tail} = lists:split(Steps, ?sortedDataSet),
  StartPosition = lists:append(Tail, Head),
  StartFlip = list_to_integer(lists:sublist(OffSetLong, length(OffSetLong), 1)),
  ListFullEncryption = [[X] || X <- lists:flatten(FullEncryption)],
  SplitIndex = string:str(ListFullEncryption, ["."]),
  { EncSalt , _ } = lists:split(SplitIndex-1, ListFullEncryption),
  SaltEncode = base64:decode(base64:decode(list_to_binary(lists:reverse([[X] || X <- EncSalt])))),
  EncryptedString = runEncoding(StringToEncrypt, StartPosition, SaltEncode, StartFlip, 100, Offset, "", StartFlip),
  CheckHash = "Map"++numstr(Steps)++"$"++numstr(Offset)++numstr(StartFlip)++"$"++EncryptedString,
  (CheckHash == Hash).

% Internal Functions
runEncoding(Password, _, Salt, _, 0, _, _, _) ->
  SaltEncode = lists:reverse(binary_to_list(base64:encode(base64:encode(Salt)))),
  SaltEncode++"."++numstr(length(SaltEncode))++"."++Password;
runEncoding(Password, StartPosition, Salt, StartFlip, Rounds, Offset, "", StartFlip) ->
  SplitPassword = [[X] || X <- Password],
  Newpass = encoding(SplitPassword, StartPosition, Salt, StartFlip, Offset, "", StartFlip),
  runEncoding(Newpass, StartPosition, Salt, StartFlip, (Rounds-1), Offset,  "", StartFlip).

encoding([], _, _, _, _, Encoded, _) ->
  Encoded;
encoding(SplitPassword, Dictonary, Salt, Flip, Offset, Encoded, StartFlip) ->
  PassChar = lists:nth(1, SplitPassword),
  IndexChar = string:str(Dictonary, [PassChar]),
  { Head, Tail } = lists:split(IndexChar, Dictonary),
  NewDictonary = generateNewDictonary(Head, Tail, Flip),
  EncodePass = lists:nth(Offset, NewDictonary),
  { NewHead, NewTail } = lists:split(string:str(NewDictonary, [EncodePass]), NewDictonary),
  [ _ | NewPassword ] = SplitPassword,
  Newflip = round(1/Flip*2),
  encoding(NewPassword, lists:append(NewTail,NewHead), Salt, Newflip, Offset, Encoded++EncodePass, StartFlip).

generateNewDictonary(Head, Tail, 1) ->
  lists:append(Tail, Head);
generateNewDictonary(Head, Tail, _) ->
  lists:reverse(lists:append(Tail, Head)).

numstr(Number) ->
  lists:flatten(io_lib:format("~p", [Number])).
