%%%-------------------------------------------------------------------
%%% @author rishabh
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Jul 2020 11:47 AM
%%%-------------------------------------------------------------------
-author("rishabh").
%%% To maintain the list of registered users
-record(reg_user, {username, key}).

%%% Storing key of every topic
-record(topic_key,{topic_name, key}).

%%% Tuples for storing subscriber of each topic
-record(subscribers,{username, enc_address, node_flag }).