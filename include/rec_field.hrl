%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2025
%%% @doc
%%% rec_field_data数据头文件
%%% @end
%%% Created : 2025-8-1 00:00:00
%%%-------------------------------------------------------------------

-ifndef(REC_FIELD_HRL).
-define(REC_FIELD_HRL, true).

%% 记录字段信息
-record(rec_field_info, {
    name                     :: atom()                     %% 名称
    , pos                    :: pos_integer()              %% 位置
    , bin_name               :: binary()                   %% 二进制名称
    , type                   :: term()                     %% 类型
    , default                :: term()                     %% 默认值
}).

-endif.