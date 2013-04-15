%% protocol defines

-define(CT_TAG, 16#494d5631).
-define(BIN_PRO_VER_1_0, 16#00010000).
-define(LUINT, little-unsigned-integer).
-define(UINT32, 32/little-unsigned-integer).
-define(UINT8, 8/little-unsigned-integer).

-define(RANDOM_KEY_SEED_LEN, 16).

% 连接状态
% 会话第一步(握手)
-define(CT_FLAG_CON_S1, 0).
% 会话第二步(握手)
-define(CT_FLAG_CON_S2, 1).
% 会话第三步(握手)
-define(CT_FLAG_CON_S3, 2).
% 会话第四步(握手)
-define(CT_FLAG_CON_S4, 3).
% 心跳包
-define(CT_FLAG_KEEPALIVE, 5).
% 会话进入正常流程
-define(CT_FLAG_CON_OK, 7).

%% MSG_TYPE_UNKNOWN = 0,
%% MSG_TYPE_SINGLE = 1,//单人
%% MSG_TYPE_GROUP  = 2,//群
%% MSG_TYPE_MCHAT  = 3 //多人
