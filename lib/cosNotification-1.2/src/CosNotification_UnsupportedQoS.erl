%%  coding: latin-1
%%------------------------------------------------------------
%%
%% Implementation stub file
%% 
%% Target: CosNotification_UnsupportedQoS
%% Source: /home/vagrant/build-dir_15-12-18_12-20-50/otp-support/lib/cosNotification/src/CosNotification.idl
%% IC vsn: 4.4
%% 
%% This file is automatically generated. DO NOT EDIT IT.
%%
%%------------------------------------------------------------

-module('CosNotification_UnsupportedQoS').
-ic_compiled("4_4").


-include("CosNotification.hrl").

-export([tc/0,id/0,name/0]).



%% returns type code
tc() -> {tk_except,"IDL:omg.org/CosNotification/UnsupportedQoS:1.0",
            "UnsupportedQoS",
            [{"qos_err",
              {tk_sequence,
                  {tk_struct,"IDL:omg.org/CosNotification/PropertyError:1.0",
                      "PropertyError",
                      [{"code",
                        {tk_enum,
                            "IDL:omg.org/CosNotification/QoSError_code:1.0",
                            "QoSError_code",
                            ["UNSUPPORTED_PROPERTY","UNAVAILABLE_PROPERTY",
                             "UNSUPPORTED_VALUE","UNAVAILABLE_VALUE",
                             "BAD_PROPERTY","BAD_TYPE","BAD_VALUE"]}},
                       {"name",{tk_string,0}},
                       {"available_range",
                        {tk_struct,
                            "IDL:omg.org/CosNotification/PropertyRange:1.0",
                            "PropertyRange",
                            [{"low_val",tk_any},{"high_val",tk_any}]}}]},
                  0}}]}.

%% returns id
id() -> "IDL:omg.org/CosNotification/UnsupportedQoS:1.0".

%% returns name
name() -> "CosNotification_UnsupportedQoS".


