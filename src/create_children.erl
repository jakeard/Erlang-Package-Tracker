-module(create_children).
-export([create/0]).

create() ->
        add_c(),
        update_c(),
        get_c().

add_c() ->
        package_sup:add_child(add_package,add_rr,add1,worker),
        package_sup:add_child(add_package,add_rr,add2,worker),
        package_sup:add_child(add_package,add_rr,add3,worker).

update_c() ->
        package_sup:add_child(update_package,update_rr,update1,worker),
        package_sup:add_child(update_package,update_rr,update2,worker),
        package_sup:add_child(update_package,update_rr,update3,worker).

get_c() ->
        package_sup:add_child(query_package,get_rr,get1,worker),
        package_sup:add_child(query_package,get_rr,get2,worker),
        package_sup:add_child(query_package,get_rr,get3,worker).