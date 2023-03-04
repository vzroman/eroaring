/*----------------------------------------------------------------
* Copyright (c) 2023 vzroman@gmail.com
*
* This file is provided to you under the Apache License,
* Version 2.0 (the "License"); you may not use this file
* except in compliance with the License.  You may obtain
* a copy of the License at
*
*   http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing,
* software distributed under the License is distributed on an
* "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
* KIND, either express or implied.  See the License for the
* specific language governing permissions and limitations
* under the License.
----------------------------------------------------------------*/

#include "eroaring.h"

#define ERL_NIF_REGULAR_BOUND 0

static ErlNifFunc nif_funcs[] = {
    {"from_list", 1, eroaring::from_list, ERL_NIF_REGULAR_BOUND},
    {"to_list", 1, eroaring::to_list, ERL_NIF_REGULAR_BOUND},
    {"intersection", 2, eroaring::intersection, ERL_NIF_REGULAR_BOUND},
    {"union", 2, eroaring::union_sets, ERL_NIF_REGULAR_BOUND},
    {"subtract", 2, eroaring::subtract, ERL_NIF_REGULAR_BOUND},
    {"add_elements", 2, eroaring::add_elements, ERL_NIF_REGULAR_BOUND},
    {"remove_elements", 2, eroaring::remove_elements, ERL_NIF_REGULAR_BOUND},
    {"contains", 2, eroaring::contains, ERL_NIF_REGULAR_BOUND},
    {"count", 1, eroaring::count, ERL_NIF_REGULAR_BOUND},
};

namespace eroaring {
    ERL_NIF_TERM ATOM_TRUE;
    ERL_NIF_TERM ATOM_FALSE;
}

static void on_unload(ErlNifEnv * /*env*/, void * /*priv_data*/){
}

static int on_upgrade(ErlNifEnv* /*env*/, void** priv_data, void** old_priv_data, ERL_NIF_TERM /*load_info*/){
    /* Convert the private data to the new version. */
    *priv_data = *old_priv_data;
    return 0;
}

static int on_load(ErlNifEnv* env, void** /*priv_data*/, ERL_NIF_TERM /*load_info*/){

    eroaring::ATOM_TRUE = enif_make_atom(env, "true");
    eroaring::ATOM_FALSE = enif_make_atom(env, "false");

    return 0;
}

extern "C" {
    ERL_NIF_INIT(eroaring, nif_funcs, &on_load, NULL, &on_upgrade, &on_unload)
}