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
#include "roaring.hh"

using namespace roaring;

namespace eroaring {

ERL_NIF_TERM Create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
    Roaring r;
    uint32_t size = r.getSizeInBytes();
    ERL_NIF_TERM bin;
    unsigned char *buf = enif_make_new_binary(env, size, &bin);
    r.write((char *)buf);
    return bin;
}

ERL_NIF_TERM FromList(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    if(!(argc == 1 && enif_is_list(env, argv[0]))) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM head, tail = argv[0];
    uint32_t length;
    enif_get_list_length(env, tail, &length);

    uint32_t values[length];
    uint32_t i=0;

    try {
        while(enif_get_list_cell(env, tail, &head, &tail)) {
            if(!enif_get_uint(env, head, &values[i++]))
            throw;
        }
    } catch (...){
        return enif_make_badarg(env);
    }    

    Roaring r(length, values);
    
    uint32_t size = r.getSizeInBytes();
    ERL_NIF_TERM bin;
    unsigned char *buf = enif_make_new_binary(env, size, &bin);
    r.write((char *)buf);
    return bin;

    err:
        return enif_make_badarg(env);
}

}