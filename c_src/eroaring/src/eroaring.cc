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

ERL_NIF_TERM from_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    if(!(argc == 1 && enif_is_list(env, argv[0]))) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM head, tail = argv[0];
    uint32_t length;
    enif_get_list_length(env, tail, &length);

    uint64_t elements[length];
    uint32_t i=0;

    try {
        while(enif_get_list_cell(env, tail, &head, &tail)) {
            if(!enif_get_uint64(env, head, &elements[i++]))
            throw;
        }
    } catch (...){
        return enif_make_badarg(env);
    }    

    Roaring64Map r(length, elements);

    uint32_t size = r.getSizeInBytes();
    ERL_NIF_TERM bin;
    unsigned char *buf = enif_make_new_binary(env, size, &bin);
    r.write((char *)buf);
    return bin;
}

ERL_NIF_TERM to_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    ErlNifBinary value;

    if(!(argc == 1 && enif_inspect_binary(env, argv[0], &value ))) {
        return enif_make_badarg(env);
    }

    Roaring64Map r;
    try {
        r = Roaring64Map::read((const char*)value.data );
    } catch (...){
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM val;
    ERL_NIF_TERM result = enif_make_list(env, 0);

    uint64_t length = r.cardinality();

    if(length > 0) {
        uint64_t *values = (uint64_t *)enif_alloc(length * sizeof(uint64_t));
        if (!values) {
            return enif_make_badarg(env);
        }

        r.toUint64Array(values);

        do {
            val = enif_make_uint64(env, values[--length]);
            result = enif_make_list_cell(env, val, result);
        } while( length >0 );
        enif_free( values );
    }
    return result;

}

ERL_NIF_TERM intersection(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    ErlNifBinary value1, value2;

    if(!(argc == 2 && enif_inspect_binary(env, argv[0], &value1 ) && enif_inspect_binary(env, argv[1], &value2 ))) {
        return enif_make_badarg(env);
    }

    Roaring64Map result;
    try {
        Roaring64Map r1 = Roaring64Map::read((const char*)value1.data );
        Roaring64Map r2 = Roaring64Map::read((const char*)value2.data );

        result = r1 & r2;

    } catch (...){
        return enif_make_badarg(env);
    }

    uint32_t size = result.getSizeInBytes();
    ERL_NIF_TERM bin;
    unsigned char *buf = enif_make_new_binary(env, size, &bin);
    result.write((char *)buf);
    return bin;
}

ERL_NIF_TERM union_sets(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    ErlNifBinary value1, value2;

    if(!(argc == 2 && enif_inspect_binary(env, argv[0], &value1 ) && enif_inspect_binary(env, argv[1], &value2 ))) {
        return enif_make_badarg(env);
    }

    Roaring64Map result;
    try {
        Roaring64Map r1 = Roaring64Map::read((const char*)value1.data );
        Roaring64Map r2 = Roaring64Map::read((const char*)value2.data );

        result = r1 | r2;

    } catch (...){
        return enif_make_badarg(env);
    }

    uint32_t size = result.getSizeInBytes();
    ERL_NIF_TERM bin;
    unsigned char *buf = enif_make_new_binary(env, size, &bin);
    result.write((char *)buf);
    return bin;
}

ERL_NIF_TERM subtract(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    ErlNifBinary value1, value2;

    if(!(argc == 2 && enif_inspect_binary(env, argv[0], &value1 ) && enif_inspect_binary(env, argv[1], &value2 ))) {
        return enif_make_badarg(env);
    }

    Roaring64Map result;
    try {
        Roaring64Map r1 = Roaring64Map::read((const char*)value1.data );
        Roaring64Map r2 = Roaring64Map::read((const char*)value2.data );

        result = r1 - r2;

    } catch (...){
        return enif_make_badarg(env);
    }

    uint32_t size = result.getSizeInBytes();
    ERL_NIF_TERM bin;
    unsigned char *buf = enif_make_new_binary(env, size, &bin);
    result.write((char *)buf);
    return bin;
}

ERL_NIF_TERM add_elements(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    ErlNifBinary value;
    if(!(argc == 2 && enif_inspect_binary(env, argv[0], &value ) && enif_is_list(env, argv[1]))) {
        return enif_make_badarg(env);
    }

    Roaring64Map r;

    ERL_NIF_TERM head, tail = argv[1];
    uint32_t length;
    enif_get_list_length(env, tail, &length);

    uint64_t elements[length];
    uint32_t i=0;

    try {
        r = Roaring64Map::read((const char*)value.data );
        while(enif_get_list_cell(env, tail, &head, &tail)) {
            if(!enif_get_uint64(env, head, &elements[i++]))
            throw;
        }
    } catch (...){
        return enif_make_badarg(env);
    }    

    r.addMany(length, elements);

    uint32_t size = r.getSizeInBytes();
    ERL_NIF_TERM bin;
    unsigned char *buf = enif_make_new_binary(env, size, &bin);
    r.write((char *)buf);
    return bin;
}

ERL_NIF_TERM remove_elements(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    ErlNifBinary value;
    if(!(argc == 2 && enif_inspect_binary(env, argv[0], &value ) && enif_is_list(env, argv[1]))) {
        return enif_make_badarg(env);
    }

    Roaring64Map r;

    ERL_NIF_TERM head, tail = argv[1];

    uint64_t e;

    try {
        r = Roaring64Map::read((const char*)value.data );
        while(enif_get_list_cell(env, tail, &head, &tail)) {
            if(!enif_get_uint64(env, head, &e))
            throw;
            r.remove( e );
        }
    } catch (...){
        return enif_make_badarg(env);
    }    

    uint32_t size = r.getSizeInBytes();
    ERL_NIF_TERM bin;
    unsigned char *buf = enif_make_new_binary(env, size, &bin);
    r.write((char *)buf);
    return bin;
}

ERL_NIF_TERM contains(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    ErlNifBinary value;
    uint64_t e;

    if(!(argc == 2 && enif_inspect_binary(env, argv[0], &value ) && enif_get_uint64(env, argv[1], &e))) {
        return enif_make_badarg(env);
    }

    Roaring64Map r;
    try {
        r = Roaring64Map::read((const char*)value.data );
    } catch (...){
        return enif_make_badarg(env);
    }

    if (r.contains( e )){
        return eroaring::ATOM_TRUE;
    }else{
        return eroaring::ATOM_FALSE;
    }

}

ERL_NIF_TERM count(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    ErlNifBinary value;

    if(!(argc == 1 && enif_inspect_binary(env, argv[0], &value ))) {
        return enif_make_badarg(env);
    }

    Roaring64Map r;
    try {
        r = Roaring64Map::read((const char*)value.data );
    } catch (...){
        return enif_make_badarg(env);
    }

    uint64_t length = r.cardinality();

    return enif_make_uint64(env, length);
}

}