// Copyright 2013 Daniel Parker
// Distributed under the Boost license, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// See https://sourceforge.net/projects/jsoncons/files/ for latest version
// See https://sourceforge.net/p/jsoncons/wiki/Home/ for documentation.

#ifndef JSONCONS_JSON2_HPP
#define JSONCONS_JSON2_HPP

#include <cstdio>
#include <cstdlib>
#include <string>
#include <vector>
#include <exception>
#include <cstdlib>
#include <cstring>
#include <utility>
#include <algorithm>
#include <fstream>
#include <limits>
#include "jsoncons/json1.hpp"
#include "jsoncons/json_type_traits.hpp"
#include "jsoncons/json_structures.hpp"
#include "jsoncons/json_reader.hpp"
#include "jsoncons/json_deserializer.hpp"
#include "jsoncons/json_serializer.hpp"

#if defined(__GNUC__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wswitch"
#endif

namespace jsoncons {

template<typename Char, typename Alloc>
bool basic_json<Char, Alloc>::operator!=(const basic_json<Char, Alloc>& rhs) const
{
    return !(*this == rhs);
}

template<typename Char, typename Alloc>
bool basic_json<Char, Alloc>::operator==(const basic_json<Char, Alloc>& rhs) const
{
    return var_ == rhs.var_;
}

template<typename Char, typename Alloc>
basic_json<Char, Alloc>& basic_json<Char, Alloc>::at(size_t i)
{
    switch (var_.type_)
    {
    case value_types::array_t:
        return var_.value_.array_->at(i);
    default:
        JSONCONS_THROW_EXCEPTION("Index on non-array value not supported");
    }
}

template<typename Char, typename Alloc>
const basic_json<Char, Alloc>& basic_json<Char, Alloc>::at(size_t i) const
{
    switch (var_.type_)
    {
    case value_types::array_t:
        return var_.value_.array_->at(i);
    default:
        JSONCONS_THROW_EXCEPTION("Index on non-array value not supported");
    }
}

template<typename Char, typename Alloc>
basic_json<Char, Alloc>& basic_json<Char, Alloc>::at(const std::basic_string<Char>& name)
{
    switch (var_.type_)
    {
    case value_types::empty_object_t:
        JSONCONS_THROW_EXCEPTION_1("%s not found", name);
    case value_types::object_t:
        return var_.value_.object_->get(name);
    default:
        {
            JSONCONS_THROW_EXCEPTION_1("Attempting to get %s from a value that is not an object", name);
        }
    }
}

template<typename Char, typename Alloc>
const basic_json<Char, Alloc>& basic_json<Char, Alloc>::at(const std::basic_string<Char>& name) const
{
    switch (var_.type_)
    {
    case value_types::empty_object_t:
        JSONCONS_THROW_EXCEPTION_1("%s not found", name);
    case value_types::object_t:
        return var_.value_.object_->get(name);
    default:
        {
            JSONCONS_THROW_EXCEPTION_1("Attempting to get %s from a value that is not an object", name);
        }
    }
}

template<typename Char, typename Alloc>
const basic_json<Char, Alloc>& basic_json<Char, Alloc>::get(const std::basic_string<Char>& name) const
{
    static const basic_json<Char, Alloc> a_null = null_type();

    switch (var_.type_)
    {
    case value_types::empty_object_t:
        return a_null;
    case value_types::object_t:
        {
            const_object_iterator it = var_.value_.object_->find(name);
            return it != end_members() ? it->value() : a_null;
        }
    default:
        {
            JSONCONS_THROW_EXCEPTION_1("Attempting to get %s from a value that is not an object", name);
        }
    }
}

template<typename Char, typename Alloc>
template<typename T>
typename basic_json<Char, Alloc>::const_val_proxy basic_json<Char, Alloc>::get(const std::basic_string<Char>& name, T default_val) const
{
    switch (var_.type_)
    {
    case value_types::empty_object_t:
        {
            return const_val_proxy(default_val);
        }
    case value_types::object_t:
        {
            const_object_iterator it = var_.value_.object_->find(name);
            if (it != end_members())
            {
                return const_val_proxy(it->value());
            }
            else
            {
                return const_val_proxy(default_val);
            }
        }
    default:
        {
            JSONCONS_THROW_EXCEPTION_1("Attempting to get %s from a value that is not an object", name);
        }
    }
}

template<typename Char, typename Alloc>
void basic_json<Char, Alloc>::add(basic_json<Char, Alloc>&& value){
    switch (var_.type_){
    case value_types::array_t:
        var_.value_.array_->push_back(std::move(value));
        break;
    default:
        {
            JSONCONS_THROW_EXCEPTION("Attempting to insert into a value that is not an array");
        }
    }
}

template<typename Char, typename Alloc>
void basic_json<Char, Alloc>::add(size_t index, basic_json<Char, Alloc>&& value){
    switch (var_.type_){
    case value_types::array_t:
        var_.value_.array_->add(index, std::move(value));
        break;
    default:
        {
            JSONCONS_THROW_EXCEPTION("Attempting to insert into a value that is not an array");
        }
    }
}

template<typename Char, typename Alloc>
void basic_json<Char, Alloc>::clear()
{
    switch (var_.type_)
    {
    case value_types::array_t:
        var_.value_.array_->clear();
        break;
    case value_types::object_t:
        var_.value_.object_->clear();
        break;
    default:
        break;
    }
}

template<typename Char, typename Alloc>
void basic_json<Char, Alloc>::remove_range(size_t from_index, size_t to_index)
{
    switch (var_.type_)
    {
    case value_types::array_t:
        var_.value_.array_->remove_range(from_index, to_index);
        break;
    default:
        break;
    }
}

template<typename Char, typename Alloc>
void basic_json<Char, Alloc>::remove_member(const std::basic_string<Char>& name)
{
    switch (var_.type_)
    {
    case value_types::object_t:
        var_.value_.object_->remove(name);
        break;
    default:
        break;
    }
}

template<typename Char, typename Alloc>
void basic_json<Char, Alloc>::add(const basic_json<Char, Alloc>& value)
{
    switch (var_.type_)
    {
    case value_types::array_t:
        var_.value_.array_->push_back(value);
        break;
    default:
        {
            JSONCONS_THROW_EXCEPTION("Attempting to insert into a value that is not an array");
        }
    }
}

template<typename Char, typename Alloc>
void basic_json<Char, Alloc>::add(size_t index, const basic_json<Char, Alloc>& value)
{
    switch (var_.type_)
    {
    case value_types::array_t:
        var_.value_.array_->add(index, value);
        break;
    default:
        {
            JSONCONS_THROW_EXCEPTION("Attempting to insert into a value that is not an array");
        }
    }
}

template<typename Char, typename Alloc>
size_t basic_json<Char, Alloc>::size() const
{
    switch (var_.type_)
    {
    case value_types::empty_object_t:
        return 0;
    case value_types::object_t:
        return var_.value_.object_->size();
    case value_types::array_t:
        return var_.value_.array_->size();
    default:
        return 0;
    }
}

template<typename Char, typename Alloc>
typename basic_json<Char, Alloc>::object_key_proxy basic_json<Char, Alloc>::operator[](const std::basic_string<Char>& name)
{
    return object_key_proxy(*this, name);
}

template<typename Char, typename Alloc>
const basic_json<Char, Alloc>& basic_json<Char, Alloc>::operator[](const std::basic_string<Char>& name) const
{
    return at(name);
}

template<typename Char, typename Alloc>
basic_json<Char, Alloc>& basic_json<Char, Alloc>::operator[](size_t i)
{
    return at(i);
}

template<typename Char, typename Alloc>
const basic_json<Char, Alloc>& basic_json<Char, Alloc>::operator[](size_t i) const
{
    return at(i);
}

template<typename Char, typename Alloc>
std::basic_string<Char> basic_json<Char, Alloc>::to_string() const
{
    std::basic_ostringstream<Char> os;
    {
        basic_json_serializer<Char> serializer(os);
        to_stream(serializer);
    }
    return os.str();
}

template<typename Char, typename Alloc>
std::basic_string<Char> basic_json<Char, Alloc>::to_string(const basic_output_format<Char>& format) const
{
    std::basic_ostringstream<Char> os;
    {
        basic_json_serializer<Char> serializer(os, format);
        to_stream(serializer);
    }
    return os.str();
}

template<typename Char, typename Alloc>
void basic_json<Char, Alloc>::to_stream(basic_json_output_handler<Char>& handler) const
{
    switch (var_.type_)
    {
    case value_types::small_string_t:
        handler.value(var_.value_.small_string_value_,var_.small_string_length_);
        break;
    case value_types::string_t:
        handler.value(var_.value_.string_value_->c_str(),var_.value_.string_value_->length());
        break;
    case value_types::double_t:
        handler.value(var_.value_.float_value_);
        break;
    case value_types::integer_t:
        handler.value(var_.value_.integer_value_);
        break;
    case value_types::uinteger_t:
        handler.value(var_.value_.uinteger_value_);
        break;
    case value_types::bool_t:
        handler.value(var_.value_.bool_value_);
        break;
    case value_types::null_t:
        handler.value(null_type());
        break;
    case value_types::empty_object_t:
        handler.begin_object();
        handler.end_object();
        break;
    case value_types::object_t:
        {
            handler.begin_object();
            json_object<Char, Alloc> *o = var_.value_.object_;
            for (const_object_iterator it = o->begin(); it != o->end(); ++it)
            {
                handler.name((it->name()).c_str(),it->name().length());
                it->value().to_stream(handler);
            }
            handler.end_object();
        }
        break;
    case value_types::array_t:
        {
            handler.begin_array();
            json_array<Char, Alloc> *o = var_.value_.array_;
            for (const_array_iterator it = o->begin(); it != o->end(); ++it)
            {
                it->to_stream(handler);
            }
            handler.end_array();
        }
        break;
    case value_types::any_t:
        var_.value_.any_value_->to_stream(handler);
        break;
    default:
        // throw
        break;
    }
}

template<typename Char, typename Alloc>
void basic_json<Char, Alloc>::to_stream(std::basic_ostream<Char>& os) const
{
    basic_json_serializer<Char> serializer(os);
    to_stream(serializer);
}

template<typename Char, typename Alloc>
void basic_json<Char, Alloc>::to_stream(std::basic_ostream<Char>& os, const basic_output_format<Char>& format) const
{
    basic_json_serializer<Char> serializer(os, format);
    to_stream(serializer);
}

template<typename Char, typename Alloc>
void basic_json<Char, Alloc>::to_stream(std::basic_ostream<Char>& os, const basic_output_format<Char>& format, bool indenting) const
{
    basic_json_serializer<Char> serializer(os, format, indenting);
    to_stream(serializer);
}


// Deprecated static data members
template<typename Char, typename Alloc>
const basic_json<Char, Alloc> basic_json<Char, Alloc>::an_object = basic_json<Char, Alloc>(value_types::object_t,0);
template<typename Char, typename Alloc>
const basic_json<Char, Alloc> basic_json<Char, Alloc>::an_array = basic_json<Char, Alloc>(value_types::array_t,0);        
template<typename Char, typename Alloc>
const basic_json<Char, Alloc> basic_json<Char, Alloc>::null = basic_json<Char, Alloc>(jsoncons::null_type());

template<typename Char, typename Alloc>
basic_json<Char, Alloc> basic_json<Char, Alloc>::make_2d_array(size_t m, size_t n)
{
    basic_json<Char, Alloc> a(basic_json<Char, Alloc>(json_array<Char,Alloc>()));
    a.resize(m);
    for (size_t i = 0; i < a.size(); ++i)
    {
        a[i] = basic_json<Char, Alloc>::make_array(n);
    }
    return a;
}

template<typename Char, typename Alloc>
template<typename T>
basic_json<Char, Alloc> basic_json<Char, Alloc>::make_2d_array(size_t m, size_t n, T val)
{
    basic_json<Char, Alloc> v;
    v = val;
    basic_json<Char, Alloc> a = make_array(m);
    for (size_t i = 0; i < a.size(); ++i)
    {
        a[i] = basic_json<Char, Alloc>::make_array(n, v);
    }
    return a;
}

template<typename Char, typename Alloc>
basic_json<Char, Alloc> basic_json<Char, Alloc>::make_3d_array(size_t m, size_t n, size_t k)
{
    basic_json<Char, Alloc> a(basic_json<Char, Alloc>(json_array<Char,Alloc>()));
    a.resize(m);
    for (size_t i = 0; i < a.size(); ++i)
    {
        a[i] = basic_json<Char, Alloc>::make_2d_array(n, k);
    }
    return a;
}

template<typename Char, typename Alloc>
template<typename T>
basic_json<Char, Alloc> basic_json<Char, Alloc>::make_3d_array(size_t m, size_t n, size_t k, T val)
{
    basic_json<Char, Alloc> v;
    v = val;
    basic_json<Char, Alloc> a = make_array(m);
    for (size_t i = 0; i < a.size(); ++i)
    {
        a[i] = basic_json<Char, Alloc>::make_2d_array(n, k, v);
    }
    return a;
}

template<typename Char, typename Alloc>
basic_json<Char, Alloc> basic_json<Char, Alloc>::parse(std::basic_istream<Char>& is)
{
    basic_json_deserializer<Char, Alloc> handler;
    basic_json_reader<Char> reader(is, handler);
    reader.read_next();
    reader.check_done();
    if (!handler.is_valid())
    {
        JSONCONS_THROW_EXCEPTION("Failed to parse json stream");
    }
    return handler.get_result();
}

template<typename Char, typename Alloc>
basic_json<Char, Alloc> basic_json<Char, Alloc>::parse(std::basic_istream<Char>& is, 
                                                       basic_parse_error_handler<Char>& err_handler)
{
    basic_json_deserializer<Char, Alloc> handler;
    basic_json_reader<Char> reader(is, handler, err_handler);
    reader.read_next();
    reader.check_done();
    if (!handler.is_valid())
    {
        JSONCONS_THROW_EXCEPTION("Failed to parse json stream");
    }
    return handler.get_result();
}

template<typename Char, typename Alloc>
basic_json<Char, Alloc> basic_json<Char, Alloc>::parse(const std::basic_string<Char>& s)
{
    basic_json_deserializer<Char, Alloc> handler;
    basic_json_parser<Char> parser(handler);
    parser.begin_parse();
    parser.parse(s.c_str(),0,s.length());
    parser.end_parse();
    parser.check_done(s.c_str(),parser.index(),s.length());
    if (!handler.is_valid())
    {
        JSONCONS_THROW_EXCEPTION("Failed to parse json string");
    }
    return handler.get_result();
}

template<typename Char, typename Alloc>
basic_json<Char, Alloc> basic_json<Char, Alloc>::parse(const std::basic_string<Char>& s, 
                                                       basic_parse_error_handler<Char>& err_handler)
{
    basic_json_deserializer<Char, Alloc> handler;
    basic_json_parser<Char> parser(handler,err_handler);
    parser.begin_parse();
    parser.parse(s.c_str(),0,s.length());
    parser.end_parse();
    parser.check_done(s.c_str(),parser.index(),s.length());
    if (!handler.is_valid())
    {
        JSONCONS_THROW_EXCEPTION("Failed to parse json string");
    }
    return handler.get_result();
}

template<typename Char, typename Alloc>
basic_json<Char, Alloc> basic_json<Char, Alloc>::parse_string(const std::basic_string<Char>& s)
{
    basic_json_deserializer<Char, Alloc> handler;
    basic_json_parser<Char> parser(handler);
    parser.begin_parse();
    parser.parse(s.c_str(),0,s.length());
    parser.end_parse();
    parser.check_done(s.c_str(),parser.index(),s.length());
    if (!handler.is_valid())
    {
        JSONCONS_THROW_EXCEPTION("Failed to parse json string");
    }
    return handler.get_result();
}

template<typename Char, typename Alloc>
basic_json<Char, Alloc> basic_json<Char, Alloc>::parse_string(const std::basic_string<Char>& s, 
                                                              basic_parse_error_handler<Char>& err_handler)
{
    basic_json_deserializer<Char, Alloc> handler;
    basic_json_parser<Char> parser(handler,err_handler);
    parser.begin_parse();
    parser.parse(s.c_str(),0,s.length());
    parser.end_parse();
    parser.check_done(s.c_str(),parser.index(),s.length());
    if (!handler.is_valid())
    {
        JSONCONS_THROW_EXCEPTION("Failed to parse json string");
    }
    return handler.get_result();
}

template<typename Char, typename Alloc>
basic_json<Char, Alloc> basic_json<Char, Alloc>::parse_file(const std::string& filename)
{
    FILE* fp = std::fopen (filename.c_str(), "rb");
    if (fp == nullptr) 
    {
        throw json_exception_1<char>("Cannot open file %s", filename);
    }
    basic_json_deserializer<Char, Alloc> handler;
    try
    {
        // obtain file size:
        std::fseek (fp , 0 , SEEK_END);
        long size = std::ftell (fp);
        std::rewind(fp);

        if (size > 0)
        {
            std::vector<Char> buffer(size);

            // copy the file into the buffer:
            size_t result = std::fread (buffer.data(),1,size,fp);
            if (result != static_cast<unsigned long long>(size))
            {
                throw json_exception_1<char>("Error reading file %s", filename);
            }

            basic_json_parser<Char> parser(handler);
            parser.begin_parse();
            parser.parse(buffer.data(),0,buffer.size());
            parser.end_parse();
            parser.check_done(buffer.data(),parser.index(),buffer.size());
        }

        std::fclose (fp);
    }
    catch (...)
    {
        std::fclose (fp);
        throw;
    }
    if (!handler.is_valid())
    {
        JSONCONS_THROW_EXCEPTION("Failed to parse json file");
    }
    return handler.get_result();
}

template<typename Char, typename Alloc>
basic_json<Char, Alloc> basic_json<Char, Alloc>::parse_file(const std::string& filename, 
                                                            basic_parse_error_handler<Char>& err_handler)
{
    FILE* fp = std::fopen (filename.c_str(), "rb");
    if (fp == nullptr) 
    {
        throw json_exception_1<char>("Cannot open file %s", filename);
    }
    basic_json_deserializer<Char, Alloc> handler;
    try
    {
        // obtain file size:
        std::fseek (fp , 0 , SEEK_END);
        long size = std::ftell (fp);
        std::rewind(fp);

        if (size > 0)
        {
            std::vector<Char> buffer(size);

            // copy the file into the buffer:
            size_t result = std::fread (buffer.data(),1,size,fp);
            if (result != static_cast<unsigned long long>(size))
            {
                throw json_exception_1<char>("Error reading file %s", filename);
            }

            basic_json_parser<Char> parser(handler,err_handler);
            parser.begin_parse();
            parser.parse(buffer.data(),0,buffer.size());
            parser.end_parse();
            parser.check_done(buffer.data(),parser.index(),buffer.size());
        }

        std::fclose (fp);
    }
    catch (...)
    {
        std::fclose (fp);
        throw;
    }
    if (!handler.is_valid())
    {
        JSONCONS_THROW_EXCEPTION("Failed to parse json file");
    }
    return handler.get_result();
}

template<typename Char, typename Alloc>
typename basic_json<Char, Alloc>::object_iterator basic_json<Char, Alloc>::begin_members()
{
    switch (var_.type_)
    {
    case value_types::empty_object_t:
        var_.type_ = value_types::object_t;
        var_.value_.object_ = new json_object<Char, Alloc>();
    case value_types::object_t:
        return var_.value_.object_->begin();
    default:
        JSONCONS_THROW_EXCEPTION("Not an object");
    }
}

template<typename Char, typename Alloc>
typename basic_json<Char, Alloc>::const_object_iterator basic_json<Char, Alloc>::begin_members() const
{
    switch (var_.type_)
    {
    case value_types::empty_object_t:
        return cobject().begin_members();
    case value_types::object_t:
        return var_.value_.object_->begin();
    default:
        JSONCONS_THROW_EXCEPTION("Not an object");
    }
}

template<typename Char, typename Alloc>
typename basic_json<Char, Alloc>::object_iterator basic_json<Char, Alloc>::end_members()
{
    switch (var_.type_)
    {
    case value_types::empty_object_t:
        var_.type_ = value_types::object_t;
        var_.value_.object_ = new json_object<Char, Alloc>();
    case value_types::object_t:
        return var_.value_.object_->end();
    default:
        JSONCONS_THROW_EXCEPTION("Not an object");
    }
}

template<typename Char, typename Alloc>
typename basic_json<Char, Alloc>::const_object_iterator basic_json<Char, Alloc>::end_members() const
{
    switch (var_.type_)
    {
    case value_types::empty_object_t:
        return cobject().end_members();
    case value_types::object_t:
        return var_.value_.object_->end();
    default:
        JSONCONS_THROW_EXCEPTION("Not an object");
    }
}

template<typename Char, typename Alloc>
typename basic_json<Char, Alloc>::array_iterator basic_json<Char, Alloc>::begin_elements()
{
    switch (var_.type_)
    {
    case value_types::array_t:
        return var_.value_.array_->begin();
    default:
        JSONCONS_THROW_EXCEPTION("Not an array");
    }
}

template<typename Char, typename Alloc>
typename basic_json<Char, Alloc>::const_array_iterator basic_json<Char, Alloc>::begin_elements() const
{
    switch (var_.type_)
    {
    case value_types::array_t:
        return var_.value_.array_->begin();
    default:
        JSONCONS_THROW_EXCEPTION("Not an array");
    }
}

template<typename Char, typename Alloc>
typename basic_json<Char, Alloc>::array_iterator basic_json<Char, Alloc>::end_elements()
{
    switch (var_.type_)
    {
    case value_types::array_t:
        return var_.value_.array_->end();
    default:
        JSONCONS_THROW_EXCEPTION("Not an array");
    }
}

template<typename Char, typename Alloc>
typename basic_json<Char, Alloc>::const_array_iterator basic_json<Char, Alloc>::end_elements() const
{
    switch (var_.type_)
    {
    case value_types::array_t:
        return var_.value_.array_->end();
    default:
        JSONCONS_THROW_EXCEPTION("Not an array");
    }
}

template<typename Char, typename Alloc>
bool basic_json<Char, Alloc>::empty() const
{
    return var_.empty();
}

template<typename Char, typename Alloc>
void basic_json<Char, Alloc>::reserve(size_t n)
{
    switch (var_.type_)
    {
    case value_types::array_t:
        var_.value_.array_->reserve(n);
        break;
    case value_types::empty_object_t:
        var_.type_ = value_types::object_t;
        var_.value_.object_ = new json_object<Char, Alloc>();
    case value_types::object_t:
        var_.value_.object_->reserve(n);
        break;
    }
}

template<typename Char, typename Alloc>
void basic_json<Char, Alloc>::resize(size_t n)
{
    switch (var_.type_)
    {
    case value_types::array_t:
        var_.value_.array_->resize(n);
        break;
    }
}

template<typename Char, typename Alloc>
template<typename T>
void basic_json<Char, Alloc>::resize(size_t n, T val)
{
    switch (var_.type_)
    {
    case value_types::array_t:
        var_.value_.array_->resize(n, val);
        break;
    }
}

template<typename Char, typename Alloc>
size_t basic_json<Char, Alloc>::capacity() const
{
    switch (var_.type_)
    {
    case value_types::array_t:
        return var_.value_.array_->capacity();
    case value_types::object_t:
        return var_.value_.object_->capacity();
    default:
        return 0;
    }
}

template<typename Char, typename Alloc>
bool basic_json<Char, Alloc>::has_member(const std::basic_string<Char>& name) const
{
    switch (var_.type_)
    {
    case value_types::object_t:
        {
            const_object_iterator it = var_.value_.object_->find(name);
            return it != end_members();
        }
        break;
    default:
        return false;
    }
}

template<typename Char, typename Alloc>
long long basic_json<Char, Alloc>::as_longlong() const
{
    switch (var_.type_)
    {
    case value_types::double_t:
        return static_cast<long long>(var_.value_.float_value_);
    case value_types::integer_t:
        return static_cast<long long>(var_.value_.integer_value_);
    case value_types::uinteger_t:
        return static_cast<long long>(var_.value_.uinteger_value_);
    case value_types::bool_t:
        return var_.value_.bool_value_ ? 1 : 0;
    default:
        JSONCONS_THROW_EXCEPTION("Not a long long");
    }
}

template<typename Char, typename Alloc>
unsigned long long basic_json<Char, Alloc>::as_ulonglong() const
{
    switch (var_.type_)
    {
    case value_types::double_t:
        return static_cast<unsigned long long>(var_.value_.float_value_);
    case value_types::integer_t:
        return static_cast<unsigned long long>(var_.value_.integer_value_);
    case value_types::uinteger_t:
        return static_cast<unsigned long long>(var_.value_.uinteger_value_);
    case value_types::bool_t:
        return var_.value_.bool_value_ ? 1 : 0;
    default:
        JSONCONS_THROW_EXCEPTION("Not a unsigned long long");
    }
}

template<typename Char, typename Alloc>
double basic_json<Char, Alloc>::as_double() const
{
    switch (var_.type_)
    {
    case value_types::double_t:
        return var_.value_.float_value_;
    case value_types::integer_t:
        return static_cast<double>(var_.value_.integer_value_);
    case value_types::uinteger_t:
        return static_cast<double>(var_.value_.uinteger_value_);
    case value_types::null_t:
        return std::numeric_limits<double>::quiet_NaN();
    default:
        JSONCONS_THROW_EXCEPTION("Not a double");
    }
}

template<typename Char, typename Alloc>
int basic_json<Char, Alloc>::as_int() const
{
    switch (var_.type_)
    {
    case value_types::double_t:
        return static_cast<int>(var_.value_.float_value_);
    case value_types::integer_t:
        return static_cast<int>(var_.value_.integer_value_);
    case value_types::uinteger_t:
        return static_cast<int>(var_.value_.uinteger_value_);
    case value_types::bool_t:
        return var_.value_.bool_value_ ? 1 : 0;
    default:
        JSONCONS_THROW_EXCEPTION("Not a int");
    }
}

template<typename Char, typename Alloc>
unsigned int basic_json<Char, Alloc>::as_uint() const
{
    switch (var_.type_)
    {
    case value_types::double_t:
        return static_cast<unsigned int>(var_.value_.float_value_);
    case value_types::integer_t:
        return static_cast<unsigned int>(var_.value_.integer_value_);
    case value_types::uinteger_t:
        return static_cast<unsigned int>(var_.value_.uinteger_value_);
    case value_types::bool_t:
        return var_.value_.bool_value_ ? 1 : 0;
    default:
        JSONCONS_THROW_EXCEPTION("Not a unsigned int");
    }
}

template<typename Char, typename Alloc>
long basic_json<Char, Alloc>::as_long() const
{
    switch (var_.type_)
    {
    case value_types::double_t:
        return static_cast<long>(var_.value_.float_value_);
    case value_types::integer_t:
        return static_cast<long>(var_.value_.integer_value_);
    case value_types::uinteger_t:
        return static_cast<long>(var_.value_.uinteger_value_);
    case value_types::bool_t:
        return var_.value_.bool_value_ ? 1 : 0;
    default:
        JSONCONS_THROW_EXCEPTION("Not a long");
    }
}

template<typename Char, typename Alloc>
unsigned long basic_json<Char, Alloc>::as_ulong() const
{
    switch (var_.type_)
    {
    case value_types::double_t:
        return static_cast<unsigned long>(var_.value_.float_value_);
    case value_types::integer_t:
        return static_cast<unsigned long>(var_.value_.integer_value_);
    case value_types::uinteger_t:
        return static_cast<unsigned long>(var_.value_.uinteger_value_);
    case value_types::bool_t:
        return var_.value_.bool_value_ ? 1 : 0;
    default:
        JSONCONS_THROW_EXCEPTION("Not an unsigned long");
    }
}

template<typename Char, typename Alloc>
typename basic_json<Char, Alloc>::any& basic_json<Char, Alloc>::any_value()
{
    switch (var_.type_)
    {
    case value_types::any_t:
        {
			return *var_.value_.any_value_;
        }
    default:
        JSONCONS_THROW_EXCEPTION("Not an any value");
    }
}

template<typename Char, typename Alloc>
const typename basic_json<Char, Alloc>::any& basic_json<Char, Alloc>::any_value() const
{
    switch (var_.type_)
    {
    case value_types::any_t:
        {
			return *var_.value_.any_value_;
        }
    default:
        JSONCONS_THROW_EXCEPTION("Not an any value");
    }
}

template<typename Char, typename Alloc>
std::basic_string<Char> basic_json<Char, Alloc>::as_string() const
{
    switch (var_.type_)
    {
    case value_types::small_string_t:
        return std::basic_string<Char>(var_.value_.small_string_value_,var_.small_string_length_);
    case value_types::string_t:
        return std::basic_string<Char>(var_.value_.string_value_->c_str(),var_.value_.string_value_->length());
    default:
        return to_string();
    }
}

template<typename Char, typename Alloc>
const Char* basic_json<Char, Alloc>::as_cstring() const
{
    switch (var_.type_)
    {
    case value_types::small_string_t:
        return var_.value_.small_string_value_;
    case value_types::string_t:
        return var_.value_.string_value_->c_str();
    default:
        JSONCONS_THROW_EXCEPTION("Not a string");
    }
}

template<typename Char, typename Alloc>
std::basic_string<Char> basic_json<Char, Alloc>::as_string(const basic_output_format<Char>& format) const
{
    switch (var_.type_)
    {
    case value_types::small_string_t:
        return std::basic_string<Char>(var_.value_.small_string_value_,var_.small_string_length_);
    case value_types::string_t:
        return std::basic_string<Char>(var_.value_.string_value_->c_str(),var_.value_.string_value_->length());
    default:
        return to_string(format);
    }
}

template <typename Char,typename Alloc>
std::basic_ostream<Char>& operator<<(std::basic_ostream<Char>& os, const basic_json<Char, Alloc>& o)
{
    o.to_stream(os);
    return os;
}

template <typename Char, typename Alloc>
std::basic_istream<Char>& operator>>(std::basic_istream<Char>& is, basic_json<Char, Alloc>& o)
{
    basic_json_deserializer<Char, Alloc> handler;
    basic_json_reader<Char> reader(is, handler);
    reader.read_next();
    reader.check_done();
    if (!handler.is_valid())
    {
        JSONCONS_THROW_EXCEPTION("Failed to parse json stream");
    }
    o = handler.get_result();
    return is;
}

template<typename Char, typename Alloc>
class json_printable
{
public:
    json_printable(const basic_json<Char, Alloc>& o,
                   bool is_pretty_print)
       : o_(&o), is_pretty_print_(is_pretty_print)
    {
    }

    json_printable(const basic_json<Char, Alloc>& o,
                   bool is_pretty_print,
                   const basic_output_format<Char>& format)
       : o_(&o), is_pretty_print_(is_pretty_print), format_(format)
    {
        ;
    }

    void to_stream(std::basic_ostream<Char>& os) const
    {
        o_->to_stream(os, format_, is_pretty_print_);
    }

    friend std::basic_ostream<Char>& operator<<(std::basic_ostream<Char>& os, const json_printable<Char, Alloc>& o)
    {
        o.to_stream(os);
        return os;
    }

    const basic_json<Char, Alloc> *o_;
    bool is_pretty_print_;
    basic_output_format<Char> format_;
private:
    json_printable();
};

template<typename Char, class Alloc>
json_printable<Char, Alloc> print(const basic_json<Char, Alloc>& val)
{
    return json_printable<Char, Alloc>(val,false);
}

template<typename Char, class Alloc>
json_printable<Char, Alloc> print(const basic_json<Char, Alloc>& val,
                                  const basic_output_format<Char>& format)
{
    return json_printable<Char, Alloc>(val, false, format);
}

template<typename Char, class Alloc>
json_printable<Char, Alloc> pretty_print(const basic_json<Char, Alloc>& val)
{
    return json_printable<Char, Alloc>(val,true);
}

template<typename Char, class Alloc>
json_printable<Char, Alloc> pretty_print(const basic_json<Char, Alloc>& val,
                                         const basic_output_format<Char>& format)
{
    return json_printable<Char, Alloc>(val, true, format);
}

}

#if defined(__GNUC__)
#pragma GCC diagnostic pop
#endif

#endif
