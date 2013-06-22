// Copyright 2013 Daniel Parker
// Distributed under Boost license

#ifndef JSONCONS_JSON2_HPP
#define JSONCONS_JSON2_HPP

#include <string>
#include <vector>
#include <assert.h> 
#include <exception>
#include <cstdlib>
#include <cstring>
#include <utility>
#include <algorithm>
#include <fstream>
#include <limits>
#include "jsoncons/json1.hpp"
#include "jsoncons/json_structures.hpp"
#include "jsoncons/json_parser.hpp"
#include "jsoncons/json_stream_listener.hpp"
#include "jsoncons/json_serializer.hpp"

namespace jsoncons {

// proxy
template <class Char>
basic_json<Char>::proxy::proxy(basic_json<Char>& var, const std::basic_string<Char>& name)
    : val_(var), name_(name)
{
}

template <class Char>
typename basic_json<Char>::proxy& basic_json<Char>::proxy::operator=(const basic_json& val)
{
    val_.set_member(name_, val);
    return *this;
}


template <class Char>
basic_json<Char>::basic_json()
{
    type_ = null_t;
}

template <class Char>
template <class InputIterator>
basic_json<Char>::basic_json(InputIterator first, InputIterator last)
{
    type_ = array_t;
    value_.array_ = new json_array<Char>(first,last);
}

template <class Char>
basic_json<Char>::basic_json(const basic_json<Char>& val)
{
    type_ = val.type_;
    switch (type_)
    {
    case double_t:
    case longlong_t:
    case ulonglong_t:
    case bool_t:
    case null_t:
        value_ = val.value_;
        break;
    case string_t:
        value_.string_value_.length_ = val.value_.string_value_.length_;
        if (value_.string_value_.length_ > 0)
        {
            value_.string_value_.data_ = new Char[value_.string_value_.length_];
            std::memcpy(value_.string_value_.data_,val.value_.string_value_.data_,value_.string_value_.length_*sizeof(Char));
        }
        else
        {
            value_.string_value_.data_ = nullptr;
        }
        break;
    case array_t:
        value_.array_ = val.value_.array_->clone();
        break;
    case object_t:
        value_.object_ = val.value_.object_->clone();
        break;
    default:
        // throw
		break;
    }
}

template <class Char>
basic_json<Char>::basic_json(basic_json&& other)
{
    type_ = other.type_;
    value_ = other.value_;
    other.type_ = null_t;
}

template <class Char>
basic_json<Char>::basic_json(json_object<Char>* var)
{
    type_ = object_t;
    value_.object_ = var;
}

template <class Char>
basic_json<Char>::basic_json(json_array<Char>* var)
{
    type_ = array_t;
    value_.array_ = var;
}

template <class Char>
basic_json<Char>::basic_json(double val)
{
    type_ = double_t;
    value_.double_value_ = val;
}

template <class Char>
basic_json<Char>::basic_json(long long val)
{
    type_ = longlong_t;
    value_.longlong_value_ = val;
}

template <class Char>
basic_json<Char>::basic_json(int val)
{
    type_ = longlong_t;
    value_.longlong_value_ = val;
}

template <class Char>
basic_json<Char>::basic_json(unsigned int val)
{
    type_ = longlong_t;
    value_.longlong_value_ = val;
}

template <class Char>
basic_json<Char>::basic_json(long val)
{
    type_ = longlong_t;
    value_.longlong_value_ = val;
}

template <class Char>
basic_json<Char>::basic_json(unsigned long val)
{
    type_ = longlong_t;
    value_.longlong_value_ = val;
}

template <class Char>
basic_json<Char>::basic_json(unsigned long long val)
{
    type_ = ulonglong_t;
    value_.ulonglong_value_ = val;
}

template <class Char>
basic_json<Char>::basic_json(bool val)
{
    type_ = bool_t;
    value_.bool_value_ = val;
}

template <class Char>
basic_json<Char>::basic_json(const std::basic_string<Char>& s)
{
    type_ = string_t;
    value_.string_value_.length_ = s.length();
    if (s.length() > 0)
    {
        value_.string_value_.data_ = new Char[s.length()];
        std::memcpy(value_.string_value_.data_,&s[0],s.length()*sizeof(Char));
    }
    else
    {
        value_.string_value_.data_ = nullptr;
    }
}

template <class Char>
basic_json<Char>::basic_json(const Char* s)
{
    type_ = string_t;
    size_t len = json_char_traits<Char>::cstring_len(s);
    value_.string_value_.length_ = len;
    if (len > 0)
    {
        value_.string_value_.data_ = new Char[len];
        std::memcpy(value_.string_value_.data_,s,len*sizeof(Char));
    }
    else
    {
        value_.string_value_.data_ = nullptr;
    }
}

template <class Char>
basic_json<Char>::~basic_json()
{
    switch (type_)
    {
    case double_t:
    case longlong_t:
    case ulonglong_t:
    case bool_t:
    case null_t:
        break;
    case string_t:
        delete value_.string_value_.data_;
        break;
    case array_t:
        delete value_.array_;
        break;
    case object_t:
        delete value_.object_;
        break;
    }
}

template <class Char>
basic_json<Char>& basic_json<Char>::operator=(basic_json<Char> rhs)
{
    rhs.swap(*this);
    return *this;
}

template <class Char>
void basic_json<Char>::swap(basic_json<Char>& o) throw()
{
    std::swap(type_,o.type_);
    std::swap(value_,o.value_);
}

template <class Char>
basic_json<Char>& basic_json<Char>::at(size_t i) 
{
    switch (type_)
    {
    case object_t:
        return value_.object_->at(i);
    case array_t:
        return value_.array_->at(i);
    default:
        JSONCONS_THROW_EXCEPTION("Not an array or object");
    }
}

template <class Char>
const basic_json<Char>& basic_json<Char>::at(size_t i) const
{
    switch (type_)
    {
    case object_t:
        return value_.object_->at(i);
    case array_t:
        return value_.array_->at(i);
    default:
        JSONCONS_THROW_EXCEPTION("Not an array or object");
    }
}

template <class Char>
basic_json<Char>& basic_json<Char>::get(const std::basic_string<Char>& name) 
{
    switch (type_)
    {
    case object_t:
        return value_.object_->get(name);
    default:
        {
            JSONCONS_THROW_EXCEPTION_1("Attempting to get %s from a value that is not an object", name);
        }
    }
}

template <class Char>
const basic_json<Char>& basic_json<Char>::get(const std::basic_string<Char>& name) const
{
    switch (type_)
    {
    case object_t:
        return value_.object_->get(name);
    default:
        {
            std::ostringstream os;
            os << "Attempting to get " << name << " from a value that is not an object"; 
            JSONCONS_THROW_EXCEPTION(os.str());
        }
    }
}

template <class Char>
basic_json<Char>& basic_json<Char>::get(const std::basic_string<Char>& name, basic_json<Char>& default_val) 
{
    switch (type_)
    {
    case object_t:
        return has_member(name) ? value_.object_->get(name) : default_val;
    default:
        {
            std::ostringstream os;
            os << "Attempting to get " << name << " from a value that is not an object"; 
            JSONCONS_THROW_EXCEPTION(os.str());
        }
    }
}

template <class Char>
const basic_json<Char>& basic_json<Char>::get(const std::basic_string<Char>& name, const basic_json<Char>& default_val) const
{
    switch (type_)
    {
    case object_t:
        return has_member(name) ? value_.object_->get(name) : default_val;
    default:
        {
            std::ostringstream os;
            os << "Attempting to get " << name << " from a value that is not an object"; 
            JSONCONS_THROW_EXCEPTION(os.str());
        }
    }
}

template <class Char>
void basic_json<Char>::set_member(const std::basic_string<Char>& name, const basic_json<Char>& value)
{
    switch (type_)
    {
    case object_t:
        value_.object_->set_member(name,value);
        break;
    default:
        {
            std::ostringstream os;
            os << "Attempting to set " << name << " on a value that is not an object"; 
            JSONCONS_THROW_EXCEPTION(os.str());
        }
    }
}

template <class Char>
void basic_json<Char>::set_member(std::basic_string<Char>&& name, basic_json<Char>&& value)
{
    switch (type_)
    {
    case object_t:
        value_.object_->set_member(name,value);
        break;
    default:
        {
            std::ostringstream os;
            os << "Attempting to set " << name << " on a value that is not an object"; 
            JSONCONS_THROW_EXCEPTION(os.str());
        }
    }
}

template <class Char>
void basic_json<Char>::push_back(const basic_json<Char>& value)
{
    switch (type_)
    {
    case array_t:
        value_.array_->push_back(value);
        break;
    default:
        {
            std::ostringstream os;
            os << "Attempting to insert into a value that is not an array"; 
            JSONCONS_THROW_EXCEPTION(os.str());
        }
    }
}

template <class Char>
void basic_json<Char>::push_back(basic_json<Char>&& value)
{
    switch (type_)
    {
    case array_t:
        value_.array_->push_back(value);
        break;
    default:
        {
            std::ostringstream os;
            os << "Attempting to insert into a value that is not an array"; 
            JSONCONS_THROW_EXCEPTION(os.str());
        }
    }
}

template <class Char>
size_t basic_json<Char>::size() const
{
    switch (type_)
    {
    case object_t:
        return value_.object_->size();
    case array_t:
        return value_.array_->size();
    default:
        return 0;
    }
}

template <class Char>
typename basic_json<Char>::proxy basic_json<Char>::operator[](const std::basic_string<Char>& name)
{
    return proxy(*this,name);
}

template <class Char>
const typename basic_json<Char>::proxy basic_json<Char>::operator[](const std::basic_string<Char>& name) const
{
    return proxy(*this,name);
}

template <class Char>
basic_json<Char>& basic_json<Char>::operator[](size_t i)
{
    return value_.array_->at(i);
}

template <class Char>
const basic_json<Char>& basic_json<Char>::operator[](size_t i) const
{
    return value_.array_->at(i);
}

template <class Char>
std::basic_string<Char> basic_json<Char>::to_string() const
{
    std::basic_ostringstream<Char> os;
    to_stream(os);
    return os.str();
}

template <class Char>
template <class Serializer>
void basic_json<Char>::serialize(Serializer& serializer) const
{
    switch (type_)
    {
    case string_t:
        serializer.value(value_.string_value_.data_,value_.string_value_.length_);
        break;
    case double_t:
        serializer.value(value_.double_value_);
        break;
    case longlong_t:
        serializer.value(value_.longlong_value_);
        break;
    case ulonglong_t:
        serializer.value(value_.ulonglong_value_);
        break;
    case bool_t:
        serializer.value(value_.bool_value_);
        break;
    case null_t:
        serializer.null();
        break;
    case object_t:
		{
        serializer.begin_object();
        json_object<Char>* o = value_.object_;
        for (auto it = o->begin(); it != o->end(); ++it)
        {
            serializer.begin_member(&(it->name()[0]),it->name().length());
            it->value().serialize(serializer);
            serializer.end_member();
        }
        serializer.end_object();
		}
        break;
    case array_t:
		{
        serializer.begin_array();
        json_array<Char>* o = value_.array_;
        for (auto it = o->begin(); it != o->end(); ++it)
        {
            serializer.begin_element();
            it->serialize(serializer);
            serializer.end_element();
        }
        serializer.end_array();
		}
        break;
    default:
        // throw
        break;
    }
}

template <class Char>
void basic_json<Char>::to_stream(std::basic_ostream<Char>& os) const
{
    serialize(basic_json_stream_writer<Char>(os)); 
}

template <class Char>
void basic_json<Char>::to_stream(std::basic_ostream<Char>& os, const basic_output_format<Char>& format) const
{
    serialize(basic_json_stream_writer<Char>(os,format));
}

template <class Char>
const basic_json<Char> basic_json<Char>::an_object(new json_object<Char>());

template <class Char>
const basic_json<Char> basic_json<Char>::an_array(new json_array<Char>());

template <class Char>
const basic_json<Char> basic_json<Char>::null = basic_json<Char>();

template <class Char> 
basic_json<Char> basic_json<Char>::parse(std::basic_istream<Char>& is)
{
    basic_json_parser<Char> parser(is);
    basic_json_stream_listener<Char> handler;
    parser.parse(handler);
    basic_json<Char> val;
    handler.swap_root(val);
    return val;
}

template <class Char>
basic_json<Char> basic_json<Char>::parse_string(const std::basic_string<Char>& s)
{
    std::basic_istringstream<Char> is(s);
    basic_json_parser<Char> parser(is);
    basic_json_stream_listener<Char> handler;
    parser.parse(handler);
    basic_json<Char> val;
    handler.swap_root(val);
    return val;
}

template <class Char>
basic_json<Char> basic_json<Char>::parse_file(const std::string& filename)
{
    std::basic_ifstream<Char> is(filename.c_str(), std::basic_ifstream<Char>::in | std::basic_ifstream<Char>::binary);
    if (!is.is_open())
    {
        std::ostringstream ss;
        ss << "Cannot open file " << filename;
        JSONCONS_THROW_EXCEPTION(ss.str());
    }
    is.seekg(0,std::ios_base::end);
    std::ios::pos_type pos = is.tellg();
    is.seekg(0,std::ios_base::beg);

    size_t length = static_cast<size_t>(pos);

    if (length == 0)
    {
        std::ostringstream ss;
        ss << "File " << filename << " is empty";
        JSONCONS_THROW_EXCEPTION(ss.str());
    }

    std::string buffer;
    buffer.resize(length);
    is.read(&buffer[0],length);
    std::istringstream sstr(buffer);

    basic_json_parser<Char> parser(sstr);
    basic_json_stream_listener<Char> handler;
    parser.parse(handler);
    basic_json<Char> val;
    handler.swap_root(val);
    return val;
}

template <class Char>
typename basic_json<Char>::object_iterator basic_json<Char>::begin_members()
{
    switch (type_)
    {
    case object_t:
        return value_.object_->begin();
    default:
        JSONCONS_THROW_EXCEPTION("Not an object");
    }
}

template <class Char>
typename basic_json<Char>::const_object_iterator basic_json<Char>::begin_members() const
{
    switch (type_)
    {
    case object_t:
        return value_.object_->begin();
    default:
        JSONCONS_THROW_EXCEPTION("Not an object");
    }
}

template <class Char>
typename basic_json<Char>::object_iterator basic_json<Char>::end_members()
{
    switch (type_)
    {
    case object_t:
        return value_.object_->end();
    default:
        JSONCONS_THROW_EXCEPTION("Not an object");
    }
}

template <class Char>
typename basic_json<Char>::const_object_iterator basic_json<Char>::end_members() const
{
    switch (type_)
    {
    case object_t:
        return value_.object_->end();
    default:
        JSONCONS_THROW_EXCEPTION("Not an object");
    }
}

template <class Char>
typename basic_json<Char>::array_iterator basic_json<Char>::begin_elements()
{
    switch (type_)
    {
    case array_t:
        return value_.array_->begin();
    default:
        JSONCONS_THROW_EXCEPTION("Not an array");
    }
}

template <class Char>
typename basic_json<Char>::const_array_iterator basic_json<Char>::begin_elements() const
{
    switch (type_)
    {
    case array_t:
        return value_.array_->begin();
    default:
        JSONCONS_THROW_EXCEPTION("Not an array");
    }
}

template <class Char>
typename basic_json<Char>::array_iterator basic_json<Char>::end_elements()
{
    switch (type_)
    {
    case array_t:
        return value_.array_->end();
    default:
        JSONCONS_THROW_EXCEPTION("Not an array");
    }
}

template <class Char>
typename basic_json<Char>::const_array_iterator basic_json<Char>::end_elements() const
{
    switch (type_)
    {
    case array_t:
        return value_.array_->end();
    default:
        JSONCONS_THROW_EXCEPTION("Not an array");
    }
}

template <class Char>
double basic_json<Char>::as_double() const
{
    switch (type_)
    {
    case double_t:
        return value_.double_value_;
    case longlong_t:
        return static_cast<double>(value_.longlong_value_);
    case ulonglong_t:
        return static_cast<double>(value_.ulonglong_value_);
    case null_t:
        return std::numeric_limits<double>::quiet_NaN();;
    default:
        JSONCONS_THROW_EXCEPTION("Not a double");
    }
}

template <class Char>
bool basic_json<Char>::is_empty() const
{
    switch (type_)
    {
    case array_t:
        return value_.array_->size() == 0;
    case object_t:
        return value_.object_->size() == 0;
    default:
        return false;
    }
}

template <class Char>
bool basic_json<Char>::has_member(const std::basic_string<Char>& name) const
{
    switch (type_)
    {
    case object_t:
        {
            const_object_iterator it = value_.object_->find(name);
            return it != end_members();
        }
        break;
    default:
        return false;
    }
}

template <class Char>
bool basic_json<Char>::as_bool() const
{
    switch (type_)
    {
    case bool_t:
        return value_.bool_value_;
    default:
        JSONCONS_THROW_EXCEPTION("Not a int");
    }
}

template <class Char>
int basic_json<Char>::as_int() const
{
    switch (type_)
    {
    case double_t:
        return static_cast<int>(value_.double_value_);
    case longlong_t:
        return static_cast<int>(value_.longlong_value_);
    case ulonglong_t:
        return static_cast<int>(value_.ulonglong_value_);
    case bool_t:
        return value_.bool_value_ ? 1 : 0;
    default:
        JSONCONS_THROW_EXCEPTION("Not a int");
    }
}

template <class Char>
unsigned int basic_json<Char>::as_uint() const
{
    switch (type_)
    {
    case double_t:
        return static_cast<unsigned int>(value_.double_value_);
    case longlong_t:
        return static_cast<unsigned int>(value_.longlong_value_);
    case ulonglong_t:
        return static_cast<unsigned int>(value_.ulonglong_value_);
    case bool_t:
        return value_.bool_value_ ? 1 : 0;
    default:
        JSONCONS_THROW_EXCEPTION("Not a unsigned int");
    }
}

template <class Char>
long long basic_json<Char>::as_longlong() const
{
    switch (type_)
    {
    case double_t:
        return static_cast<long long>(value_.double_value_);
    case longlong_t:
        return static_cast<long long>(value_.longlong_value_);
    case ulonglong_t:
        return static_cast<long long>(value_.ulonglong_value_);
    case bool_t:
        return value_.bool_value_ ? 1 : 0;
    default:
        JSONCONS_THROW_EXCEPTION("Not a long long");
    }
}

template <class Char>
unsigned long long basic_json<Char>::as_ulonglong() const
{
    switch (type_)
    {
    case double_t:
        return static_cast<unsigned long long>(value_.double_value_);
    case longlong_t:
        return static_cast<unsigned long long>(value_.longlong_value_);
    case ulonglong_t:
        return static_cast<unsigned long long>(value_.ulonglong_value_);
    case bool_t:
        return value_.bool_value_ ? 1 : 0;
    default:
        JSONCONS_THROW_EXCEPTION("Not a unsigned long long");
    }
}

template <class Char>
std::basic_string<Char> basic_json<Char>::as_string() const
{
    switch (type_)
    {
    case string_t:
        return std::basic_string<Char>(value_.string_value_.data_,value_.string_value_.length_);
    default:
        return to_string();
    }
}

template <class Char>
std::ostream& operator<<(std::ostream& os, const basic_json<Char>& o)
{
    o.to_stream(os);
    return os;
}

inline 
char to_hex_character(unsigned char c)
{
    assert( c <= 0xF );

    return ( c < 10 ) ? ('0' + c) : ('A' - 10 + c);
}

inline
bool is_non_ascii_character(unsigned int c)
{
    return c >= 0x80;
}

template <class Char>
void escape_string(const Char* s, 
                   size_t length,
                   const basic_output_format<Char>& format,
                   std::basic_ostream<Char>& os)
{
    for (size_t i = 0; i < length; ++i)
    {
        Char c = s[i];
        switch (c)
        {
        case '\\':
            os << '\\';
            break;
        case '"':
            os << '\\' << '\"';
            break;
        case '\b':
            os << '\\' << 'b';
            break;
        case '\f':
            os << '\\';
            os << 'f';
            break;
        case '\n':
            os << '\\';
            os << 'n';
            break;
        case '\r':
            os << '\\';
            os << 'r';
            break;
        case '\t':
            os << '\\';
            os << 't';
            break;
        default:
            unsigned int u(c >= 0 ? c : 256 + c );
            if (is_control_character(u) || (format.escape_all_non_ascii() && is_non_ascii_character(u)))
            {
                // convert utf8 to codepoint
                unsigned int cp = json_char_traits<Char>::char_sequence_to_codepoint(s,length,i);

                os << '\\';
                os << 'u';
                os << to_hex_character(cp >>12 & 0x000F );
                os << to_hex_character(cp >>8  & 0x000F ); 
                os << to_hex_character(cp >>4  & 0x000F ); 
                os << to_hex_character(cp     & 0x000F ); 
            }
            else if (format.escape_solidus() && c == '/')
            {
                os << '\\';
                os << '/';
            }
            else
            {
                os << c;
            }
            break;
        }
    }
}

}

#endif
