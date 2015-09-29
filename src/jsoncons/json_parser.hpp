// Copyright 2015 Daniel Parker
// Distributed under the Boost license, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// See https://sourceforge.net/projects/jsoncons/files/ for latest version
// See https://sourceforge.net/p/jsoncons/wiki/Home/ for documentation.

#ifndef JSONCONS_JSON_PARSER_HPP
#define JSONCONS_JSON_PARSER_HPP

#include <memory>
#include <string>
#include <sstream>
#include <vector>
#include <istream>
#include <cstdlib>
#include <stdexcept>
#include <system_error>
#include "jsoncons/jsoncons.hpp"
#include "jsoncons/json_input_handler.hpp"
#include "jsoncons/parse_error_handler.hpp"

namespace jsoncons {

namespace mode {
    enum mode_t {
        array_element,
        object_member_name,
        object_member_value,
        done
    };
};

namespace symbols {
    enum symbols_t {
        control_char,
        tab, 
        lf,
        cr,
        space,  
        lbracket, 
        rbracket, 
        lbrace, 
        rbrace, 
        colon, 
        comma, 
        quote, 
        backslash, 
        slash,
        single_quote, 
        star,
        plus,  
        minus, 
        period, 
        zero , 
        digit, 
        a, 
        b, 
        c, 
        d, 
        e, 
        f, 
        l, 
        n, 
        r, 
        s, 
        t, 
        u, 
        upper_abcdf, 
        upper_e,     
        other
    };
};

namespace state {
    enum state_t {
        start, 
        slash,  
        slash_slash, 
        slash_star, 
        slash_star_star,
        expect_comma_or_end,  
        object,
        expect_member_name, 
        expect_colon,
        expect_value,
        array, 
        string,
        escape, 
        u1, 
        u2, 
        u3, 
        u4, 
        expect_surrogate_pair1, 
        expect_surrogate_pair2, 
        u6, 
        u7, 
        u8, 
        u9, 
        minus, 
        zero,  
        integer,
        fraction,
        exp1,
        exp2,
        exp3,
        n,
        nu,
        nul,
        t,  
        tr,  
        tru, 
        f,  
        fa, 
        fal,
        fals,
        done,
        error,
        num_states
    };
};

template<typename Char>
class basic_json_parser : private basic_parsing_context<Char>
{
    static const size_t default_max_buffer_length = 16384;
    static const int default_depth = 100;

public:
    basic_json_parser(basic_json_input_handler<Char>& handler)
       : top_(-1),
         stack_(default_depth),
         handler_(std::addressof(handler)),
         err_handler_(std::addressof(default_basic_parse_error_handler<Char>::instance())),
         is_negative_(false),
         cp_(0),
         index_(0)

    {
        depth_ = default_depth;
        state_ = state::start;
        top_ = -1;
        line_ = 1;
        column_ = 0;
        prev_char_ = 0;
        max_depth_ = std::numeric_limits<int>::max JSONCONS_NO_MACRO_EXP();
    }

    basic_parsing_context<Char> const & parsing_context() const
    {
        return *this;
    }

    basic_json_parser(basic_json_input_handler<Char>& handler,
                      basic_parse_error_handler<Char>& err_handler)
       : top_(-1),
         stack_(default_depth),
         handler_(std::addressof(handler)),
         err_handler_(std::addressof(err_handler)),
         is_negative_(false),
         cp_(0),
         index_(0)

    {
        depth_ = default_depth;
        state_ = state::start;
        top_ = -1;
        line_ = 1;
        column_ = 0;
        prev_char_ = 0;
        max_depth_ = std::numeric_limits<int>::max JSONCONS_NO_MACRO_EXP();
    }

    ~basic_json_parser()
    {
    }

    void init()
    {
    }

    size_t max_depth() const
    {
        return static_cast<size_t>(max_depth_);
    }

    void max_depth(size_t max_depth)
    {
        max_depth_ = static_cast<int>(std::min(max_depth,static_cast<size_t>(std::numeric_limits<int>::max JSONCONS_NO_MACRO_EXP())));
        if (depth_ > max_depth_)
        {
            depth_ = max_depth_;
            stack_.resize(depth_);
        }
    }

    bool done() const
    {
        return state_ == state::done;
    }

    void begin_parse()
    {
        if (!push(mode::done))
        {
            state_ = state::error;
            err_handler_->error(std::error_code(json_parser_errc::max_depth_exceeded, json_parser_category()), *this);
        }
        state_ = state::start;
    }

    void check_done(Char const* p, size_t start, size_t length)
    {
        index_ = start;
        for (; index_ < length; ++index_)
        {
            int next_char = p[index_];
            switch (next_char)
            {
            case '\n':
            case '\r':
            case '\t':
            case ' ':
                break;
            default:
                state_ = state::error;
                err_handler_->error(std::error_code(json_parser_errc::extra_character, json_parser_category()), *this);
                break;
            }
        }
    }

    void parse(Char const* p, size_t start, size_t length)
    {
        index_ = start;
        for (; index_ < length && state_ != state::done; ++index_)
        {
            int next_char = p[index_];
            symbols::symbols_t next_symbol;
            if (next_char >= 128) 
            {
                next_symbol = symbols::other;
            } 
            else 
            {
                next_symbol = input_symbols[next_char];
            }
            ++column_;
            switch (next_symbol)
            {
            case symbols::control_char:
                state_ = state::error;
                err_handler_->error(std::error_code(json_parser_errc::illegal_control_character, json_parser_category()), *this);
                break;
            case symbols::cr:
                ++line_, column_ = 1;
                break;
            case symbols::lf:
                if (prev_char_ != '\r')
                {
                    ++line_, column_ = 1;
                }
                break;
            }

            switch (state_)
            {
            case state::start: 
                {
                    switch (next_symbol)
                    {
                    case symbols::space:case symbols::lf:case symbols::cr:case symbols::tab:
                        break; 
                    case symbols::lbrace:
                        handler_->begin_json();
                        if (!push(mode::object_member_name))
                        {
                            state_ = state::error;
                            err_handler_->error(std::error_code(json_parser_errc::max_depth_exceeded, json_parser_category()), *this);
                        }
                        state_ = state::object;
                        handler_->begin_object(*this);
                        break;
                    case symbols::lbracket:
                        handler_->begin_json();
                        if (!push(mode::array_element))
                        {
                            state_ = state::error;
                            err_handler_->error(std::error_code(json_parser_errc::max_depth_exceeded, json_parser_category()), *this);
                        }
                        state_ = state::array;
                        handler_->begin_array(*this);
                        break;
                    case symbols::slash:
                        saved_state_ = state_;
                        state_ = state::slash;
                        break;
                    case symbols::rbrace:
                        state_ = state::error;
                        err_handler_->error(std::error_code(json_parser_errc::unexpected_end_of_object, json_parser_category()), *this);
                        break;
                    case symbols::rbracket:
                        state_ = state::error;
                        err_handler_->error(std::error_code(json_parser_errc::unexpected_end_of_array, json_parser_category()), *this);
                        break;
                    default:
                        state_ = state::error;
                        err_handler_->error(std::error_code(json_parser_errc::invalid_json_text, json_parser_category()), *this);
                        break;
                    }
                }
                break;

            case state::expect_comma_or_end: 
                {
                    switch (next_symbol)
                    {
                    case symbols::space:case symbols::lf:case symbols::cr:case symbols::tab:
                        break; 
                    case symbols::rbrace:
                        if (!pop(mode::object_member_value))
                        {
                            state_ = state::error;
                            err_handler_->error(std::error_code(json_parser_errc::mismatched_parentheses_or_brackets, json_parser_category()), *this);
                        }
                        handler_->end_object(*this);
                        if (peek() == mode::done)
                        {
                            state_ = state::done;
                            handler_->end_json();
                        }
                        else
                        {
                            state_ = state::expect_comma_or_end;
                        }
                        break;
                    case symbols::rbracket:
                        if (!pop(mode::array_element))
                        {
                            state_ = state::error;
                            err_handler_->error(std::error_code(json_parser_errc::mismatched_parentheses_or_brackets, json_parser_category()), *this);
                        }
                        handler_->end_array(*this);
                        if (peek() == mode::done)
                        {
                            state_ = state::done;
                            handler_->end_json();
                        }
                        else
                        {
                            state_ = state::expect_comma_or_end;
                        }
                        break;
                    case symbols::comma:
                        begin_member_or_element();
                        break;
                    case symbols::slash:
                        saved_state_ = state_;
                        state_ = state::slash;
                        break;
                    default:
                        state_ = state::error;
                        err_handler_->error(std::error_code(json_parser_errc::expected_comma_or_end, json_parser_category()), *this);
                        break;
                    }
                }
                break;
            case state::object: 
                {
                    switch (next_symbol)
                    {
                    case symbols::space:case symbols::lf:case symbols::cr:case symbols::tab:
                        break;
                    case symbols::rbrace:
                        if (!pop(mode::object_member_name))
                        {
                            state_ = state::error;
                            err_handler_->error(std::error_code(json_parser_errc::invalid_json_text, json_parser_category()), *this);
                        }
                        handler_->end_object(*this);
                        if (peek() == mode::done)
                        {
                            state_ = state::done;
                            handler_->end_json();
                        }
                        else
                        {
                            state_ = state::expect_comma_or_end;
                        }
                        break;
                    case symbols::quote:
                        state_ = state::string;
                        break;
                    case symbols::slash:
                        saved_state_ = state_;
                        state_ = state::slash;
                        break;
                    case symbols::single_quote:
                        state_ = state::error;
                        err_handler_->error(std::error_code(json_parser_errc::single_quote, json_parser_category()), *this);
                        break;
                    default:
                        state_ = state::error;
                        err_handler_->error(std::error_code(json_parser_errc::expected_name, json_parser_category()), *this);
                        break;
                    }
                }
                break;
            case state::expect_member_name: 
                {
                    switch (next_symbol)
                    {
                    case symbols::space:case symbols::lf:case symbols::cr:case symbols::tab:
                        break;
                    case symbols::quote:
                        state_ = state::string;
                        break;
                    case symbols::slash:
                        saved_state_ = state_;
                        state_ = state::slash;
                        break;
                    case symbols::rbrace:
                        state_ = state::error;
                        err_handler_->error(std::error_code(json_parser_errc::extra_comma, json_parser_category()), *this);
                        break;
                    case symbols::single_quote:
                        state_ = state::error;
                        err_handler_->error(std::error_code(json_parser_errc::single_quote, json_parser_category()), *this);
                        break;
                    default:
                        state_ = state::error;
                        err_handler_->error(std::error_code(json_parser_errc::expected_name, json_parser_category()), *this);
                        break;
                    }
                }
                break;
            case state::expect_colon: 
                {
                    switch (next_symbol)
                    {
                    case symbols::space:case symbols::lf:case symbols::cr:case symbols::tab:
                        break;
                    case symbols::colon:
                        begin_member_value();
                        state_ = state::expect_value;
                        break;
                    case symbols::slash:
                        saved_state_ = state_;
                        state_ = state::slash;
                        break;
                    default:
                        state_ = state::error;
                        err_handler_->error(std::error_code(json_parser_errc::expected_colon, json_parser_category()), *this);
                        break;
                    }
                }
                break;
            case state::expect_value: 
                {
                    switch (next_symbol)
                    {
                    case symbols::space:case symbols::lf:case symbols::cr:case symbols::tab:
                        break;
                    case symbols::lbrace:
                        if (!push(mode::object_member_name))
                        {
                            state_ = state::error;
                            err_handler_->error(std::error_code(json_parser_errc::max_depth_exceeded, json_parser_category()), *this);
                        }
                        state_ = state::object;
                        handler_->begin_object(*this);
                        break;
                    case symbols::lbracket:
                        if (!push(mode::array_element))
                        {
                            state_ = state::error;
                            err_handler_->error(std::error_code(json_parser_errc::max_depth_exceeded, json_parser_category()), *this);
                        }
                        state_ = state::array;
                        handler_->begin_array(*this);
                        break;
                    case symbols::quote:
                        state_ = state::string;
                        break;
                    case symbols::slash:
                        saved_state_ = state_;
                        state_ = state::slash;
                        break;
                    case symbols::minus:
                        is_negative_ = true;
                        state_ = state::minus;
                        break;
                    case symbols::zero: 
                        string_buffer_.push_back(next_char);
                        state_ = state::zero;
                        break;
                    case symbols::digit:
                        string_buffer_.push_back(next_char);
                        state_ = state::integer;
                        break;
                    case symbols::f:
                        state_ = state::f;
                        if ((index_+4) < length)
                        {
                            if ((p[index_+1] == 'a') & (p[index_+2] == 'l') & (p[index_+3] == 's') & (p[index_+4] == 'e'))
                            {
                                index_ += 4;
                                column_ += 4;
                                handler_->value(false, *this);
                                state_ = state::expect_comma_or_end;
                            }
                        }
                        break;
                    case symbols::n:
                        state_ = state::n;
                        if ((index_+3) < length)
                        {
                            if ((p[index_+1] == 'u') & (p[index_+2] == 'l') & (p[index_+3] == 'l'))
                            {
                                index_ += 3;
                                column_ += 3;
                                handler_->value(null_type(), *this);
                                state_ = state::expect_comma_or_end;
                            }
                        }
                        break;
                    case symbols::t:
                        state_ = state::t;
                        if ((index_+3) < length)
                        {
                            if ((p[index_+1] == 'r') & (p[index_+2] == 'u') & (p[index_+3] == 'e'))
                            {
                                index_ += 3;
                                column_ += 3;
                                handler_->value(true, *this);
                                state_ = state::expect_comma_or_end;
                            }
                        }
                        break;
                    case symbols::rbracket:
                        if (peek() == mode::array_element)
                        {
                            state_ = state::error;
                            err_handler_->error(std::error_code(json_parser_errc::extra_comma, json_parser_category()), *this);
                        }
                        else
                        {
                            state_ = state::error;
                            err_handler_->error(std::error_code(json_parser_errc::expected_value, json_parser_category()), *this);
                        }
                        break;
                    case symbols::single_quote:
                        state_ = state::error;
                        err_handler_->error(std::error_code(json_parser_errc::single_quote, json_parser_category()), *this);
                        break;
                    default:
                        state_ = state::error;
                        err_handler_->error(std::error_code(json_parser_errc::expected_value, json_parser_category()), *this);
                        break;
                    }
                }
                break;
            case state::array: 
                {
                    switch (next_symbol)
                    {
                    case symbols::space:case symbols::lf:case symbols::cr:case symbols::tab:
                        break;
                    case symbols::lbrace:
                        if (!push(mode::object_member_name))
                        {
                            state_ = state::error;
                            err_handler_->error(std::error_code(json_parser_errc::max_depth_exceeded, json_parser_category()), *this);
                        }
                        state_ = state::object;
                        handler_->begin_object(*this);
                        break;
                    case symbols::lbracket:
                        if (!push(mode::array_element))
                        {
                            state_ = state::error;
                            err_handler_->error(std::error_code(json_parser_errc::max_depth_exceeded, json_parser_category()), *this);
                        }
                        state_ = state::array;
                        handler_->begin_array(*this);
                        break;
                    case symbols::rbracket:
                        if (!pop(mode::array_element))
                        {
                            state_ = state::error;
                            err_handler_->error(std::error_code(json_parser_errc::invalid_json_text, json_parser_category()), *this);
                        }
                        handler_->end_array(*this);
                        if (peek() == mode::done)
                        {
                            state_ = state::done;
                            handler_->end_json();
                        }
                        else
                        {
                            state_ = state::expect_comma_or_end;
                        }
                        break;
                    case symbols::quote:
                        state_ = state::string;
                        break;
                    case symbols::slash:
                        saved_state_ = state_;
                        state_ = state::slash;
                        break;
                    case symbols::minus:
                        is_negative_ = true;
                        state_ = state::minus;
                        break;
                    case symbols::zero: 
                        string_buffer_.push_back(next_char);
                        state_ = state::zero;
                        break;
                    case symbols::digit:
                        string_buffer_.push_back(next_char);
                        state_ = state::integer;
                        break;
                    case symbols::f:
                        state_ = state::f;
                        if ((index_+4) < length)
                        {
                            if ((p[index_+1] == 'a') & (p[index_+2] == 'l') & (p[index_+3] == 's') & (p[index_+4] == 'e'))
                            {
                                index_ += 4;
                                column_ += 4;
                                handler_->value(false, *this);
                                state_ = state::expect_comma_or_end;
                            }
                        }
                        break;
                    case symbols::n:
                        state_ = state::n;
                        if ((index_+3) < length)
                        {
                            if ((p[index_+1] == 'u') & (p[index_+2] == 'l') & (p[index_+3] == 'l'))
                            {
                                index_ += 3;
                                column_ += 3;
                                handler_->value(null_type(), *this);
                                state_ = state::expect_comma_or_end;
                            }
                        }
                        break;
                    case symbols::t:
                        state_ = state::t;
                        if ((index_+3) < length)
                        {
                            if ((p[index_+1] == 'r') & (p[index_+2] == 'u') & (p[index_+3] == 'e'))
                            {
                                index_ += 3;
                                column_ += 3;
                                handler_->value(true, *this);
                                state_ = state::expect_comma_or_end;
                            }
                        }
                        break;
                    case symbols::single_quote:
                        state_ = state::error;
                        err_handler_->error(std::error_code(json_parser_errc::single_quote, json_parser_category()), *this);
                        break;
                    default:
                        state_ = state::error;
                        err_handler_->error(std::error_code(json_parser_errc::expected_value, json_parser_category()), *this);
                        break;
                    }
                }
                break;
            case state::string: 
                {
                    switch (next_symbol)
                    {
                    case symbols::lf:
                    case symbols::cr:
                    case symbols::tab:
                        state_ = state::error;
                        err_handler_->error(std::error_code(json_parser_errc::illegal_character_in_string, json_parser_category()), *this);
                        break;
                    case symbols::backslash: 
                        state_ = state::escape;
                        break;
                    case symbols::quote:
                        end_string_value();
                        break;
                    default:
                        string_buffer_.push_back(next_char);
                        break;
                    }
                }
                break;
            case state::escape: 
                {
                    escape_next_char(next_char);
                }
                break;
            case state::u1: 
                append_codepoint(next_char);
                state_ = state::u2;
                break;
            case state::u2: 
                append_codepoint(next_char);
                state_ = state::u3;
                break;
            case state::u3: 
                append_codepoint(next_char);
                state_ = state::u4;
                break;
            case state::u4: 
                {
                    append_codepoint(next_char);
                    if (cp_ >= min_lead_surrogate && cp_ <= max_lead_surrogate)
                    {
                        state_ = state::expect_surrogate_pair1;
                    }
                    else
                    {
                        json_char_traits<Char, sizeof(Char)>::append_codepoint_to_string(cp_, string_buffer_);
                        state_ = state::string;
                    }
                }
                break;
            case state::expect_surrogate_pair1: 
                {
                    switch (next_symbol)
                    {
                    case symbols::backslash: 
                        cp2_ = 0;
                        state_ = state::expect_surrogate_pair2;
                        break;
                    default:
                        state_ = state::error;
                        err_handler_->error(std::error_code(json_parser_errc::expected_codepoint_surrogate_pair, json_parser_category()), *this);
                        break;
                    }
                }
                break;
            case state::expect_surrogate_pair2: 
                {
                    switch (next_symbol)
                    {
                    case symbols::u:
                        state_ = state::u6;
                        break;
                    default:
                        state_ = state::error;
                        err_handler_->error(std::error_code(json_parser_errc::expected_codepoint_surrogate_pair, json_parser_category()), *this);
                        break;
                    }
                }
                break;
            case state::u6:
                {
                    append_second_codepoint(next_char);
                    state_ = state::u7;
                }
                break;
            case state::u7: 
                {
                    append_second_codepoint(next_char);
                    state_ = state::u8;
                }
                break;
            case state::u8: 
                {
                    append_second_codepoint(next_char);
                    state_ = state::u9;
                }
                break;
            case state::u9: 
				{
                    append_second_codepoint(next_char);
                    uint32_t cp = 0x10000 + ((cp_ & 0x3FF) << 10) + (cp2_ & 0x3FF);
                    json_char_traits<Char, sizeof(Char)>::append_codepoint_to_string(cp, string_buffer_);
                    state_ = state::string;
				}
                break;
            case state::minus:  
                {
                    switch (next_symbol)
                    {
                    case symbols::zero: 
                        string_buffer_.push_back(next_char);
                        state_ = state::zero;
                        break;
                    case symbols::digit:
                        string_buffer_.push_back(next_char);
                        state_ = state::integer;
                        break;
                    default:
                        state_ = state::error;
                        err_handler_->error(std::error_code(json_parser_errc::expected_value, json_parser_category()), *this);
                        break;
                    }
                }
                break;
            case state::zero:  
                {
                    switch (next_symbol)
                    {
                    case symbols::space:case symbols::lf:case symbols::cr:case symbols::tab:
                        end_integer_value();
                        state_ = state::expect_comma_or_end;
                        break; // No change
                    case symbols::rbrace:
                        end_integer_value();
                        if (!pop(mode::object_member_value))
                        {
                            state_ = state::error;
                            err_handler_->error(std::error_code(json_parser_errc::invalid_json_text, json_parser_category()), *this);
                        }
                        handler_->end_object(*this);
                        if (peek() == mode::done)
                        {
                            state_ = state::done;
                            handler_->end_json();
                        }
                        else
                        {
                            state_ = state::expect_comma_or_end;
                        }
                        break;
                    case symbols::rbracket:
                        end_integer_value();
                        if (!pop(mode::array_element))
                        {
                            state_ = state::error;
                            err_handler_->error(std::error_code(json_parser_errc::invalid_json_text, json_parser_category()), *this);
                        }
                        handler_->end_array(*this);
                        if (peek() == mode::done)
                        {
                            state_ = state::done;
                            handler_->end_json();
                        }
                        else
                        {
                            state_ = state::expect_comma_or_end;
                        }
                        break;
                    case symbols::period:
                        string_buffer_.push_back(next_char);
                        state_ = state::fraction;
                        break;
                    case symbols::comma:
                        end_integer_value();
                        begin_member_or_element();
                        break;
                    case symbols::zero: 
                    case symbols::digit:
                        state_ = state::error;
                        err_handler_->error(std::error_code(json_parser_errc::leading_zero, json_parser_category()), *this);
                        break;
                    default:
                        state_ = state::error;
                        err_handler_->error(std::error_code(json_parser_errc::invalid_number, json_parser_category()), *this);
                        break;
                    }
                }
                break;
            case state::integer: 
                {
                    switch (next_symbol)
                    {
                    case symbols::space:case symbols::lf:case symbols::cr:case symbols::tab:
                        end_integer_value();
                        state_ = state::expect_comma_or_end;
                        break; 
                    case symbols::rbrace:
                        end_integer_value();
                        if (!pop(mode::object_member_value))
                        {
                            state_ = state::error;
                            err_handler_->error(std::error_code(json_parser_errc::invalid_json_text, json_parser_category()), *this);
                        }
                        handler_->end_object(*this);
                        if (peek() == mode::done)
                        {
                            state_ = state::done;
                            handler_->end_json();
                        }
                        else
                        {
                            state_ = state::expect_comma_or_end;
                        }
                        break;
                    case symbols::rbracket:
                        end_integer_value();
                        if (!pop(mode::array_element))
                        {
                            state_ = state::error;
                            err_handler_->error(std::error_code(json_parser_errc::invalid_json_text, json_parser_category()), *this);
                        }
                        handler_->end_array(*this);
                        if (peek() == mode::done)
                        {
                            state_ = state::done;
                            handler_->end_json();
                        }
                        else
                        {
                            state_ = state::expect_comma_or_end;
                        }
                        break;
                    case symbols::zero: 
                    case symbols::digit:
                        string_buffer_.push_back(next_char);
                        state_ = state::integer;
                        break;
                    case symbols::period:
                        string_buffer_.push_back(next_char);
                        state_ = state::fraction;
                        break;
                    case symbols::comma:
                        end_integer_value();
                        begin_member_or_element();
                        break;
                    case symbols::e:
                    case symbols::upper_e:
                        string_buffer_.push_back(next_char);
                        state_ = state::exp1;
                        break;
                    default:
                        state_ = state::error;
                        err_handler_->error(std::error_code(json_parser_errc::invalid_number, json_parser_category()), *this);
                        break;
                    }
                }
                break;
            case state::fraction: 
                {
                    switch (next_symbol)
                    {
                    case symbols::space:case symbols::lf:case symbols::cr:case symbols::tab:
                        end_fraction_value();
                        state_ = state::expect_comma_or_end;
                        break; 
                    case symbols::rbrace:
                        end_fraction_value();
                        if (!pop(mode::object_member_value))
                        {
                            state_ = state::error;
                            err_handler_->error(std::error_code(json_parser_errc::invalid_json_text, json_parser_category()), *this);
                        }
                        handler_->end_object(*this);
                        if (peek() == mode::done)
                        {
                            state_ = state::done;
                            handler_->end_json();
                        }
                        else
                        {
                            state_ = state::expect_comma_or_end;
                        }
                        break;
                    case symbols::rbracket:
                        end_fraction_value();
                        if (!pop(mode::array_element))
                        {
                            state_ = state::error;
                            err_handler_->error(std::error_code(json_parser_errc::invalid_json_text, json_parser_category()), *this);
                        }
                        handler_->end_array(*this);
                        if (peek() == mode::done)
                        {
                            state_ = state::done;
                            handler_->end_json();
                        }
                        else
                        {
                            state_ = state::expect_comma_or_end;
                        }
                        break;
                    case symbols::zero: 
                    case symbols::digit:
                        string_buffer_.push_back(next_char);
                        state_ = state::fraction;
                        break;
                    case symbols::comma:
                        end_fraction_value();
                        begin_member_or_element();
                        break;
                    case symbols::e:
                    case symbols::upper_e:
                        string_buffer_.push_back(next_char);
                        state_ = state::exp1;
                        break;
                    default:
                        state_ = state::error;
                        err_handler_->error(std::error_code(json_parser_errc::invalid_number, json_parser_category()), *this);
                        break;
                    }
                }
                break;
            case state::exp1: 
                {
                    switch (next_symbol)
                    {
                    case symbols::plus:
                        state_ = state::exp2;
                        break;
                    case symbols::minus:
                        string_buffer_.push_back(next_char);
                        state_ = state::exp2;
                        break;
                    case symbols::zero: 
                    case symbols::digit:
                        string_buffer_.push_back(next_char);
                        state_ = state::exp3;
                        break;
                    default:
                        state_ = state::error;
                        err_handler_->error(std::error_code(json_parser_errc::expected_value, json_parser_category()), *this);
                        break;
                    }
                }
                break;
            case state::exp2:  
                {
                    switch (next_symbol)
                    {
                    case symbols::zero: 
                    case symbols::digit:
                        string_buffer_.push_back(next_char);
                        state_ = state::exp3;
                        break;
                    default:
                        state_ = state::error;
                        err_handler_->error(std::error_code(json_parser_errc::expected_value, json_parser_category()), *this);
                        break;
                    }
                }
                break;
            case state::exp3: 
                {
                    switch (next_symbol)
                    {
                    case symbols::space:case symbols::lf:case symbols::cr:case symbols::tab:
                        end_fraction_value();
                        state_ = state::expect_comma_or_end;
                        break; 
                    case symbols::rbrace:
                        end_fraction_value();
                        if (!pop(mode::object_member_value))
                        {
                            state_ = state::error;
                            err_handler_->error(std::error_code(json_parser_errc::invalid_json_text, json_parser_category()), *this);
                        }
                        handler_->end_object(*this);
                        if (peek() == mode::done)
                        {
                            state_ = state::done;
                            handler_->end_json();
                        }
                        else
                        {
                            state_ = state::expect_comma_or_end;
                        }
                        break;
                    case symbols::rbracket:
                        end_fraction_value();
                        if (!pop(mode::array_element))
                        {
                            state_ = state::error;
                            err_handler_->error(std::error_code(json_parser_errc::invalid_json_text, json_parser_category()), *this);
                        }
                        handler_->end_array(*this);
                        if (peek() == mode::done)
                        {
                            state_ = state::done;
                            handler_->end_json();
                        }
                        else
                        {
                            state_ = state::expect_comma_or_end;
                        }
                        break;
                    case symbols::comma:
                        end_fraction_value();
                        begin_member_or_element();
                        break;
                    case symbols::zero: 
                    case symbols::digit:
                        string_buffer_.push_back(next_char);
                        state_ = state::exp3;
                        break;
                    default:
                        state_ = state::error;
                        err_handler_->error(std::error_code(json_parser_errc::invalid_number, json_parser_category()), *this);
                        break;
                    }
                }
                break;
            case state::t: 
                {
                    switch (next_symbol)
                    {
                    case symbols::r:
                        state_ = state::tr;
                        break;
                    default:
                        state_ = state::error;
                        err_handler_->error(std::error_code(json_parser_errc::expected_value, json_parser_category()), *this);
                        break;
                    }
                }
                break;
            case state::tr: 
                {
                    switch (next_symbol)
                    {
                    case symbols::u:
                        state_ = state::tru;
                        break;
                    default:
                        state_ = state::error;
                        err_handler_->error(std::error_code(json_parser_errc::expected_value, json_parser_category()), *this);
                        break;
                    }
                }
                break;
            case state::tru:  
                {
                    switch (next_symbol)
                    {
                    case symbols::e: 
                        handler_->value(true, *this);
                        state_ = state::expect_comma_or_end;
                        break;
                    default:
                        state_ = state::error;
                        err_handler_->error(std::error_code(json_parser_errc::expected_value, json_parser_category()), *this);
                        break;
                    }
                }
                break;
            case state::f:  
                {
                    switch (next_symbol)
                    {
                    case symbols::a:
                        state_ = state::fa;
                        break;
                    default:
                        state_ = state::error;
                        err_handler_->error(std::error_code(json_parser_errc::expected_value, json_parser_category()), *this);
                        break;
                    }
                }
                break;
            case state::fa: 
                {
                    switch (next_symbol)
                    {
                    case symbols::l: 
                        state_ = state::fal;
                        break;
                    default:
                        state_ = state::error;
                        err_handler_->error(std::error_code(json_parser_errc::expected_value, json_parser_category()), *this);
                        break;
                    }
                }
                break;
            case state::fal:  
                {
                    switch (next_symbol)
                    {
                    case symbols::s:
                        state_ = state::fals;
                        break;
                    default:
                        state_ = state::error;
                        err_handler_->error(std::error_code(json_parser_errc::expected_value, json_parser_category()), *this);
                        break;
                    }
                }
                break;
            case state::fals:  
                {
                    switch (next_symbol)
                    {
                    case symbols::e:
                        handler_->value(false, *this);
                        state_ = state::expect_comma_or_end;
                        break;
                    default:
                        state_ = state::error;
                        err_handler_->error(std::error_code(json_parser_errc::expected_value, json_parser_category()), *this);
                        break;
                    }
                }
                break;
            case state::n: 
                {
                    switch (next_symbol)
                    {
                    case symbols::u:
                        state_ = state::nu;
                        break;
                    default:
                        state_ = state::error;
                        err_handler_->error(std::error_code(json_parser_errc::expected_value, json_parser_category()), *this);
                        break;
                    }
                }
                break;
            case state::nu:  
                {
                    switch (next_symbol)
                    {
                    case symbols::l: 
                        state_ = state::nul;
                        break;
                    default:
                        state_ = state::error;
                        err_handler_->error(std::error_code(json_parser_errc::expected_value, json_parser_category()), *this);
                        break;
                    }
                }
                break;
            case state::nul:  
                {
                    switch (next_symbol)
                    {
                    case symbols::l: 
                        handler_->value(null_type(), *this);
                        state_ = state::expect_comma_or_end;
                        break;
                    default:
                        state_ = state::error;
                        err_handler_->error(std::error_code(json_parser_errc::expected_value, json_parser_category()), *this);
                        break;
                    }
                }
                break;
            case state::slash: 
                {
                    switch (next_symbol)
                    {
                    case symbols::star:
                        state_ = state::slash_star;
                        break;
                    case symbols::slash:
                        state_ = state::slash_slash;
                        break;
                    default:    
                        state_ = state::error;
                        err_handler_->error(std::error_code(json_parser_errc::invalid_json_text, json_parser_category()), *this);
                        break;
                    }
                }
                break;
            case state::slash_star:  
                {
                    switch (next_symbol)
                    {
                    case symbols::star:
                        state_ = state::slash_star_star;
                        break;
                    }
                }
                break;
            case state::slash_slash: 
                {
                    switch (next_symbol)
                    {
                    case symbols::lf:
                    case symbols::cr:
                        state_ = saved_state_;
                        break;
                    }
                }
                break;
            case state::slash_star_star: 
                {
                    switch (next_symbol)
                    {
                    case symbols::slash:
                        state_ = saved_state_;
                        break;
                    default:    
                        state_ = state::slash_star;
                        break;
                    }
                }
                break;
            }
            prev_char_ = next_char;
        }
    }

    void end_parse()
    {
        if (!pop(mode::done))
        {
            state_ = state::error;
            err_handler_->error(std::error_code(json_parser_errc::unexpected_eof, json_parser_category()), *this);
        }
    }

    state::state_t state() const
    {
        return state_;
    }

    size_t index() const
    {
        return index_;
    }
private:
    void end_fraction_value()
    {
        try
        {
            double d = string_to_float(string_buffer_);
            if (is_negative_)
                d = -d;
            handler_->value(d, *this);
        }
        catch (...)
        {
            state_ = state::error;
            err_handler_->error(std::error_code(json_parser_errc::invalid_number, json_parser_category()), *this);
            handler_->value(null_type(), *this); // recovery
        }
        string_buffer_.clear();
        is_negative_ = false;
        state_ = state::expect_comma_or_end;
    }

    void end_integer_value()
    {
        if (is_negative_)
        {
            try
            {
                long long d = string_to_integer(is_negative_, string_buffer_.c_str(), string_buffer_.length());
                handler_->value(d, *this);
            }
            catch (const std::exception&)
            {
                try
                {
                    double d = string_to_float(string_buffer_);
                    handler_->value(-d, *this);
                }
                catch (...)
                {
                    state_ = state::error;
                    err_handler_->error(std::error_code(json_parser_errc::invalid_number, json_parser_category()), *this);
                    handler_->value(null_type(), *this);
                }
            }
        }
        else
        {
            try
            {
                unsigned long long d = string_to_unsigned(string_buffer_.c_str(), string_buffer_.length());
                handler_->value(d, *this);
            }
            catch (const std::exception&)
            {
                try
                {
                    double d = string_to_float(string_buffer_);
                    handler_->value(d, *this);
                }
                catch (...)
                {
                    state_ = state::error;
                    err_handler_->error(std::error_code(json_parser_errc::invalid_number, json_parser_category()), *this);
                    handler_->value(null_type(), *this);
                }
            }
        }
        string_buffer_.clear();
        is_negative_ = false;
        state_ = state::expect_comma_or_end;
    }

    void append_codepoint(int next_char)
    {
        switch (next_char)
        {
        case '0': case '1':case '2':case '3':case '4':case '5':case '6':case '7':case '8': case '9':
        case 'a':case 'b':case 'c':case 'd':case 'e':case 'f':
        case 'A':case 'B':case 'C':case 'D':case 'E':case 'F':
            cp_ = append_to_codepoint(cp_, next_char);
            break;
        default:
            state_ = state::error;
            err_handler_->error(std::error_code(json_parser_errc::expected_value, json_parser_category()), *this);
            break;
        }
    }

    void append_second_codepoint(int next_char)
    {
        switch (next_char)
        {
        case '0': 
        case '1':case '2':case '3':case '4':case '5':case '6':case '7':case '8': case '9':
        case 'a':case 'b':case 'c':case 'd':case 'e':case 'f':
        case 'A':case 'B':case 'C':case 'D':case 'E':case 'F':
            cp2_ = append_to_codepoint(cp2_, next_char);
            break;
        default:
            state_ = state::error;
            err_handler_->error(std::error_code(json_parser_errc::expected_value, json_parser_category()), *this);
            break;
        }
    }

    void escape_next_char(int next_input)
    {
        switch (next_input)
        {
        case '\"':
            string_buffer_.push_back('\"');
            state_ = state::string;
            break;
        case '\\': 
            string_buffer_.push_back('\\');
            state_ = state::string;
            break;
        case '/':
            string_buffer_.push_back('/');
            state_ = state::string;
            break;
        case 'b':
            string_buffer_.push_back('\b');
            state_ = state::string;
            break;
        case 'f':  
            string_buffer_.push_back('\f');
            state_ = state::string;
            break;
        case 'n':
            string_buffer_.push_back('\n');
            state_ = state::string;
            break;
        case 'r':
            string_buffer_.push_back('\r');
            state_ = state::string;
            break;
        case 't':
            string_buffer_.push_back('\t');
            state_ = state::string;
            break;
        case 'u':
            cp_ = 0;
            state_ = state::u1;
            break;
        default:    
            state_ = state::error;
            err_handler_->error(std::error_code(json_parser_errc::illegal_escaped_character, json_parser_category()), *this);
            break;
        }
    }

    void end_string_value() 
    {
        switch (stack_[top_])
        {
        case mode::object_member_name:
            handler_->name(string_buffer_.c_str(), string_buffer_.length(), *this);
            state_ = state::expect_colon;
            break;
        case mode::array_element:
        case mode::object_member_value:
            handler_->value(string_buffer_.c_str(), string_buffer_.length(), *this);
            state_ = state::expect_comma_or_end;
            break;
        default:
            state_ = state::error;
            err_handler_->error(std::error_code(json_parser_errc::invalid_json_text, json_parser_category()), *this);
            break;
        }
        string_buffer_.clear();
    }

    void begin_member_or_element() 
    {
        switch (stack_[top_])
        {
        case mode::object_member_value:
            // A comma causes a flip from object_member_value mode to object_member_name mode.
            if (!flip(mode::object_member_value, mode::object_member_name))
            {
                state_ = state::error;
                err_handler_->error(std::error_code(json_parser_errc::invalid_json_text, json_parser_category()), *this);
            }
            state_ = state::expect_member_name;
            break;
        case mode::array_element:
            state_ = state::expect_value;
            break;
        default:
            state_ = state::error;
            err_handler_->error(std::error_code(json_parser_errc::invalid_json_text, json_parser_category()), *this);
            break;
        }
    }

    void begin_member_value()
    {
        if (!flip(mode::object_member_name, mode::object_member_value))
        {
            state_ = state::error;
            err_handler_->error(std::error_code(json_parser_errc::invalid_json_text, json_parser_category()), *this);
        }
        state_ = state::expect_value;
    }
 
    uint32_t append_to_codepoint(uint32_t cp, int next_char)
    {
        cp *= 16;
        if (next_char >= '0'  &&  next_char <= '9')
        {
            cp += next_char - '0';
        }
        else if (next_char >= 'a'  &&  next_char <= 'f')
        {
            cp += next_char - 'a' + 10;
        }
        else if (next_char >= 'A'  &&  next_char <= 'F')
        {
            cp += next_char - 'A' + 10;
        }
        else
        {
            state_ = state::error;
            err_handler_->error(std::error_code(json_parser_errc::invalid_hex_escape_sequence, json_parser_category()), *this);
        }
        return cp;
    }

    unsigned long do_line_number() const override
    {
        return line_;
    }

    unsigned long do_column_number() const override
    {
        return column_;
    }

    Char do_last_char() const override
    {
        return c_;
    }

    bool push(mode::mode_t mode)
    {
        ++top_;
        if (top_ >= depth_)
        {
            if (top_ >= max_depth_)
            {
                return false;
            }
            depth_ *= 2;
            stack_.resize(depth_);
        }
        stack_[top_] = mode;
        return true;
    }

    int peek()
    {
        return stack_[top_];
    }

    bool peek(mode::mode_t mode)
    {
        return stack_[top_] == mode;
    }

    bool flip(mode::mode_t mode1, mode::mode_t mode2)
    {
        if (top_ < 0 || stack_[top_] != mode1)
        {
            return false;
        }
        stack_[top_] = mode2;
        return true;
    }

    bool pop(mode::mode_t mode)
    {
        if (top_ < 0 || stack_[top_] != mode)
        {
            return false;
        }
        --top_;
        return true;
    }

    template<typename CharType>
    unsigned long long string_to_unsigned(const CharType *s, size_t length) throw(std::overflow_error)
    {
        const unsigned long long max_value = std::numeric_limits<unsigned long long>::max JSONCONS_NO_MACRO_EXP();
        const unsigned long long max_value_div_10 = max_value / 10;
        unsigned long long n = 0;
        for (size_t i = 0; i < length; ++i)
        {
            unsigned long long x = s[i] - '0';
            if (n > max_value_div_10)
            {
                throw std::overflow_error("Unsigned overflow");
            }
            n = n * 10;
            if (n > max_value - x)
            {
                throw std::overflow_error("Unsigned overflow");
            }

            n += x;
        }
        return n;
    }

    template<typename CharType>
    long long string_to_integer(bool has_neg, const CharType *s, size_t length) throw(std::overflow_error)
    {
        const long long max_value = std::numeric_limits<long long>::max JSONCONS_NO_MACRO_EXP();
        const long long max_value_div_10 = max_value / 10;

        long long n = 0;
        for (size_t i = 0; i < length; ++i)
        {
            long long x = s[i] - '0';
            if (n > max_value_div_10)
            {
                throw std::overflow_error("Integer overflow");
            }
            n = n * 10;
            if (n > max_value - x)
            {
                throw std::overflow_error("Integer overflow");
            }

            n += x;
        }
        return has_neg ? -n : n;
    }

    state::state_t state_;
    int top_;
    std::vector<mode::mode_t> stack_;
    basic_json_input_handler<Char> *handler_;
    basic_parse_error_handler<Char> *err_handler_;
    unsigned long column_;
    unsigned long line_;
    Char c_;
    uint32_t cp_;
    uint32_t cp2_;
    std::basic_string<Char> string_buffer_;
    bool is_negative_;
    state::state_t saved_state_;
    int prev_char_;
    size_t index_;
    int depth_;
    int max_depth_;
    static symbols::symbols_t input_symbols[128];
};

template <class Char>
symbols::symbols_t basic_json_parser<Char>::input_symbols[128] = {
    symbols::control_char,      
    symbols::control_char,      
    symbols::control_char,      
    symbols::control_char,      
    symbols::control_char,      
    symbols::control_char,      
    symbols::control_char,      
    symbols::control_char,
    symbols::control_char,      
    symbols::tab, 
    symbols::lf, 
    symbols::control_char,      
    symbols::control_char,      
    symbols::cr, 
    symbols::control_char,      
    symbols::control_char,
    symbols::control_char,      
    symbols::control_char,      
    symbols::control_char,      
    symbols::control_char,      
    symbols::control_char,      
    symbols::control_char,      
    symbols::control_char,      
    symbols::control_char,
    symbols::control_char,      
    symbols::control_char,      
    symbols::control_char,      
    symbols::control_char,      
    symbols::control_char,      
    symbols::control_char,      
    symbols::control_char,      
    symbols::control_char,
    symbols::space, 
    symbols::other,   
    symbols::quote, 
    symbols::other,   
    symbols::other,   
    symbols::other,   
    symbols::other,   
    symbols::single_quote,
    symbols::other,   
    symbols::other,   
    symbols::star,   
    symbols::plus,  
    symbols::comma, 
    symbols::minus, 
    symbols::period, 
    symbols::slash,
    symbols::zero,  
    symbols::digit, 
    symbols::digit, 
    symbols::digit, 
    symbols::digit, 
    symbols::digit, 
    symbols::digit, 
    symbols::digit,
    symbols::digit, 
    symbols::digit, 
    symbols::colon, 
    symbols::other,   
    symbols::other,   
    symbols::other,   
    symbols::other,   
    symbols::other,
    symbols::other,   
    symbols::upper_abcdf, 
    symbols::upper_abcdf, 
    symbols::upper_abcdf, 
    symbols::upper_abcdf, 
    symbols::upper_e,     
    symbols::upper_abcdf, 
    symbols::other,
    symbols::other,   
    symbols::other,   
    symbols::other,   
    symbols::other,   
    symbols::other,   
    symbols::other,   
    symbols::other,   
    symbols::other,
    symbols::other,   
    symbols::other,   
    symbols::other,   
    symbols::other,   
    symbols::other,   
    symbols::other,   
    symbols::other,   
    symbols::other,
    symbols::other,   
    symbols::other,   
    symbols::other,   
    symbols::lbracket, 
    symbols::backslash, 
    symbols::rbracket, 
    symbols::other,   
    symbols::other,
    symbols::other,   
    symbols::a, 
    symbols::b, 
    symbols::c, 
    symbols::d, 
    symbols::e, 
    symbols::f, 
    symbols::other,
    symbols::other,   
    symbols::other,   
    symbols::other,   
    symbols::other,   
    symbols::l, 
    symbols::other,   
    symbols::n, 
    symbols::other,
    symbols::other,   
    symbols::other,   
    symbols::r, 
    symbols::s, 
    symbols::t, 
    symbols::u, 
    symbols::other,   
    symbols::other,
    symbols::other,   
    symbols::other,   
    symbols::other, 
    symbols::lbrace, 
    symbols::other,   
    symbols::rbrace, 
    symbols::other,   
    symbols::other
};


typedef basic_json_parser<char> json_parser;
typedef basic_json_parser<wchar_t> wjson_parser;

}

#endif

