    jsoncons::json

    typedef basic_json<char,std::allocator<void>> json

The `json` class is an instantiation of the `basic_json` class template that uses `char` as the character type
and `std::allocator<void>` as the allocator type. The allocator type is used to supply an allocator for dynamically allocated, 
fixed size small objects in the `json` container, the `json` container will rebind it as necessary. The allocator type
is not used for structures including vectors and strings that use large or variable amounts of memory, 
these always use the default allocators.

### Header

    #include "jsoncons/json.hpp"

### Member types

    member_type

[member_type](json_member_type) stores a name and a json value

    any
[any](json%20any) can contain any value that supports copy construction and assignment.

    null_type
An alias for `jsoncons::null_type`

    object
Object type

    array
Array type

Type tags that can be used with `is<T>` and `as<T>`

    object_iterator
A bidirectional iterator to `json::member_type`

    const_object_iterator
A bidirectional iterator to `const json::member_type`

    array_iterator
A random access iterator to `json`

    const_array_iterator
A random access iterator to `const json`

    object_range
    const_object_range
An object "range" defined by `begin()` and `end()` 

    array_range
    const_array_range
An array "range" defined by `begin()` and `end()` 

### Static member functions

    static json parse(std::istream& is)
    static json parse(std::istream& is, 
                      parse_error_handler& err_handler)
Parses an input stream of JSON text and returns a json object or array value. If parsing fails, throws a [json_parse_exception](json_parse_exception).

    static json parse_file(const std::string& filename)
    static json parse_file(const std::string& filename, 
                           parse_error_handler& err_handler)
Opens a binary input stream to a JSON unicode file, parsing the file assuming UTF-8, and returns a json object or array value. If parsing fails, throws a [json_parse_exception](json_parse_exception). This method expects that the file contains UTF-8 (or clean 7 bit ASCII), if that is not the case, use the `parse` method that takes an `std::istream` instead, imbue your stream with the appropriate facet for handling unicode conversions.

    static json parse(const std::string& s)
    static json parse(const std::string& s, 
                      parse_error_handler& err_handler)
Parses a string of JSON text and returns a json object or array value. If parsing fails, throws a [json_parse_exception](json_parse_exception).

    static json make_array()

    static json make_array(size_t n)

    template <typename T>
    static json make_array(size_ n, T val)

    template <size_t N>
    static json make_array(size_t size1 ... size_t sizeN)

    template <size_t N,typename T>
    static json make_array(size_t size1 ... size_t sizeN, T val)
Makes a multidimensional array with the number of dimensions specified as a template parameter. The size of each dimension is passed as a parameter, and optionally an inital value. If no initial value, the default is an empty json object. The elements may be accessed using familiar C++ native array syntax.

### Constructors

    json()
Constructs an empty json object. 

    json(const json& val)
Constructs a copy of val

    json(json&& val)
Acquires the contents of val, leaving val a `null` value

    template <typename T>
    json(T val)
Constructs a `json` value for types supported in `json_type_traits`

    template <class InputIterator>
    json(InputIterator first, InputIterator last)

Constructs a json array with the elements in the range [first,last).

### Destructor

    ~json()
Destroys all values and deletes all memory allocated for strings, arrays, and objects.

### Assignment operator

    json& operator=(const json& rhs)
    json& operator=(json&& rhs)

    template <class T>
    json& operator=(T rhs)
Assigns a new value to a `json` variable, replacing it's current contents.

### Ranges and Iterators

    object_range members();  
    const_object_range members() const;  
Returns a "range" defined by `begin()` and `end()` over the members of a `json` object      

    array_range elements();
    const_array_range elements() const;
Returns a "range" defined by `begin()` and `end()` over the elements of a `json` array      

### Capacity

    size_t size() const
Returns the number of elements in a json array, or the number of members in a json object, or `zero`

    bool empty() const
Returns `true` if a json string, object or array has no elements, otherwise `false`.

    size_t capacity() const
Returns the size of the storage space currently allocated for a json object or array.

    void reserve(size_t n)
Increases the capacity of a json object or array to allow at least `n` members or elements. 

    void resize(size_t n)
Resizes a json array so that it contains `n` elements. 

    void resize(size_t n, const json& val)
Resizes a json array so that it contains `n` elements that are initialized to `val`. 

### Accessors

    size_t count(const std::string& name) const
Returns the number of object members that match `name`.    

    template <typename T>
    bool is() const
Returns `true` if json value has type `T`, `false` otherwise.  

    is<char>
    is<signed char>
    is<unsigned char>
    is<wchar_t>
    is<short>
    is<unsigned short> 
    is<int> 
    is<unsigned int> 
    is<long> 
    is<unsigned long> 
    is<long long> 
    is<unsigned long long> 
Return `true` if json value is of integral type and within the range of the template type, `false` otherwise.  

    is<double> 
Return true if the json value is of floating point type and within the range of the template type, `false` otherwise.  

    is<std::string> 
Returns `true` if the json value is of string type, `false` otherwise.  

    is<bool>
Returns `true` if the json value is of boolean type, `false` otherwise.  

    is<json::null_type>
Returns `true` if the json value is null, `false` otherwise.  

    is<json::any>
Returns `true` if the json value is type any, `false` otherwise.

    is<json::object> 
Returns `true` if the json value is an object, `false` otherwise.  

    is<json::array> 
Returns `true` if the json value is an array, `false` otherwise.  

    is<json::std::vector<T>>
Returns `true` if the json value is an array and each element has type `T`, `false` otherwise.

    bool is_null() const
    bool is_string() const
    bool is_numeric() const
    bool is_longlong() const
    bool is_ulonglong() const
    bool is_double() const
    bool is_bool() const
    bool is_object() const
    bool is_array() const
    bool is_any() const
Non-generic versions of `is_` methods

    json& operator[](size_t i)
    const json& operator[](size_t i) const
Returns a reference to the value at position i in a json object or array.

    json& operator[](const std::string& name)
Returns a proxy to a keyed value. If written to, inserts or updates with the new value. If read, evaluates to a reference to the keyed value, if it exists, otherwise throws. 

    const json& operator[](const std::string& name) const
If `name` matches the name of a member in the json object, returns a reference to the json object, otherwise throws.

    object_iterator find(const std::string& name)
    object_iterator find(const char* name)
    const_object_iterator find(const std::string& name) const
    const_object_iterator find(const char* name) const
Returns an object iterator to a member whose name compares equal to `name`. If there is no such member, returns `end_member()`.

    json& at(const std::string& name)
    const json& at(const std::string& name) const
If `name` matches the name of a member in the json object, returns a reference to the json object, otherwise throws.  These have the same behavior as the corresponding `operator[]` functions, but the non-const `at` is more efficient (doesn't have to return a proxy.)

    json& at(size_t i)
    const json& at(size_t i) const
Returns a reference to the element at position `i` in a json array.  These have the same behavior as the corresponding `operator[]` functions.

    template <typename T>
    const json get(const std::string& name, T default_val) const
If `name` matches the name of a member in the json object, returns a copy of the json object, otherwise returns a copy of `default_val`.

    template <typename T>
    T as() const
Attempts to coerce the json value to the template type

    as<bool>
Returns `false` if value is `false` or `null`, if value is a zero length string, or if value is a zero length array or object. Everything else returns `true`.

    as<double>
If value is double, returns value, if value is signed or unsigned integer, casts to double, if value is `null`, returns `NaN`, otherwise throws.

    as<char>
    as<signed char>
    as<unsigned char>
    as<wchar_t>
    as<short>
    as<unsigned short> 
    as<int> 
    as<unsigned int> 
    as<long> 
    as<unsigned long> 
    as<long long> 
    as<unsigned long long> 
Return integer value if value has integral type, performs cast if value has double type, returns 1 or 0 if value has bool type, otherwise throws.

    as<string>
If value is string, returns value, otherwise returns result of `to_string`.

    bool as_bool() const noexcept
    long long as_longlong() const
    unsigned long long as_ulonglong() const
    double as_double() const
    std::string as_string() const
Non-generic versions of `as` methods

    template <typename T>
    const T& any_cast() const

    template <typename T>
    T& any_cast() 
If the value does not have type `any`, throws, otherwise casts the value back to the original type.

### Modifiers

    void clear()
Remove all elements from an array or object value, otherwise do nothing

    void remove_range(size_t from_index, size_t to_index)
Removes all elements from an array value whose index is between `from_index`, the first element to be removed, and `to_index`, one after the last element to be removed.

    void remove_member(const std::string& name)
Remove a member from a `json` object

    template <typename T>
    void set(const std::string& name, T val)

    void set(const std::string& name, const json& val)
    void set(std::string&& name, json&& val)
Inserts a new member or replaces an existing member in a json object.

    template <typename T>
    void add(T val)

    void add(const json& val)
    void add(json&& val)
Adds a new element at the end of a json array. The content of `val` is copied (or moved) to the new element.

    void add(size_t index, const json& val)
    void add(size_t index, json&& val)
Adds a new element at the specified index of a json array, shifting all elements currently at or above that index to the right.
The content of `val` is copied (or moved) to the new element.

    void swap(json& val)
Exchanges the content of the `json` value with the content of `val`, which is another `json` value.

### Relational operators

    bool operator==(const json& rhs) const
Returns `true` if two json objects compare equal, `false` otherwise.

    bool operator!=(const json& rhs) const
Returns `true` if two json objects do not compare equal, `false` otherwise.

### Serialization

    std::string to_string() const
Inserts json value into string.

    std::string to_string(const output_format& format) const
Inserts json value into string using specified [output_format](output_format).

    std::ostream& to_stream(std::ostream& os) const
Inserts json value into stream with default output format.

    std::ostream& to_stream(std::ostream<Char> os, const output_format& format) const
Inserts json value into stream using specified [output_format](output_format).

    void to_stream(json_output_handler& handler) const
Reports JSON related events for JSON objects, arrays, object members and array elements to a [json_output_handler](json_output_handler), such as a [json_serializer](json_serializer).

### Non member functions

    std::istream& operator>> (std::istream& os, json& val)
Reads a `json` value from a stream.

    std::ostream& operator<< (std::ostream& os, const json& val)
Inserts json value into stream.

    std::ostream& print(const json& val)  
    std::ostream& print(const json& val, const output_format<Char>& format)  
Inserts json value into stream using the specified [output_format](output_format) if supplied.

    std::ostream& pretty_print(const json& val)  
    std::ostream& pretty_print(const json& val, const output_format<Char>& format)  
Inserts json value into stream using the specified [output_format](output_format) if supplied.

    void swap(json& a, json& b)
Exchanges the values of `a` and `b`

Deprecated:

    const json& get(const std::string& name) const
Use the version of `get` with two parameters and explicitly specify a json null default value 

    bool has_member(const std::string& name) const
Use count(const std::string& name) instead.

    object_iterator begin_members()
    const_object_iterator begin_members() const
Use members().begin() instead.

    object_iterator end_members()
    const_object_iterator end_members() const
Use members().end() instead.

    array_iterator begin_elements()
    const_array_iterator begin_elements() const
Use elements().begin() instead.

    array_iterator end_elements()
    const_array_iterator end_elements() const
Use elements().end() instead.

    bool is_empty() const
Use `empty` instead

    static json parse_string(const std::string& s)
    static json parse_string(const std::string& s, 
                             parse_error_handler& err_handler)
Use `parse` instead

    void resize_array(size_t n)
Use `resize` instead. 

    void resize_array(size_t n, const json& val)
Use `resize` instead. 

### Member constants

    null
Constant json null value. Use assignment to `jsoncons::null_type()` or `json::null_type()` instead.

    an_object
Empty constant json array value. Use the default constructor `json()` instead.

    an_array
Empty constant json array value.  Use assignment to `json::array()` or `json::make_array()` instead.


### Examples

### Accessors and defaults

    json obj;

    obj["field1"] = 1;
    obj["field3"] = "Toronto";

    double x1 = obj.count("field1") > 0 ? obj["field1"].as<double>() : 10.0;
    double x2 = obj.count("field2") > 0 ? obj["field2"].as<double>() : 20.0;

    std::string x3 = obj.get("field3","Montreal").as<std::string>();
    std::string x4 = obj.get("field4","San Francisco").as<std::string>();

    std::cout << "x1=" << x1 << std::endl;
    std::cout << "x2=" << x2 << std::endl;
    std::cout << "x3=" << x3 << std::endl;
    std::cout << "x4=" << x4 << std::endl;

The output is

    x1=1
    x2=20
    x3=Toronto
    x4=San Francisco

### Nulls

    json obj;
    obj["field1"] = json::null_type();
    std::cout << obj << std::endl;

The output is 

    {"field1":null}

### Array

    json arr = json::make_array();
    arr.add(10);
    arr.add(20);
    arr.add(30);

    std::cout << arr << std::endl;

The output is 

    [10,20,30]

### Array from std::vector

    std::vector<int> v;
    v.push_back(10);
    v.push_back(20);
    v.push_back(30);

    json arr(v.begin(),v.end());

    std::cout << arr << std::endl;

The output is 

    [10,20,30]

### Object iterator

    json obj;
    obj["city"] = "Toronto";
    obj["province"] = "Ontario";
    obj["country"] = "Canada";

    for (auto it = obj.members().begin(); it != obj.members().end(); ++it)
    {
        std::cout << it->name() << "=" << it->value().as<std::string>() << std::endl;
    }

The output is

    city=Toronto
    country=Canada
    province=Ontario

### Array iterator

    json arr = json::make_array();
    arr.add("Toronto");
    arr.add("Vancouver");
    arr.add("Montreal");

    for (auto it = arr.elements().begin(); it != arr.elements().end(); ++it)
    {
        std::cout << it->as<std::string>() << std::endl;
    }

The output is

    Toronto
    Vancouver 
    Montreal

### Constructing json structures

	json root;

    root["persons"] = json::make_array();

    json person;
    person["first_name"] = "John";
    person["last_name"] = "Smith";
    person["birth_date"] = "1972-01-30";
    
    json address;
    address["city"] = "Toronto";
    address["country"] = "Canada";
    
    person["address"] = std::move(address);

    root["persons"].add(std::move(person));

    std::cout << pretty_print(root) << std::endl;

The output is

    {
        "persons":
        [
            {
                "address":
                {
                    "city":"Toronto",
                    "country":"Canada"
                },
                "birth_date":"1972-01-30",
                "first_name":"John",
                "last_name":"Smith"
            }
        ]
    }

### Default NaN, inf and -inf replacement

    json obj;
    obj["field1"] = std::sqrt(-1.0);
    obj["field2"] = 1.79e308*1000;
    obj["field3"] = -1.79e308*1000;
    std::cout << obj << std::endl;

The output is

    {"field1":null,"field2":null,"field3":null}

### Custom NaN, inf and -inf replacement

    json obj;
    obj["field1"] = std::sqrt(-1.0);
    obj["field2"] = 1.79e308*1000;
    obj["field3"] = -1.79e308*1000;

    output_format format;
    format.nan_replacement("null");
    format.pos_inf_replacement("1e9999");
    format.neg_inf_replacement("-1e9999");

    std::cout << print(obj,format) << std::endl;

The output is

    {"field1":null,"field2":1e9999,"field3":-1e9999}

### Suppressing NaN, inf and -inf replacement

    json obj;
    obj["field1"] = std::sqrt(-1.0);
    obj["field2"] = 1.79e308*1000;
    obj["field3"] = -1.79e308*1000;

    output_format format;
    format.replace_nan(false);
    format.replace_inf(false);

    std::cout << print(obj,format) << std::endl;

The (illegal) json output produced by Visual Studio 2010 is

    {"field1":-1.#IND,"field2":1.#INF,"field3":-1.#INF}

