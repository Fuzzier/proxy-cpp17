/**
 * @file
 *
 * @brief Proxy.
 *
 * This is a rewrite of Microsoft proxy library in C++17.
 * It is a working version, but not all functions have been ported.
 *
 * @author  Mingxin Wang
 * @author  Ported by Wei Tang <gauchyler@uestc.edu.cn>
 * @date    2025-03-13
 *
 * @copyright
 *   Copyright (c) Microsoft Corporation.
 *   Licensed under the MIT License.
 */

#ifndef PROXY_HPP__49B6AD65_E042_4FA8_9E5E_407FC8D2809A
#define PROXY_HPP__49B6AD65_E042_4FA8_9E5E_407FC8D2809A

#include <new> // launder
#include <tuple>
#include <limits>
#include <memory> // destroy_at, allocator_traits
#include <utility> // move, forward
#include <type_traits>
#include <initializer_list>
#include <cstddef> // nullptr_t, size_t, offsetof
#include <cassert>


////////////////////////////////////////
// For C++17 and earlier versions.
#if (__cplusplus < 202002L)
namespace std {

template<class T, class... Args>
constexpr T* construct_at(T* p, Args&&... args)
{
    return ::new (static_cast<void*>(p)) T(std::forward<Args>(args)...);
}

} // namespace std
#endif // (__cplusplus < 202002L)


namespace nsfx {
namespace pro {


////////////////////////////////////////////////////////////////////////////////
#if !defined(NSFX_ENFORCE_EBO)
# if defined(_MSC_VER)
/// Enforce empty base class optimization.
#  define NSFX_ENFORCE_EBO __declspec(empty_bases)
# else
#  define NSFX_ENFORCE_EBO
# endif  // defined(_MSC_VER)
#endif // !defined(NSFX_ENFORCE_EBO)


////////////////////////////////////////////////////////////////////////////////
/**
 * @brief Proxy.
 *
 * # Proxy
 *
 * A *proxy* is an object that provides polymorphism, that is, it exposes a set
 * of operations on behalf of an object, while hidding the actual object and
 * the implementations of the operations.
 *
 * In other words, a proxy provides an interface that consists of a set of
 * *accessor* functions (member functions of the proxy), and encapsulates
 * the type of the underlying object (also called the *proxiable* object) and
 * the implementations of the accessor functions.
 *
 * 1. It provides a set of *accessor* functions.
 * 2. It holds a type-erased reference to the *proxiable* object.
 * 3. The *accessor* functions invoke the actual functions to operate
 *    the *proxiable* object.
 *
 * The *proxy* holds the following data:
 * * A storage (array of bytes) that holds the type-erased *proxiable* object.
 * * A set of *accessor* functions.
 *   - The accessor functions are conceptually *virtual* functions.
 *   - Each accessor function has a predefined name and signature.
 *   - The set of accessor forms an interface that is implemented by the proxy.
 * * A *meta*, that is, a table of *dispatcher* function pointers.
 *   - The meta is conceptually a *virtual* function table.
 *   - Each dispatcher function pointer points to a dispatcher function.
 *   - Each dispatcher function recovers the type of the proxiable object
 *     stored in the proxy, and invokes the actual function that operates upon
 *     the proxiable object.
 *
 * @verbatim
 *    virtual functions                           locates meta_ and ptr_
 * +--------------------+      +----------------+ invokes disptacher function pointer
 * | accessor functions | ---> | int foo(float) | --------------+
 * +--------------------+      +----------------+               |
 *                             | int bar(int)   |               |
 *                             +----------------+               |
 * virtual function table                                       v
 * +--------------------+      +-----------------------------------+ points to dispatcher function
 * |  meta_ : meta_ptr  | ---> | int (foo_dispatcher*)(ptr, float) | -------+
 * +--------------------+      +-----------------------------------+        |
 *                             | int (bar_dispatcher*)(ptr, int)   |        |
 *                             +-----------------------------------+        |
 *   type-erased object                                                     |
 * +--------------------+                                                   |
 * | ptr_ : std::byte[] |                                                   |
 * +--------------------+                                                   |
 *   ^                                                                      |
 *   |                                                                      |
 *   | recovers object type P  +-----------------------------------+        |
 *   +------------------------ | int foo_dispatcher(ptr, float)    | <------+
 *   | invokes actual callable +-----------------------------------+
 *   |                         | int bar_dispatcher(ptr, int)      |
 *   |                         +-----------------------------------+
 *   v
 * +-------------------+
 * | int P::foo(float) |
 * +-------------------+
 * | int bar(P, int)   |
 * +-------------------+
 * @endverbatim
 *
 * # Callable
 *
 * A callable may be a member function, or a free function.
 * A callable is identified by three pieces of meta information:
 * 1. The name of the member function.
 * 2. The overload (qualified signature) of the member function.
 * 3. The object type under operation.
 *
 * @verbatim
 * struct A
 * {
 *     bool foo(int, float);
 *     // name: foo
 *     // overload (qualified signature): bool(int, float)
 *     // `this` object type: A
 * };
 * @endverbatim
 *
 * # Dispatch (D)
 *
 * ## What is a dispatch?
 *
 * A *dispatch* is a type that represents (binds to) the name of a callable.
 * It is an abstraction of the *name* of a callable.
 *
 * * Member dispatch
 *   A member dispatch represents the name of a member function.
 *
 * * Free dispatch
 *   A member dispatch represents the name of a free function.
 *
 * ## How to invoke a dispatch?
 *
 * If the function is a member function, then a reference to an object
 * and a pack of arguments are required to invoke the dispatch.
 *
 * If the function is a free function, then a pack of arguments are required
 * to invoke the dispatch.
 *
 * ## How to define a dispatch?
 *
 * * `NSFX_DEF_MEM_DISPATCH()`  : binds to the name of a member function.
 * * `NSFX_DEF_FREE_DISPATCH()` : binds to the name of a free function.
 *
 * Since a dispatch is a class with member templates, it can be defined
 * in namespace or class, but not in function scope.
 *
 * # Proxiable (P)
 *
 * ## What is a proxiable type?
 *
 * A *proxiable* type is the type of the object that provides operations.
 * * The object type that provides the member functions.
 * * The 1st argument type of the free functions.
 *
 * # Overload (O)
 *
 * ## What is an overload?
 *
 * An *overload* is a qualified signature type.
 * It is an abstraction of an *operation*, that is, the input parameter types and
 * the return type of an operation.
 *
 * The overload determines the *name-erased* form of an operation, that is,
 * a function pointer.
 *
 * It provides the following meta information:
 * * The types of input parameters.
 * * The type of return value.
 * * The `noexcept` specification.
 * * The `const` and `ref` qualifications (for member functions).
 *
 * It has one of the following forms:
 * * `R(Args...)`
 * * `R(Args...) noexcept`
 * * `R(Args...) &`
 * * `R(Args...) & noexcept`
 * * `R(Args...) &&`
 * * `R(Args...) && noexcept`
 * * `R(Args...) const`
 * * `R(Args...) const noexcept`
 * * `R(Args...) const&`
 * * `R(Args...) const& noexcept`
 * * `R(Args...) const&&`
 * * `R(Args...) const&& noexcept`
 *
 * ## Member function
 *
 * The overload of a member function **does not** include the type of
 * `this` pointer.
 *
 * The object type that provides the member function is to be proxied.
 *
 * For example,
 * ~~~
 * struct P
 * {
 *     // The overload is `void(int, float)`.
 *     void foo(int, float);
 * };
 * ~~~
 *
 * ## Free function
 *
 * The overload of a free function **does not** include the type of
 * the *1st argument/operand*.
 *
 * The 1st argument/operand type of a free function is to be proxied.
 *
 * For example,
 * ~~~
 * // The overload is `void(int, float)`.
 * void foo(P&, int, float);
 * ~~~
 *
 * # Dispatcher
 *
 * A *dispatcher* is an implementation function that invokes the actual callable.
 *
 * The dispatcher type is a function pointer type that looks like:
 * ~~~
 * R(*)(std::byte&, Args...)
 * ~~~
 * The 1st argument is the storage that holds the type-erased proxiable object.
 *
 * The dispatcher is deduced from D, O, P:
 *
 * @verbatim
 * D + O + P => dispatcher function
 * @endverbatim
 *
 * * `D` and `O` give the name and signature of the callable.
 *   e.g., the name and signature of the member function of the proxiable `P`,
 *   or the name and signature of the free function that operates upon `P`.
 * * `P` gives the type of the proxiable object.
 *
 * The dispatcher adopts `P` to recover the type of the type-erased proxiable
 * object, and uses `D` and `O` to invoke the member function of the proxiable
 * object, or the free function that operates upon the proxiable object.
 *
 * ## Dispatcher pointer
 *
 * The type of the dispatcher function is totally determined by the overload,
 * which is the name-erased and type-erased form of a callable.
 *
 * @verbatim
 * O => dispatcher type (function pointer)
 * @endverbatim
 *
 * For example:
 * ~~~
 * The dispatcher type of `foo` is `void(*)(std::byte&, int, float)`.
 * ~~~
 *
 * # Accessor (A)
 *
 * An *accessor* function is a member function that a proxy exposes on behave of
 * the underlying proxiable object.
 *
 * The name of the accessor function is determined by the dispatch type `D`,
 * while the signature of the accessor function is given by the overload type `O`.
 *
 * The dispatch type `D`:
 * * Not only gives the name of the actual callable (the function that operates
 *   upon the proxiable object).
 * * But also gives the name of the accessor (interface) function of the proxy
 *   that invokes the actual callable.
 *
 * @verbatim
 * D + O => A
 * @endverbatim
 *
 * The set of accessor functions consists of the interface that a proxy promises
 * to implement.
 *
 * @verbatim
 * D + Os... => As... = interface of the proxy
 * @endverbatim
 *
 * An accessor class template is provided by the dispatch type `D`.
 * The accessor class provides a *named* member function that invokes
 * the dispatcher function pointer, which invokes the actual callable.
 *
 * The accessors classes are parent classes of a proxy, thus a proxy inherits
 * the *named* member functions.
 * * The accessor classes `As...` are merged into `composite_accessor<As...>`.
 *
 * # Meta provider (MP)
 *
 * A meta provider is a class template that generate a dispatcher function
 * from a proxiable type `P`.
 *
 * @verbatim
 * D + O => MP (meta provider)
 * D + O + P => MP + P => dispatcher function
 * @endverbatim
 *
 * ~~~
 * overload_traits<O>::meta_provider<D>::get<P>()
 * ~~~
 *
 * The meta provider is embedded in `overload_traits<>`, since the argument and
 * return types are visible there.
 *
 * # Dispatcher meta (M)
 *
 * A dispatcher meta is a table that holds a single dispatcher function pointer.
 *
 * @verbatim
 * D + O => MP => M
 * M + P => dispatcher function
 * @endverbatim
 *
 * # Meta
 *
 * A *meta* is a table of dispatcher function pointers.
 * * It is a composite of dispatcher metas.
 * * It is a table of dispatcher function pointers that invokes the callables.
 * * The dispatcher meta are merged into `composite_meta<Ms...>`.
 *
 * @verbatim
 * Ms... => meta
 * Ms... + P => instance of a meta
 * @endverbatim
 *
 * # Convention (C)
 *
 * A *convention* represents a set of accessor functions that:
 * * Have the same name, determined by the dispatch type `D`.
 * * Have different signature, determined by the overload types `Os`.
 *
 * @verbatim
 * C = D + Os... => As...
 * @endverbatim
 *
 * The `composite_accessor`s deduced by the conventions are further combined into
 * a larger `composite_accessor`.
 *
 * @verbatim
 * Cs... => As... = interface of the proxy
 * @endverbatim
 *
 * Each convention also deduces a set of dispatcher metas `Ms...`, which
 * are merged into a `composite_meta<Ms...>`.
 * * It is a table of dispatcher function pointers that invokes the callables
 *   with the same name and different signatures.
 *
 * @verbatim
 * C = D + Os... => Ms...
 * @endverbatim
 *
 * The `composite_meta`s deduced by the conventions are further combined into
 * a larger `composite_meta`.
 * * It is a table of dispatcher function pointers that invokes the callables
 *   with the different names or different signatures.
 *
 * @verbatim
 * Cs... => Ms...
 * @endverbatim
 *
 * # Facade (F)
 *
 * A facade is a class that holds a set of convetions.
 * * It determines the accessor functions of the proxy.
 * * It determines the meta of the proxy.
 *
 * @verbatim
 * F = Cs... => As... = interface of the proxy
 * F = Cs... => Ms... = virtual table of the proxy
 * @endverbatim
 *
 * @verbatim
 * F => proxy
 * @endverbatim
 */
template<class F> class proxy;


////////////////////////////////////////
enum class constraint_level { none, nontrivial, nothrow, trivial };

enum class diagnose_level { none, assert };

namespace details {


////////////////////////////////////////////////////////////////////////////////
using ptr_prototype = void*[2];


////////////////////////////////////////
struct copy_t {};
struct move_t {};


////////////////////////////////////////////////////////////////////////////////
template<std::size_t x>
struct is_pow2
{
    static constexpr bool value = ((x & 1) == 0u) && is_pow2<(x >> 1)>::value;
};

template<> struct is_pow2<0> { static constexpr bool value = false; };
template<> struct is_pow2<1> { static constexpr bool value = true;  };

template<size_t x>
inline constexpr bool is_pow2_v = is_pow2<x>::value;


////////////////////////////////////////////////////////////////////////////////
struct proxiable_ptr_constraints
{
    std::size_t max_size;
    std::size_t max_align;
    constraint_level copyability;
    constraint_level relocatability;
    constraint_level destructibility;
};


template<std::size_t max_size_  = sizeof(ptr_prototype),
         std::size_t max_align_ = sizeof(ptr_prototype),
         constraint_level copyability_     = constraint_level::none,
         constraint_level relocatability_  = constraint_level::nothrow,
         constraint_level destructibility_ = constraint_level::nothrow>
struct proxiable_ptr_constraints_impl
{
    static constexpr std::size_t max_size  = max_size_;
    static constexpr std::size_t max_align = max_align_;
    static constexpr constraint_level copyability     = copyability_;
    static constexpr constraint_level relocatability  = relocatability_;
    static constexpr constraint_level destructibility = destructibility_;

    static_assert(is_pow2_v<max_size>);
    static_assert(max_size % max_align == 0u);

    template<std::size_t max_size, std::size_t max_align = max_size>
    using restrict_layout = proxiable_ptr_constraints_impl<
                            max_size,
                            max_align,
                            copyability,
                            relocatability,
                            destructibility>;

    template<constraint_level level>
    using support_copy = proxiable_ptr_constraints_impl<
                            max_size,
                            max_align,
                            level,  // copyability < level ? level : copyability,
                            relocatability,
                            destructibility>;

    template<constraint_level level>
    using support_relocation = proxiable_ptr_constraints_impl<
                            max_size,
                            max_align,
                            copyability,
                            level, // relocatability < level ? level : relocatability,
                            destructibility>;

    template<constraint_level level>
    using support_destruction = proxiable_ptr_constraints_impl<
                            max_size,
                            max_align,
                            copyability,
                            relocatability,
                            level>; // destructibility < level ? level : destructibility>;
};


////////////////////////////////////////////////////////////////////////////////
struct applicable_traits   { static constexpr bool applicable = true;  };
struct inapplicable_traits { static constexpr bool applicable = false; };


////////////////////////////////////////////////////////////////////////////////
 /**
  * @brief The qualifier type of an overload type.
  */
enum class qualifier_type
{
    lv,
    rv,
    const_lv,
    const_rv,
};

template<class T, qualifier_type Q>
struct add_qualifier;

template<class T>
struct add_qualifier<T, qualifier_type::lv>
{
    static_assert(!std::is_reference_v<T>);
    using type = T&;
};

template<class T>
struct add_qualifier<T, qualifier_type::rv>
{
    static_assert(!std::is_reference_v<T>);
    using type = T&&;
};

 /**
  * @brief The qualifier type of an overload type.
  *
  * @remark
  *   There is a pitfall when `T` is a *reference* type.
  *   For example,
  *   ~~~
  *   constexpr auto Q = qualifier_type::const_lv;
  *   using X = int&;
  *   using Y = add_qualifier_t<X, Q>; // using Y = int&;
  *   // Suprisingly, `const T` is applied as `int& const`, which is `int&`.
  *   // That is, `const` is applied to the reference type, not the value type.
  *   // `const T&` is applied as `int& const&`, which is `int&`.
  *   static_assert(std::is_same_v<Y, int&>);
  *   static_assert(!std::is_same_v<Y, const int&>);
  *   ~~~
  * @remark
  *   However, `std::decay_t<>` is not required, since constructors of `proxy<F>`
  *   have already used `std::decay_t<>` to construct `composite_accessor_meta`.
  */
template<class T>
struct add_qualifier<T, qualifier_type::const_lv>
{
    static_assert(!std::is_reference_v<T>);
    using type = const T&;
};

template<class T>
struct add_qualifier<T, qualifier_type::const_rv>
{
    static_assert(!std::is_reference_v<T>);
    using type = const T&&;
};

/**
 * @brief Add qualifier to an object type to invoke qualified member function.
 *
 * @tparam T The object type.
 * @tparam Q The qualifier.
 *
 * @code{.cpp}
 * struct X
 * {
 *     void foo(int) &;
 *     void foo(int) const&;
 *     void foo(int) && noexcept;
 * };
 *
 * int bar(int i)
 * {
 *     X x;
 *     // reinterpret_cast<X&>(x).foo(i);
 *     reinterpret_cast<add_qualifier_t<X, qualifier_type::lv>>(x).foo(i);
 *     // reinterpret_cast<X const&>(x).foo(i);
 *     reinterpret_cast<add_qualifier_t<X, qualifier_type::const_lv>>(x).foo(i);
 *     // reinterpret_cast<X&&>(x).foo(i);
 *     reinterpret_cast<add_qualifier_t<X, qualifier_type::rv>>(x).foo(i);
 * }
 * @endcode
 */
template<class T, qualifier_type Q>
using add_qualifier_t = typename add_qualifier<T, Q>::type;

/**
 * @brief Add qualifier to the object pointed by a pointer.
 *
 * @tparam T The object type.
 * @tparam Q The qualifier.
 *
 * @code{.cpp}
 * struct X {};
 *
 * // using X_ptr1 = X*;
 * using X_ptr1 = add_qualifier_ptr_t<X, qualifier_type::lv>;
 *
 * // using X_ptr2 = X*;
 * using X_ptr2 = add_qualifier_ptr_t<X, qualifier_type::rv>;
 *
 * // using X_const_ptr1 = const X*;
 * using X_const_ptr1 = add_qualifier_ptr_t<X, qualifier_type::const_lv>;
 *
 * // using X_const_ptr2 = const X*;
 * using X_const_ptr2 = add_qualifier_ptr_t<X, qualifier_type::const_rv>;
 * @endcode
 */
template<class T, qualifier_type Q>
using add_qualifier_ptr_t =
    std::remove_reference_t<add_qualifier_t<std::remove_reference_t<T>, Q>>*;


////////////////////////////////////////////////////////////////////////////////
template<template<class, class> class R, class O, class... Is>
struct recursive_reduction
{
    // If `Is` is empty, then `O` is returned.
    using type = O;
};

/**
 * @brief Invoke a binary meta function recursively.
 *
 * @tparam R  The binary meta function.
 * @tparam O  The initial type.
 * @tparam Is The arguments.
 *
 * If `Is` is empty, then `O` is returned.
 *
 * For example,
 * @code{.cpp}
 * template<class...Is>
 * struct composite_impl : Is... {};
 *
 * template<class O, class I>
 * struct composite_deduction;
 *
 * // merge `composite_impl<>` and `I`.
 * template<class...Is, class I>
 * struct composite_deduction<composite_impl<Is...>, I>
 * {
 *     using type = composite_impl<Is..., I>;
 * }
 *
 * // merge `composite_impl<>` and `composite_impl<>`.
 * template<class...Is, class...Js>
 * struct composite_deduction<composite_impl<Is...>, composite_impl<Js...>>
 * {
 *     using type = composite_impl<Is..., Js...>;
 * }
 *
 * template<class...Is>
 * using composite_deduction_t = typename composite_deduction<Is...>::type;
 *
 * // `Is` can be a base class or `composite_impl<>`.
 * template<class Is...>
 * using composite_t = recursive_reduction_t<
 *                     composite_deduction_t, composite_impl<>, Is>;
 * @endcode
 */
template<template<class, class> class R, class O, class... Is>
using recursive_reduction_t = typename recursive_reduction<R, O, Is...>::type;

template<template<class, class> class R, class O, class I, class... Is>
struct recursive_reduction<R, O, I, Is...>
{
    using type = recursive_reduction_t<R, R<O, I>, Is...>;
};


////////////////////////////////////////
template<class T, class = void>
struct has_tuple_size_concept_impl : std::false_type {};

template<class T>
struct has_tuple_size_concept_impl<T,
    std::enable_if_t<(
        /* `std::tuple_size_v<T>` is a `size_t` constant */
        (std::is_same_v<decltype((std::tuple_size<T>::value)), const std::size_t&>) &&
        /* `std::tuple_size_v<T>` is evaluable at compile time */
        ((void)std::tuple_size<T>::value, true)),
        void>>
    : std::true_type {};

template<class T>
inline constexpr bool has_tuple_size_concept =
        has_tuple_size_concept_impl<T>::value;

////////////////////////////////////////
template<class T, std::size_t I, class = void>
struct has_tuple_element_concept_impl : std::false_type {};

template<class T, std::size_t I>
struct has_tuple_element_concept_impl<T, I,
    std::enable_if_t<(
        (has_tuple_size_concept<T>) &&
        (I < std::tuple_size<T>::value)),
        void>>
{
    template<class _ = void, class = void>
    struct impl : std::false_type {};

    template<class _>
    struct impl<_,
    std::enable_if_t<
        (std::is_void_v<_>),
        std::void_t<std::tuple_element_t<I, T>>>>
    : std::true_type {};

    static constexpr bool value = impl<>::value;
};

template<class T, std::size_t I>
inline constexpr bool has_tuple_element_concept =
        has_tuple_element_concept_impl<T, I>::value;

////////////////////////////////////////
template<class T, class = void>
struct is_tuple_like_well_formed_concept_impl : std::false_type {};

template<class T>
struct is_tuple_like_well_formed_concept_impl<T,
    std::enable_if_t<has_tuple_size_concept<T>, void>>
{
    template<class Is>
    struct has_tuple_elements;

    template<std::size_t...Is>
    struct has_tuple_elements<std::index_sequence<Is...>>
        : std::bool_constant<(has_tuple_element_concept<T, Is> && ...)> {};

    static constexpr bool value = has_tuple_elements<
        std::make_index_sequence<std::tuple_size_v<T>>>::value;
};

template<class T>
inline constexpr bool is_tuple_like_well_formed_concept =
        is_tuple_like_well_formed_concept_impl<T>::value;

////////////////////////////////////////
template<template<class...> class T, class TL, class Is, class...Args>
struct instantiated_impl;

template<template<class...> class T, class TL, size_t...Is, class...Args>
struct instantiated_impl<T, TL, std::index_sequence<Is...>, Args...>
{
    using type = T<Args..., std::tuple_element_t<Is, TL>...>;
};

/**
 * @brief Invoke a meta function by a list of types and extra arguments.
 *
 * @tparam T    The meta function to invoke.
 * @tparam TL   The type list, i.e., `std::tuple<>`.
 * @tparam Is   The indices of elements.
 * @tparam Args The extra type arguments.
 *
 * @code{.cpp}
 * struct T0 {};
 * struct T1 {};
 * struct T2 {};
 * struct A0 {};
 * struct A1 {};
 *
 * template<class...Ts>
 * struct foo : Ts... {};
 *
 * using TL = std::tuple<T0, T1, T2>;
 *
 * // using result T<A0, A1, T0, T1, T2>;
 * using result = invoke_via_type_list_t<foo, TL, A0, A1>;
 * @endcode
 */
template<template<class...> class T, class TL, class...Args>
using instantiated_t = typename instantiated_impl<
    T, TL, std::make_index_sequence<std::tuple_size_v<TL>>, Args...>::type;


////////////////////////////////////////////////////////////////////////////////
/**
 * @brief Test the consistency of copyability constraint level.
 *
 * @tparam T     The object type.
 * @tparam level The constraint level.
 *
 * If `T` **does not** have the specified copyability constraint level,
 * this meta function returns `false`.
 */
template<class T, constraint_level level, class = void>
struct has_copyability_concept_impl : std::false_type {};

template<class T, constraint_level level>
struct has_copyability_concept_impl<T, level,
    std::enable_if_t<level == constraint_level::none, void>>
    : std::true_type {};

template<class T, constraint_level level>
struct has_copyability_concept_impl<T, level,
    std::enable_if_t<level == constraint_level::nontrivial &&
                     std::is_copy_constructible_v<T>,
                     void>>
    : std::true_type {};

template<class T, constraint_level level>
struct has_copyability_concept_impl<T, level,
    std::enable_if_t<level == constraint_level::nothrow &&
                     std::is_nothrow_copy_constructible_v<T>,
                     void>>
    : std::true_type {};

template<class T, constraint_level level>
struct has_copyability_concept_impl<T, level,
    std::enable_if_t<level == constraint_level::trivial &&
                     std::is_trivially_copy_constructible_v<T> &&
                     std::is_trivially_destructible_v<T>,
                     void>>
    : std::true_type {};

template<class T, constraint_level level>
inline constexpr bool has_copyability_concept =
        has_copyability_concept_impl<T, level>::value;

////////////////////////////////////////
/**
 * @brief Test the consistency of movability constraint level.
 *
 * @tparam T     The object type.
 * @tparam level The constraint level.
 *
 * If `T` **does not** have the specified movability constraint level,
 * this meta function returns `false`.
 */
template<class T, constraint_level level, class = void>
struct has_relocatability_concept_impl : std::false_type {};

template<class T, constraint_level level>
struct has_relocatability_concept_impl<T, level,
    std::enable_if_t<level == constraint_level::none, void>>
    : std::true_type {};

template<class T, constraint_level level>
struct has_relocatability_concept_impl<T, level,
    std::enable_if_t<level == constraint_level::nontrivial &&
                     std::is_move_constructible_v<T> &&
                     std::is_destructible_v<T>,
                     void>>
    : std::true_type {};

template<class T, constraint_level level>
struct has_relocatability_concept_impl<T, level,
    std::enable_if_t<level == constraint_level::nothrow &&
                     std::is_nothrow_move_constructible_v<T> &&
                     std::is_nothrow_destructible_v<T>,
                     void>>
    : std::true_type {};

template<class T, constraint_level level>
struct has_relocatability_concept_impl<T, level,
    std::enable_if_t<level == constraint_level::trivial &&
                     std::is_trivially_move_constructible_v<T> &&
                     std::is_trivially_destructible_v<T>,
                     void>>
    : std::true_type {};

template<class T, constraint_level level>
inline constexpr bool has_relocatability_concept =
        has_relocatability_concept_impl<T, level>::value;

////////////////////////////////////////
/**
 * @brief Test the consistency of destructibility constraint level.
 *
 * @tparam T     The object type.
 * @tparam level The constraint level.
 *
 * If `T` **does not** have the specified destructibility constraint level,
 * this meta function returns `false`.
 */
template<class T, constraint_level level, class = void>
struct has_destructibility_concept_impl : std::false_type {};

template<class T, constraint_level level>
struct has_destructibility_concept_impl<T, level,
    std::enable_if_t<level == constraint_level::none, void>>
    : std::true_type {};

template<class T, constraint_level level>
struct has_destructibility_concept_impl<T, level,
    std::enable_if_t<level == constraint_level::nontrivial &&
                     std::is_destructible_v<T>,
                     void>>
    : std::true_type {};

template<class T, constraint_level level>
struct has_destructibility_concept_impl<T, level,
    std::enable_if_t<level == constraint_level::nothrow &&
                     std::is_nothrow_destructible_v<T>,
                     void>>
    : std::true_type {};

template<class T, constraint_level level>
struct has_destructibility_concept_impl<T, level,
    std::enable_if_t<level == constraint_level::trivial &&
                     std::is_trivially_destructible_v<T>,
                     void>>
    : std::true_type {};

template<class T, constraint_level level>
inline constexpr bool has_destructibility_concept =
        has_destructibility_concept_impl<T, level>::value;

////////////////////////////////////////
template<class T>
class destruction_guard
{
public:
    explicit destruction_guard(T* p) noexcept : p_{p} {}
    destruction_guard(const destruction_guard&) = delete;
    ~destruction_guard(void) noexcept(std::is_nothrow_destructible_v<T>)
    {
        std::destroy_at(p_);
    }

private:
    T* p_;
};


////////////////////////////////////////////////////////////////////////////////
/**
 * @brief Pointer traits.
 *
 * @tparam P  The proxiable type.
 * @tparam Q  The qualifier.
 * @tparam NE Is noexcept?
 *
 * `P` is applicable, if the following conditions are satisfied:
 * 1. The qualified `P` is dereferencable; AND
 * 2. The dereference operation is nothrow if `NE` is `true`.
 */
template<class P, qualifier_type Q, bool NE, class = void>
struct ptr_traits : inapplicable_traits {};

/**
 * @brief Pointer traits.
 *
 * @tparam P  The proxiable type.
 * @tparam Q  The qualifier.
 * @tparam NE Is noexcept?
 *
 * `P` can be:
 * * A raw pointer type.
 * * A smart pointer type.
 *   For example:
 *   + `std::unique_ptr`
 *   + `std::shared_ptr`
 *   + `intrusive_ptr`
 * * An iterator type.
 * * et al.
 */
template<class P, qualifier_type Q, bool NE>
struct ptr_traits<P, Q, NE,
    std::enable_if_t<
        (!NE || noexcept(*std::declval<add_qualifier_t<P, Q>>())),
        std::void_t<decltype(*std::declval<add_qualifier_t<P, Q>>())>>>
    : applicable_traits
{
    /**
     * @brief The reference type deduced by dereferencing qualified `P`.
     */
    using target_type = decltype(*std::declval<add_qualifier_t<P, Q>>());
};

////////////////////////////////////////
/**
 * @brief The type of function pointer.
 *
 * @tparam NT   Is nothrow?
 * @tparam R    The return type.
 * @tparam Args The argument types.
 */
template<bool NE, class R, class... Args>
using func_ptr_t = std::conditional_t<
    NE, R (*)(Args...) noexcept, R (*)(Args...)>;

////////////////////////////////////////
/**
 * @brief The dispatcher function which invokes a named function.
 *
 * @tparam D  The dispatch type that binds to the name of a function.
 * @tparam P  The proxiable type.
 * @tparam Q  The qualifier.
 * @param[in] ptr  The type-erased pointer to the object.
 * @param[in] args The argument types of the member function.
 */
template<class D, class R, class... Args>
R invoke_dispatch(Args&&... args)
    noexcept(std::is_nothrow_invocable_r_v<R, D, Args...>)
{
    if constexpr (std::is_void_v<R>)
    {
        // If D{}(...) has a non-void return type, and `R` is `void`,
        // then the return value is ignored.
        D{}(std::forward<Args>(args)...);
    }
    else
    {
        return D{}(std::forward<Args>(args)...);
    }
}

/**
 * @brief Check whether `D` is invocable as specified.
 *
 * @tparam D  The dispatch type.
 * @tparam NE Is noexcept?
 */
template<class D, bool NE, class R, class...Args>
inline constexpr bool invocable_dispatch_concept =
    ( NE && std::is_nothrow_invocable_r_v<R, D, Args...>) ||
    (!NE && std::is_invocable_r_v<R, D, Args...>);

/**
 * @brief Check whether `D` is invocable as specified.
 *
 * @tparam P The proxiable type.
 *
 * `indirect`: the proxy holds a pointer to an object, and the pointer is
 *           dereferenced to invoke its method.
 */
template<class D, class P, qualifier_type Q, bool NE, class R, class... Args>
struct invocable_dispatch_ptr_indirect_concept_impl
{
    template<class _ = void, class = void>
    struct impl : std::false_type {};

    template<class _>
    struct impl<_,
    std::enable_if_t<(
        (std::is_void_v<_>) &&
        (ptr_traits<P, Q, NE>::applicable)),
        void>>
    {
        static constexpr bool value =
            invocable_dispatch_concept<D, NE, R,
                typename ptr_traits<P, Q, NE>::target_type, Args...>;
    };

    static constexpr bool value = impl<>::value;
};

template<class D, class P, qualifier_type Q, bool NE, class R, class... Args>
inline constexpr bool invocable_dispatch_ptr_indirect_concept =
    invocable_dispatch_ptr_indirect_concept_impl<D, P, Q, NE, R, Args...>::value;

/**
 * @tparam P The proxiable type.
 *
 * `direct`: the proxy holds an object, and its method is invoked directly,
 *         i.e., without dereferencing a pointer.
 */
template<class D, class P, qualifier_type Q, bool NE, class R, class... Args>
inline constexpr bool invocable_dispatch_ptr_direct_concept =
    invocable_dispatch_concept<D, NE, R, add_qualifier_t<P, Q>, Args...> &&
    ((Q != qualifier_type::rv) ||
     ( NE && std::is_nothrow_destructible_v<P>) ||
     (!NE && std::is_destructible_v<P>));

////////////////////////////////////////
/**
 * @brief Invoke the dispatch when the proxy holds a pointer to an object.
 *
 * @tparam D    The dispatch type that binds to the name of a function.
 * @tparam P    The proxiable type, which is a pointer type.
 * @tparam Q    The qualifier.
 * @tparam R    The result type.
 * @tparam Args The argument types.
 *
 * @param[in] self The storage that holds the pointer.
 * @param[in] args The arguments.
 *
 * @remark
 *   The function types of `indirect_conv_dispatcher()`, `direct_conv_dispatcher()`
 *   and `default_conv_dispatcher()` are the same, since the concrete types
 *   `add_qualifier_t<P, Q>` are abstracted to `add_qualifier_t<std::byte, Q>`.
 */
template<class D, class P, qualifier_type Q, class R, class... Args>
R indirect_conv_dispatcher(add_qualifier_t<std::byte, Q> self, Args... args)
    noexcept(invocable_dispatch_ptr_indirect_concept<D, P, Q, true, R, Args...>)
{
    return invoke_dispatch<D, R>(
        *std::forward<add_qualifier_t<P, Q>>(
            // Use `std::launch()` to prevent compiler from assuming that
            // the value stored in `self` was never changed.
            *std::launder(
                // Indirect:
                // `std::byte` stores `P`, which is a pointer type.
                reinterpret_cast<add_qualifier_ptr_t<P, Q>>(&self)
            )
        ),
        std::forward<Args>(args)...
    );
}

/**
 * @brief Invoke the dispatch when the proxy holds a concrete object.
 *
 * @tparam D    The dispatch type that binds to the name of a function.
 * @tparam P    The proxiable type, which is an object type.
 * @tparam Q    The qualifier.
 * @tparam R    The result type.
 * @tparam Args The argument types.
 *
 * @param[in] self The storage that holds the object.
 * @param[in] args The arguments.
 */
template<class D, class P, qualifier_type Q, class R, class... Args>
R direct_conv_dispatcher(add_qualifier_t<std::byte, Q> self, Args... args)
    noexcept(invocable_dispatch_ptr_direct_concept<D, P, Q, true, R, Args...>)
{
    // Direct:
    // `std::byte` stores `P`, which is an object type.
    // Use `std::launch()` to prevent compiler from assuming that
    // the value stored in `self` was never changed.
    auto& qp = *std::launder(
                reinterpret_cast<add_qualifier_ptr_t<P, Q>>(&self));
    if constexpr (Q == qualifier_type::rv)
    {
        destruction_guard guard{&qp};
        return invoke_dispatch<D, R>(
            std::forward<add_qualifier_t<P, Q>>(qp),
            std::forward<Args>(args)...);
    }
    else
    {
        return invoke_dispatch<D, R>(
            std::forward<add_qualifier_t<P, Q>>(qp),
            std::forward<Args>(args)...);
    }
}

/**
 * @brief Invoke a dispatch.
 *
 * @tparam D    The dispatch type that binds to the name of a function.
 * @tparam Q    The qualifier.
 * @tparam R    The result type.
 * @tparam Args The argument types.
 *
 * @param[in] args The arguments.
 */
template<class D, qualifier_type Q, class R, class... Args>
R default_conv_dispatcher(add_qualifier_t<std::byte, Q>, Args... args)
    noexcept(invocable_dispatch_concept<D, true, R, std::nullptr_t, Args...>)
{
    return invoke_dispatch<D, R>(nullptr, std::forward<Args>(args)...);
}

/**
 * @brief The dispatcher for non-trivial copy construction.
 *
 * @tparam P The proxiable type, which should be an object type.
 */
template<class P>
void copying_dispatcher(std::byte& self, const std::byte& rhs)
    noexcept(has_copyability_concept<P, constraint_level::nothrow>)
{
    std::construct_at(reinterpret_cast<P*>(&self),
                      *std::launder(reinterpret_cast<const P*>(&rhs)));
}

/**
 * @brief The dispatcher for trivial copy construction.
 *
 * @tparam size The size of the proxiable object.
 */
template<std::size_t size, std::size_t align>
void copying_default_dispatcher(std::byte& self, const std::byte& rhs)
    noexcept
{
    // NOTE: C++17 does not support `std::assume_aligned<align>()`.
    std::uninitialized_copy_n(&rhs, size, &self);
}

/**
 * @brief The dispatcher for non-trivial move construction.
 *
 * @tparam P The proxiable type, which should be an object type.
 */
template<class P>
void relocation_dispatcher(std::byte& self, const std::byte& rhs)
    noexcept(has_relocatability_concept<P, constraint_level::nothrow>)
{
    P* other = std::launder(reinterpret_cast<P*>(const_cast<std::byte*>(&rhs)));
    destruction_guard guard{other};
    std::construct_at(reinterpret_cast<P*>(&self), std::move(*other));
}

/**
 * @brief The dispatcher for non-trivial destruction.
 */
template<class P>
void destruction_dispatcher(std::byte& self)
    noexcept(has_destructibility_concept<P, constraint_level::nothrow>)
{
    std::destroy_at(std::launder(reinterpret_cast<P*>(&self)));
}

/**
 * @brief The dispatcher for trivial destruction.
 */
inline void destruction_default_dispatcher(std::byte&) noexcept {}


////////////////////////////////////////////////////////////////////////////////
/**
 * @brief The overload traits.
 *
 * @tparam O The qualified signature.
 *
 * The overload type is applicable, if the qualified signature matches one of
 * the following forms:
 * * `R(Args...)`
 * * `R(Args...) noexcept`
 * * `R(Args...) &`
 * * `R(Args...) & noexcept`
 * * `R(Args...) &&`
 * * `R(Args...) && noexcept`
 * * `R(Args...) const`
 * * `R(Args...) const noexcept`
 * * `R(Args...) const &`
 * * `R(Args...) const & noexcept`
 * * `R(Args...) const &&`
 * * `R(Args...) const && noexcept`
 *
 * `overload_traits<O>` provides the following members:
 * * V: `applicable : bool`
 * * F: `check_applicable<diagnose_level>() -> bool`
 * * V: `qualifier : qualifier_type`
 * * V: `is_noexcept : bool`
 * * T: `return_value`
 * * T: `dispatch_type`
 * * T: `meta_provider<IS_DIRECT, D>`
 * * V: `applicable_ptr<P> : bool`
 */
template<class O>
struct overload_traits : inapplicable_traits
{
    template<diagnose_level level>
    static constexpr bool check_applicable(void)
    {
        if constexpr (level == diagnose_level::assert)
        {
            constexpr bool is_applicable = (level == diagnose_level::assert);
            static_assert(is_applicable,
            "The overload O is inapplicable.\n"
            "========================================\n"
            "| The overload type must be one of the following function types:\n"
            "|  * R(Args...)\n"
            "|  * R(Args...) noexcept\n"
            "|  * R(Args...) &\n"
            "|  * R(Args...) & noexcept\n"
            "|  * R(Args...) &&\n"
            "|  * R(Args...) && noexcept\n"
            "|  * R(Args...) const\n"
            "|  * R(Args...) const noexcept\n"
            "|  * R(Args...) const &\n"
            "|  * R(Args...) const & noexcept\n"
            "|  * R(Args...) const &&\n"
            "|  * R(Args...) const && noexcept\n"
            "========================================\n");
        }
        return applicable;
    }
};

template<bool is_dispatch_ptr_indirect_applicable, class D, class O, class P>
constexpr void assert_is_dispatch_ptr_indirect_applicable(void)
{
    static_assert(is_dispatch_ptr_indirect_applicable,
    "The dispatch D, overload O and proxiable P are inapplicable "
    "to generate dispatcher function for indirect convention\n"
    "========================================\n"
    "| * The proxiable type P shall be a dereferencable type\n"
    "|   e.g., pointer, smart pointer, iterator, ...\n"
    "| * If D is mem dispatch, P must be invocable as (*p).func(args...)\n"
    "| * If D is free dispatch, P must be invocable as func(*p, args...)\n"
    "| * Try to remove `noexcept` from the overload type O\n"
    "|   P::operator*() may not be noexcept\n"
    "========================================\n");
}

template<bool is_dispatch_ptr_direct_applicable, class D, class O, class P>
constexpr void assert_is_dispatch_ptr_direct_applicable(void)
{
    static_assert(is_dispatch_ptr_direct_applicable,
    "The dispatch D, overload O and proxiable P are inapplicable "
    "to generate dispatcher function for direct convention\n"
    "========================================\n"
    "| * The proxiable type P shall be a value type\n"
    "| * If D is mem dispatch, P must be invocable as p.func(args...)\n"
    "| * If D is free dispatch, P must be invocable as func(p, args...)\n"
    "| * Try to remove `noexcept` from the overload type O\n"
    "========================================\n");
}

/**
 * @brief The overload traits.
 *
 * @tparam O    The overload type. For diagnostic purpose.
 * @tparam Q    The qualifier.
 * @tparam NE   Is noexcept?
 * @tparam R    The return type.
 * @tparam Args The argument types.
 */
template<class O, qualifier_type Q, bool NE, class R, class... Args>
struct overload_traits_impl : applicable_traits
{
    template<diagnose_level level>
    static constexpr bool check_applicable(void) { return applicable; }

    static constexpr qualifier_type qualifier = Q;
    static constexpr bool is_noexcept = NE;
    using return_type = R;

    /**
     * @brief The type of disptatcher function pointer.
     *
     * The dispatcher type is determined by `O`.
     */
    using dispatcher_type =
        func_ptr_t<NE, R, add_qualifier_t<std::byte, Q>, Args...>;

    /**
     * @brief A class template that generates dispatcher function.
     *
     * @tparam IS_DIRECT
     *   The 1st argument of the actual callable is a reference to the proxiable
     *   object, instead of the dereferenced proxiable object.
     * @tparam D
     *   The dispatch type that binds to the the name of actual callable.
     *
     * `meta_provider` is nested in `overload_traits`, since the required pieces of
     * meta information (`Q`, `NE`, `R`, `Args...`) are visible within `overload_traits`.
     *
     * The dispatcher function is generated via `D`, `O` and `P`.
     */
    template<bool IS_DIRECT, class D>
    struct meta_provider
    {
        /**
         * @brief Instantiate a dispatcher function and return a pointer to it.
         *
         * @tparam P The proxiable type.
         *
         * @remark The type of the returned function pointer is the same for any
         *         proxiable type `P`.
         */
        template<class P>
        static constexpr auto get(void) -> dispatcher_type
        {
            if constexpr (!IS_DIRECT &&
                invocable_dispatch_ptr_indirect_concept<D, P, Q, NE, R, Args...>)
            {
                // Instantiate a dispatcher function, and return its pointer.
                return &indirect_conv_dispatcher<D, P, Q, R, Args...>;
            }
            else if constexpr (IS_DIRECT &&
                invocable_dispatch_ptr_direct_concept<D, P, Q, NE, R, Args...>)
            {
                // Instantiate a dispatcher function, and return its pointer.
                return &direct_conv_dispatcher<D, P, Q, R, Args...>;
            }
            // else if constexpr (
            //     invocable_dispatch_concept<D, NE, R, std::nullptr_t, Args...>)
            // {
            //     // Instantiate a dispatcher function, and return its pointer.
            //     return &default_conv_dispatcher<D, Q, R, Args...>;
            // }
            else
            {
                return nullptr;
            }
        }

        template<diagnose_level level, class P>
        static constexpr bool check_applicable_ptr(void)
        {
            if constexpr (!IS_DIRECT)
            {
                constexpr bool is_dispatch_ptr_indirect_applicable =
                    invocable_dispatch_ptr_indirect_concept<D, P, Q, NE, R, Args...>;
                if constexpr (level == diagnose_level::assert)
                {
                    assert_is_dispatch_ptr_indirect_applicable<
                        is_dispatch_ptr_indirect_applicable, D, O, P>();
                }
                return is_dispatch_ptr_indirect_applicable;
            }
            else // if constexpr (IS_DIRECT)
            {
                constexpr bool is_dispatch_ptr_direct_applicable =
                    invocable_dispatch_ptr_direct_concept<D, P, Q, NE, R, Args...>;
                if constexpr (level == diagnose_level::assert)
                {
                    assert_is_dispatch_ptr_direct_applicable<
                        is_dispatch_ptr_direct_applicable, D, O, P>();
                }
                return is_dispatch_ptr_direct_applicable;
            }
        }

        template<class P>
        static constexpr bool applicable_ptr =
            check_applicable_ptr<diagnose_level::none, P>();
    };

    /**
     * @brief Check if a dispatcher function can be instantiated.
     *
     * @remark If `IS_DIRECT == false`, then the proxiable type `P` **must** be
     *         a dereferencable type.
     */
    template<bool IS_DIRECT, class D, class P>
    static constexpr bool applicable_ptr =
            meta_provider<IS_DIRECT, D>::template applicable_ptr<P>;
};

template<class R, class... Args>
struct overload_traits<R(Args...)>
    : overload_traits_impl<R(Args...), qualifier_type::lv, false, R, Args...> {};
template<class R, class... Args>
struct overload_traits<R(Args...) noexcept>
    : overload_traits_impl<R(Args...) noexcept, qualifier_type::lv, true, R, Args...> {};
template<class R, class... Args>
struct overload_traits<R(Args...) &>
    : overload_traits_impl<R(Args...) &, qualifier_type::lv, false, R, Args...> {};
template<class R, class... Args>
struct overload_traits<R(Args...) & noexcept>
    : overload_traits_impl<R(Args...) & noexcept, qualifier_type::lv, true, R, Args...> {};
template<class R, class... Args>
struct overload_traits<R(Args...) &&>
    : overload_traits_impl<R(Args...) &&, qualifier_type::rv, false, R, Args...> {};
template<class R, class... Args>
struct overload_traits<R(Args...) && noexcept>
    : overload_traits_impl<R(Args...) && noexcept, qualifier_type::rv, true, R, Args...> {};
template<class R, class... Args>
struct overload_traits<R(Args...) const>
    : overload_traits_impl<R(Args...) const, qualifier_type::const_lv, false, R, Args...> {};
template<class R, class... Args>
struct overload_traits<R(Args...) const noexcept>
    : overload_traits_impl<R(Args...) const noexcept, qualifier_type::const_lv, true, R, Args...> {};
template<class R, class... Args>
struct overload_traits<R(Args...) const &>
    : overload_traits_impl<R(Args...) const &, qualifier_type::const_lv, false, R, Args...> {};
template<class R, class... Args>
struct overload_traits<R(Args...) const & noexcept>
    : overload_traits_impl<R(Args...) const & noexcept, qualifier_type::const_lv, true, R, Args...> {};
template<class R, class... Args>
struct overload_traits<R(Args...) const &&>
    : overload_traits_impl<R(Args...) const &&, qualifier_type::const_rv, false, R, Args...> {};
template<class R, class... Args>
struct overload_traits<R(Args...) const && noexcept>
    : overload_traits_impl<R(Args...) const && noexcept, qualifier_type::const_rv, true, R, Args...> {};


/**
 * @brief The meta provider deduced from an overload type.
 *
 * @tparam D The dispatch type.
 * @tparam O The overload type.
 *
 * `meta_provider<IS_DIRECT, D, O>` provides the following members:
 * * V: `applicable_ptr<P> : bool`
 * * F: `check_applicable_ptr<diagnose_level, P>() -> bool`
 */
template<bool IS_DIRECT, class D, class O>
using meta_provider = typename overload_traits<O>
                    ::template meta_provider<IS_DIRECT, D>;

/**
 * @brief The table that holds a single dispatcher functor pointer.
 *
 * @tparam MP The `meta_provider` type.
 *
 * Each member function of a proxy is uniquely identified by two pieces of
 * meta information:
 * 1. `D` (name).
 * 2. `O` (qualified signature).
 *
 * @verbatim
 * D + O => meta_provider => dispatcher_meta
 * @endverbatim
 *
 * The `dispatcher` function is further deduced by `P` (proxiable).
 *
 * @verbatim
 * D + O + P => dispatcher_meta + P => dispatcher
 * @endverbatim
 *
 * `dispatcher_meta` is the base class of `composite_meta`.
 * `composite_meta` is a composite of `dispatcher_meta` types.
 */
template<class MP>
struct dispatcher_meta
{
    constexpr dispatcher_meta(void) noexcept = default;

    template<class P>
    constexpr explicit
    dispatcher_meta(std::in_place_type_t<P>) noexcept
        : dispatcher{MP::template get<P>()} {}

    /**
     * @brief The pointer to the dispatcher function.
     */
    decltype(MP::template get<void>()) dispatcher {nullptr};
};


////////////////////////////////////////
template<bool NE>
struct copyability_meta_provider
{
    template<class P>
    static constexpr func_ptr_t<NE, void, std::byte&, const std::byte&> get()
    {
        if constexpr (has_copyability_concept<P, constraint_level::trivial>)
        {
            return &copying_default_dispatcher<sizeof(P), alignof(P)>;
        }
        else
        {
            auto dispatcher = &copying_dispatcher<P>;
            return dispatcher;
        }
    }
};

template<bool NE>
struct relocatability_meta_provider
{
    template<class P>
    static constexpr func_ptr_t<NE, void, std::byte&, const std::byte&> get()
    {
        if constexpr (has_relocatability_concept<P, constraint_level::trivial>)
        {
            return &copying_default_dispatcher<sizeof(P), alignof(P)>;
        }
        else
        {
            auto dispatcher = &relocation_dispatcher<P>;
            return dispatcher;
        }
    }
};

template<bool NE>
struct destructibility_meta_provider
{
    template<class P>
    static constexpr func_ptr_t<NE, void, std::byte&> get()
    {
        if constexpr (has_destructibility_concept<P, constraint_level::trivial>)
        {
            return &destruction_default_dispatcher;
        }
        else
        {
            auto dispatcher = &destruction_dispatcher<P>;
            return dispatcher;
        }
    }
};

/**
 * @brief Decude a lifetime `dispatcher_meta` type from the constraint level.
 *
 * @tparam MP    The lifetime meta provider type.
 * @tparam level The constraint level.
 *
 * * If `level` is `none` or `trivial`, then a dispatcher function is **not** need to
 *   copy, move or destruct the proxiable object.
 *
 * * If `level` is `nontrivial` or `nothrow`, then a dispatcher function is needed to.
 *   copy, move or destruct the proxiable object.
 *
 * If the lifetime `dispatcher_meta` is `void`, then `meta_reduction<>` will **not** put
 * the `dispatcher_meta` into the `composite_meta_impl<>` type.
 */
template<template<bool> class MP, constraint_level level>
struct lifetime_meta_traits
{
    using type = void;
};

template<template<bool> class MP>
struct lifetime_meta_traits<MP, constraint_level::nothrow>
{
    using type = dispatcher_meta<MP<true>>;
};

template<template<bool> class MP>
struct lifetime_meta_traits<MP, constraint_level::nontrivial>
{
    using type = dispatcher_meta<MP<false>>;
};

template<template<bool> class MP, constraint_level level>
using lifetime_meta_t = typename lifetime_meta_traits<MP, level>::type;


////////////////////////////////////////
/**
 * @brief The composite of `dispatcher_meta` types.
 *
 * @tparam Ms The `dispatcher_meta` types.
 *
 * The `composite_meta` is a table of dispatcher function pointers.
 */
template<class... Ms>
struct composite_meta_impl : Ms...
{
    constexpr composite_meta_impl(void) noexcept = default;

    template<class P>
    constexpr explicit
    composite_meta_impl(std::in_place_type_t<P>) noexcept
        : Ms{std::in_place_type<P>}... {}
};

////////////////////////////////////////
template<class CA>
struct is_composite_meta : std::false_type {};

template<class... As>
struct is_composite_meta<composite_meta_impl<As...>> : std::true_type {};

template<class CA>
inline constexpr bool is_composite_meta_v = is_composite_meta<CA>::value;

////////////////////////////////////////
/**
 * @brief Merge `dispatcher_meta` or `composite_meta_impl<...>` into
 *        `composite_meta_impl<...>`.
 *
 * @tparam CM The `composite_meta_impl<...>` type.
 * @tparam M  The `dispatcher_meta` or `composite_meta_impl<...>` type.
 *
 * `M` may be `void`, since `lifetime_meta_t` may generate `void` for none and trivial
 * constraint levels.
 *
 * Each convention deduces a `composite_meta` type.
 * The `composite_meta`s deduced by each convention are further merged into
 * a larger `composite_meta` type.
 */
template<class CM, class M, bool = std::is_void_v<M>>
struct meta_reduction
{
    using type = CM;
};

/**
 * @tparam Ms The `dispatcher_meta` types.
 * @tparam M  The `dispatcher_meta` type.
 */
template<class... Ms, class M>
struct meta_reduction<composite_meta_impl<Ms...>, M, false>
{
    using type = composite_meta_impl<Ms..., M>;
};

template<class... Ms1, class... Ms2>
struct meta_reduction<composite_meta_impl<Ms1...>,
                      composite_meta_impl<Ms2...>, false>
{
    using type = composite_meta_impl<Ms1..., Ms2...>;
};

template<class CM, class M>
using meta_reduction_t = typename meta_reduction<CM, M>::type;

/**
 * @brief Merge `dispatcher_meta` or `composite_meta_impl<...>` into
 *        `composite_meta_impl<...>`.
 *
 * @tparam Ms The `dispatcher_meta` or `composite_meta_impl<...>` type.
 *
 * The resulting type is `composite_meta_impl<M1, M2, ...>`,
 * where `M1`, `M2`, ... are `dispatcher_meta` types.
 */
template<class... Ms>
using composite_meta = recursive_reduction_t<
        meta_reduction_t, composite_meta_impl<>, Ms...>;


////////////////////////////////////////
template<class CM>
struct composite_meta_traits{
    using meta = CM;
    static constexpr bool embed = false;
};

/**
 * If the *first* entry of the composite meta is **not** a `dispatcher_meta<>` type,
 * that is, the *first* entry is a `lifetime_meta<>` type,
 * then this specialization is **not** chosen due to *SFINAE*,
 * and the meta is **not** embedded in a proxy.
 *
 * If `lifetime_meta<>` types are adopted, they are always placed before other
 * `dispatcher_meta<>` types.
 */
template<class MP, class... Ms>
struct composite_meta_traits<
        composite_meta_impl<dispatcher_meta<MP>, Ms...>>
{
    using meta = composite_meta_impl<dispatcher_meta<MP>, Ms...>;

    /// If the `composite_meta_impl<>` is small, then it is embedded in a proxy.
    /// @see `meta_ptr<>`
    static constexpr bool embed =
        ( sizeof(meta) <=  sizeof(ptr_prototype)) &&
        (alignof(meta) <= alignof(ptr_prototype)) &&
        (std::is_nothrow_default_constructible_v<meta>) &&
        (std::is_trivially_copyable_v<meta>);

    /// The `dispatcher_meta<>` type of the *first* entry of the `meta`.
    using M = dispatcher_meta<MP>;
};


////////////////////////////////////////
/**
 * @brief A meta that is accessed indirectly (via a pointer).
 *
 * @tparam CM The `composite_meta_impl<>` type.
 *
 * `sizeof(meta_ptr_direct_impl) == sizeof(void*).
 */
template<class CM>
struct meta_ptr_indirect_impl
{
    constexpr meta_ptr_indirect_impl(void) noexcept : ptr_{nullptr} {};
    template<class P>
    constexpr explicit meta_ptr_indirect_impl(std::in_place_type_t<P>) noexcept
        : ptr_{&storage<P>} {}
    bool has_value(void) const noexcept { return ptr_ != nullptr; }
    void reset(void) noexcept { ptr_ = nullptr; }
    const CM* operator->(void) const noexcept { return ptr_; }

private:
    /// The meta is stored in a separate storage.
    template<class P> static constexpr CM storage{std::in_place_type<P>};
    /// The meta is accessed via `ptr_` indirectly.
    const CM* ptr_;
};

/**
 * @brief A meta that is accessed directly.
 *
 * @tparam CM The `composite_meta_impl<>` type.
 *
 * It is required that `sizeof(meta_ptr_indirect_impl) <= sizeof(ptr_prototype)`.
 *
 * `meta` is stored in `meta_ptr_direct_impl` directly.
 */
template<class CM>
struct meta_ptr_direct_impl : private CM
{
    using CM::CM;
    /// The `dispatcher_meta<>` type of the *first* entry of the `meta`.
    using M = typename composite_meta_traits<CM>::M;
    bool has_value(void) const noexcept
    { return this->M::dispatcher != nullptr; }
    void reset(void) noexcept { this->M::dispatcher = nullptr; }
    const CM* operator->(void) const noexcept { return this; }
};

/**
 * @brief The meta type which may be indirect or direct.
 *
 * @tparam CM The `composite_meta_impl<>` type.
 */
template<class CM>
using meta_ptr = std::conditional_t<(composite_meta_traits<CM>::embed),
                    meta_ptr_direct_impl<CM>,
                    meta_ptr_indirect_impl<CM>>;


////////////////////////////////////////////////////////////////////////////////
/**
 * @brief The composite of accessors.
 *
 * @tparam As The accessor types.
 *
 * Each accessor type provides a named member functions with qualified
 * signatures.
 */
template<class... As>
struct NSFX_ENFORCE_EBO composite_accessor_impl : As...
{
    static_assert((std::is_trivial_v<As> && ...));
    constexpr composite_accessor_impl(void) noexcept = default;
    constexpr composite_accessor_impl(const composite_accessor_impl&) noexcept = default;
    constexpr composite_accessor_impl& operator=(const composite_accessor_impl&) noexcept = default;
};

////////////////////////////////////////
template<class CA>
struct is_composite_accessor : std::false_type {};

template<class... As>
struct is_composite_accessor<composite_accessor_impl<As...>> : std::true_type {};

template<class CA>
inline constexpr bool is_composite_accessor_v = is_composite_accessor<CA>::value;

////////////////////////////////////////
/**
 * @brief Merge composite accessors.
 *
 * @tparam CA The `composite_meta_impl<>` type.
 * @tparam A  The accessor or `composite_meta_impl<>` type.
 */
template<class CA, class A>
struct accessor_reduction;

template<class... As, class A>
struct accessor_reduction<composite_accessor_impl<As...>, A>
{
    using type = composite_accessor_impl<As..., A>;
};

template<class... As1, class... As2>
struct accessor_reduction<composite_accessor_impl<As1...>,
                          composite_accessor_impl<As2...>>
{
    using type = composite_accessor_impl<As1..., As2...>;
};

template<class CA, class A>
using accessor_reduction_t = typename accessor_reduction<CA, A>::type;

template<class... CAs>
using composite_accessor = recursive_reduction_t<
        accessor_reduction_t, composite_accessor_impl<>, CAs...>;


////////////////////////////////////////////////////////////////////////////////
/**
 * @brief The convention type.
 *
 * @tparam D  The dispatch type.
 * @tparam Os The overload types.
 */
template<bool IS_DIRECT, class D, class... Os>
struct convention
{
    static constexpr bool is_direct = IS_DIRECT;
    using dispatch_type = D;
    using overload_types = std::tuple<Os...>;

    template<class F>
    using accessor = typename D::template accessor<F, IS_DIRECT, D, Os...>;
};


////////////////////////////////////////
/**
 * @brief Diagnose the overload types of a convention type.
 *
 * @tparam C  The convention type.
 * @tparam Os The overload types.
 */
template<class C, class... Os>
struct diagnose_conv_overloads
{
    ////////////////////
    // overloads
    ////////////////////
    template<diagnose_level level>
    static constexpr bool check_overloads_applicable(void)
    {
        constexpr std::size_t num_overloads = sizeof...(Os);
        if constexpr (level == diagnose_level::assert)
        {
            static_assert(num_overloads > 0u,
            "The convention C is inapplicable\n"
            "========================================\n"
            "| C::overload_types is empty\n"
            "========================================\n");
        }
        constexpr bool overloads_applicable =
            (overload_traits<Os>::template check_applicable<level>() && ...);
        return (num_overloads > 0u) &&
               overloads_applicable;
    }
};


/**
 * @brief Diagnose a convention type.
 *
 * @tparam C The convention type.
 *
 * The convention type `C` is applicable, if:
 * * `C::is_direct` is a bool constant; AND
 * * `C::dispatch_type` is trival; AND
 * * `C::overload_types` is tuple-like; AND
 * * It has at least one overload type; AND
 * * All overload types are applicable.
 *
 * The following members are provided:
 * * F: `check_applicable<diagnose_level>() -> bool`
 * * V: `applicable: constexpr bool`
 */
template<class C>
struct diagnose_conv
{
    ////////////////////
    // is_direct
    ////////////////////
private:
    template<class C_ = C, class = void>
    struct is_is_direct_well_formed_impl : std::false_type {};

    template<class C_>
    struct is_is_direct_well_formed_impl<C_,
    std::enable_if_t<(
        (std::is_same_v<C_, C>) &&
        /* `C::is_direct` is a bool constant */
        (std::is_same_v<decltype((C_::is_direct)), const bool&>) &&
        /* `C::is_direct` is evaluable at compile time */
        ((void)C_::is_direct, true)),
        void>>
        : std::true_type {};

    template<diagnose_level level>
    static constexpr bool check_is_direct_well_formed(void)
    {
        constexpr bool is_is_direct_well_formed =
            is_is_direct_well_formed_impl<>::value;
        if constexpr (level == diagnose_level::assert)
        {
            static_assert(is_is_direct_well_formed,
            "The convention C is inapplicable\n"
            "========================================\n"
            "| C::is_direct must exist\n"
            "| C::is_direct must be a constexpr bool\n"
            "========================================\n");
        }
        return is_is_direct_well_formed;
    }

    ////////////////////
    // dispatch_type
    ////////////////////
private:
    template<class C_ = C, class = void>
    struct is_dispatch_well_formed_impl : std::false_type {};

    template<class C_>
    struct is_dispatch_well_formed_impl<C_,
    std::enable_if_t<(
        (std::is_same_v<C_, C>) &&
        /* `C::dispatch_type` exists and is trivial */
        (std::is_trivial_v<typename C_::dispatch_type>)),
        void>>
        : std::true_type {};

    template<diagnose_level level>
    static constexpr bool check_dispatch_well_formed(void)
    {
        constexpr bool is_dispatch_well_formed =
            is_dispatch_well_formed_impl<>::value;
        if constexpr (level == diagnose_level::assert)
        {
            static_assert(is_dispatch_well_formed,
            "The convention C is inapplicable\n"
            "========================================\n"
            "| C::dispatch_type must exist\n"
            "| C::dispatch_type must be trivial\n"
            "========================================\n");
        }
        return is_dispatch_well_formed;
    }

    ////////////////////
    // overload_types
    ////////////////////
private:
    template<class C_ = C, class = void>
    struct is_overloads_tuple_like_impl : std::false_type {};

    template<class C_>
    struct is_overloads_tuple_like_impl<C_,
    std::enable_if_t<(
        (std::is_same_v<C_, C>) &&
        /* `C::overload_types` exists and is tuple-like */
        (is_tuple_like_well_formed_concept<typename C_::overload_types>)),
        void>>
        : std::true_type {};

    template<diagnose_level level>
    static constexpr bool check_overloads_well_formed(void)
    {
        constexpr bool is_overloads_tuple_like =
            is_overloads_tuple_like_impl<>::value;
        if constexpr (level == diagnose_level::assert)
        {
            static_assert(is_overloads_tuple_like,
            "The convention C is inapplicable\n"
            "========================================\n"
            "| C:overload_types must exist\n"
            "| C:overload_types must be tuple-like\n"
            "========================================\n");
        }
        if constexpr (is_overloads_tuple_like)
        {
            using diagnose_overloads = instantiated_t<
                diagnose_conv_overloads, typename C::overload_types, C>;
            constexpr bool overloads_applicable =
                diagnose_overloads
                ::template check_overloads_applicable<level>();
            return overloads_applicable;
        }
        return is_overloads_tuple_like;
    }

    ////////////////////
    // applicable
    ////////////////////
public:
    template<diagnose_level level>
    static constexpr bool check_applicable(void)
    {
        constexpr bool applicable =
                check_is_direct_well_formed<level>() &&
                check_dispatch_well_formed<level>() &&
                check_overloads_well_formed<level>();
        return applicable;
    }

    static constexpr bool applicable =
        check_applicable<diagnose_level::none>();
};


////////////////////////////////////////
/**
 * @brief The convention traits.
 *
 * @tparam C  The convention type.
 * @tparam Os The overload types.
 *
 * `conv_traits<C>` inherits members from `diagnose_conv<C>` for convenience.
 * * V: `applicable : bool`
 * * F: `check_applicable<diagnose_level>() -> bool`
 */
template<class C, class... Os>
struct conv_traits_impl_ex : diagnose_conv<C>
{
    ////////////////////
private:
    static constexpr bool IS_DIRECT = C::is_direct;
    using D = typename C::dispatch_type;

    ////////////////////
    // meta
    ////////////////////
public:
    /**
     * @brief The meta (table of dispatcher pointers) of the convention.
     *
     * * Each overload `Os` provides a `meta_provider`.
     * * `dispatcher_meta<>` uses the `meta_provider` to instantiate
     *   the dispatcher function, and holds a pointer to the function.
     * * `composite_meta_impl<>` composes the dispatcher metas into a table
     *   of dispatcher function pointers.
     */
    using meta = composite_meta_impl<
        dispatcher_meta<meta_provider<IS_DIRECT, D, Os>>...>;

    ////////////////////
    // applicable_ptr
    ////////////////////
    template<diagnose_level level, class P>
    static constexpr bool check_applicable_ptr(void)
    {
        // overload_traits::meta_provider::check_applicable_ptr()
        constexpr bool overloads_applicable_ptr =
            (overload_traits<Os>::template meta_provider<IS_DIRECT, D>
             ::template check_applicable_ptr<level, P>() && ...);
        return overloads_applicable_ptr;
    }

    /**
     * @brief Check if dispatcher functions can be instantiated.
     *
     * @remark If `IS_DIRECT == false`, then the proxiable type `P` **must** be
     *         a dereferencable type.
     */
    template<class P>
    static constexpr bool applicable_ptr =
        check_applicable_ptr<diagnose_level::none, P>();

    ////////////////////
    // accessor
    ////////////////////
    /**
     * @brief An accesor type of a convention.
     *
     * @tparam F The facade type.
     *
     * Each convention `C` has a dispatch type `D`.
     *
     * Each `D` provides an `accessor` class template that provides
     * a named member function for the proxy.
     *
     * An accessor class is deduced from `F`, `IS_DIRECT` and overload type `O`.
     */
    template<class F>
    using accessor = typename C::dispatch_type
                   ::template accessor<F, C::is_direct, D, Os...>;
};

/**
 * @brief The convention traits.
 *
 * @tparam C The convention type.
 *
 * `conv_traits<C>` inherits members from `diagnose_conv<C>` for convenience.
 * * V: `applicable : bool`
 * * F: `check_applicable<diagnose_level>() -> bool`
 */
template<class C, class = void>
struct conv_traits_impl : diagnose_conv<C> {};

template<class C>
struct conv_traits_impl<C,
    std::enable_if_t<diagnose_conv<C>::applicable, void>>
    : instantiated_t<conv_traits_impl_ex, typename C::overload_types, C>
{};

/**
 * @brief The convention traits.
 *
 * @tparam C The convention type.
 *
 * `conv_traits<C>` provides the following members:
 * * V: `applicable : bool`
 * * F: `check_applicable<diagnose_level>() -> bool`
 * * T: `meta = composite_meta_impl<>` type
 * * F: `check_applicable_ptr<diagnose_level, P>() -> bool`
 * * T: `accesor<F>`
 */
template<class C>
using conv_traits = conv_traits_impl<C>;


////////////////////////////////////////////////////////////////////////////////
template<class F, bool IS_DIRECT>
struct composite_accessor_helper
{
    /**
     * @brief Merge `CA` with the accessors of `C` into a new composite accessor.
     *
     * @tparam CA The `composite_accessor_impl<>` type.
     * @tparam C  The convention type, which provides a composite accessor.
     *
     * If `C::is_direct != IS_DIRECT`, then the accessors of `C` are **not** merged
     * with `CA`, and the resulting type is `CA`.
     *
     * Otherwise, if `C::is_direct == IS_DIRECT`, then the accessors of `C` are
     * merged with `CA`.
     */
    template<class CA, class C, class = void>
    struct reduction_impl
    {
        using type = CA;
    };

    template<class CA, class C>
    struct reduction_impl<CA, C,
        std::enable_if_t<(C::is_direct == IS_DIRECT), void>>
    {
        using type = composite_accessor<CA,
                typename conv_traits<C>::template accessor<F>>;
    };

    template<class CA, class C>
    using reduction_t = typename reduction_impl<CA, C>::type;

    /**
     * @brief Merge accessors of the convention types `Cs`.
     *
     * @tparam Cs The convention types.
     *
     * For each `C` in `Cs`, if `C::is_direct != IS_DIRECT`, then the accessors
     * of `C` are **not** merged.
     */
    template<class... Cs>
    using accessor = recursive_reduction_t<
            reduction_t, composite_accessor_impl<>, Cs...>;
};

/**
 * @brief Merge the accessors of convention types of `F` into
 *        a composite accessor with given `IS_DIRECT`.
 *
 * @tparam F The facade type.
 *
 * The convention type `C` is considered if `C::is_direct == IS_DIRECT`.
 * The accessors of the considered convention types are merged into
 * a composite accessor.
 */
template<class F, bool IS_DIRECT>
using facade_accessor = instantiated_t<
        composite_accessor_helper<F, IS_DIRECT>::template accessor,
        typename F::convention_types>;


////////////////////////////////////////////////////////////////////////////////
/**
 * @brief Merge `meta_ptr` with composite accessor.
 *
 * @tparam CA The `composite_accessor_impl<>` type.
 * @tparam CM The `composite_meta_impl<>` type.
 */
template<class CA, class CM>
struct NSFX_ENFORCE_EBO composite_accessor_meta : CA
{
    static_assert(is_composite_accessor_v<CA>);
    static_assert(is_composite_meta_v<CM>);

    using MetaPtr = meta_ptr<CM>;

    MetaPtr  meta_ptr_;

    constexpr composite_accessor_meta(void) noexcept = default;
    constexpr composite_accessor_meta(const composite_accessor_meta&) noexcept = default;
    constexpr composite_accessor_meta& operator=(const composite_accessor_meta&) noexcept = default;

    template<class P>
    constexpr explicit
    composite_accessor_meta(std::in_place_type_t<P>) noexcept
        : meta_ptr_{std::in_place_type<P>} {}
};

template<class CAM>
struct composite_accessor_meta_traits;

template<class CA, class CM>
struct composite_accessor_meta_traits<composite_accessor_meta<CA, CM>>
{
    using accessor = CA;
    using meta = CM;
};


////////////////////////////////////////
/**
 * @brief Reset meta upon destruction.
 *
 * @tparam MetaPtr The `composite_accessor_meta` type.
 */
template<class MetaPtr>
struct meta_ptr_reset_guard
{
public:
    explicit meta_ptr_reset_guard(MetaPtr& meta_ptr) noexcept : meta_ptr_{meta_ptr} {}
    meta_ptr_reset_guard(const meta_ptr_reset_guard&) = delete;
    ~meta_ptr_reset_guard(void) noexcept { meta_ptr_.reset(); }

private:
    MetaPtr& meta_ptr_;
};


////////////////////////////////////////////////////////////////////////////////
template<class F, class... Cs>
struct diagnose_facade_convs
{
    ////////////////////
    // applicable
    ////////////////////
    template<diagnose_level level>
    static constexpr bool check_convs_applicable(void)
    {
        constexpr bool convs_applicable =
            (diagnose_conv<Cs>::template check_applicable<level>() && ...);
        return convs_applicable;
    }

    ////////////////////
    // applicable_ptr
    ////////////////////
    template<diagnose_level level, class P>
    static constexpr bool check_convs_applicable_ptr(void)
    {
        constexpr bool convs_applicable_ptr =
            (conv_traits<Cs>::template check_applicable_ptr<level, P>() && ...);
        return convs_applicable_ptr;
    }
};

template<class F>
struct diagnose_facade
{
    ////////////////////
    // constraints
    ////////////////////
private:
    template<class F_ = F, class = void>
    struct is_constraints_existent_impl : std::false_type {};

    template<class F_>
    struct is_constraints_existent_impl<F_,
    std::enable_if_t<(
        (std::is_same_v<F_, F>) &&
        /* `F::constraints` is of type `proxiable_ptr_constraints` */
        (std::is_same_v<decltype((F_::constraints)), const proxiable_ptr_constraints&>) &&
        /* `F::constraints` is evaluable at compile time */
        ((void)F_::constraints, true)),
        void>>
        : std::true_type {};

    template<diagnose_level level>
    static constexpr bool check_constraints_well_formed(void)
    {
        constexpr bool is_constraints_existent =
                is_constraints_existent_impl<F>::value;
        if constexpr (level == diagnose_level::assert)
        {
            static_assert(is_constraints_existent,
            "The facade F is inapplicable\n"
            "========================================\n"
            "| * F::constraints must exist\n"
            "| * F::constraints must be a constexpr proxiable_ptr_constraints\n"
            "========================================\n");
        }
        if constexpr (is_constraints_existent)
        {
            /* `F::constraints.max_align` is a power of two */
            constexpr bool is_max_align_pow_of_two =
                is_pow2_v<F::constraints.max_align>;
            if constexpr (level == diagnose_level::assert)
            {
                static_assert(is_constraints_existent,
                "The facade F is inapplicable\n"
                "========================================\n"
                "| * F::constraints::max_align must be a power of two\n"
                "========================================\n");
            }
            /* `F::constraints.max_size` is a multiple of `F::constraints.max_align */
            constexpr bool is_max_size_multiple_of_max_align =
                (F::constraints.max_size % F::constraints.max_align == 0u);
            if constexpr (level == diagnose_level::assert)
            {
                static_assert(is_max_size_multiple_of_max_align,
                "The facade F is inapplicable\n"
                "========================================\n"
                "| * F::constraints::max_size must be a multiple of F::constraints::max_size\n"
                "========================================\n");
            }
            return is_max_align_pow_of_two &&
                   is_max_size_multiple_of_max_align;
        }
        return is_constraints_existent;
    }

    static constexpr bool is_constraints_well_formed =
        check_constraints_well_formed<diagnose_level::none>();

    ////////////////////
    // conventions
    ////////////////////
private:
    template<class F_ = F, class = void>
    struct is_convs_tuple_like_impl : std::false_type {};

    template<class F_>
    struct is_convs_tuple_like_impl<F_,
    std::enable_if_t<(
        (std::is_same_v<F_, F>) &&
        /* `F::convention_types` exists and is tuple-like */
        (is_tuple_like_well_formed_concept<typename F_::convention_types>)),
        void>>
        : std::true_type {};

    template<diagnose_level level>
    static constexpr bool check_convs_well_formed(void)
    {
        constexpr bool is_convs_tuple_like =
            is_convs_tuple_like_impl<>::value;
        if constexpr (level == diagnose_level::assert)
        {
            static_assert(is_convs_tuple_like,
            "The facade F is inapplicable\n"
            "========================================\n"
            "| * F::convention_types must exist\n"
            "| * F::convention_types must be tuple-like\n"
            "========================================\n");
        }
        if constexpr (is_convs_tuple_like)
        {
            using diagnose_convs = instantiated_t<
                diagnose_facade_convs, typename F::convention_types, F>;
            constexpr bool convs_applicable =
                diagnose_convs::template check_convs_applicable<level>();
            return convs_applicable;
        }
        return is_convs_tuple_like;
    }

    ////////////////////
    // applicable
    ////////////////////
public:
    template<diagnose_level level>
    static constexpr bool check_applicable(void)
    {
        return check_constraints_well_formed<level>() &&
               check_convs_well_formed<level>();
    }

    static constexpr bool applicable =
        check_applicable<diagnose_level::none>();

    ////////////////////
    // conv_applicable_ptr
    ////////////////////
public:
    template<diagnose_level level, class P>
    static constexpr bool check_conv_applicable_ptr(void)
    {
        using diagnose_convs = instantiated_t<
            diagnose_facade_convs, typename F::convention_types, F>;
        constexpr bool convs_applicable_ptr =
            diagnose_convs::template check_convs_applicable_ptr<level, P>();
        return convs_applicable_ptr;
    }

    template<class P>
    static constexpr bool conv_applicable_ptr =
        check_conv_applicable_ptr<diagnose_level::none, P>();
};

/**
 * @brief The facade traits.
 *
 * @tparam F  The facade type.
 * @tparam Cs The convention types.
 *
 * `facade_traits<F>` inherits members from `diagnose_facade<F>` for convenience.
 * * V: `applicable : bool`
 * * F: `check_applicable<diagnose_level>() -> bool`
 * * V: `conv_applicable_ptr<P> : bool`
 * * F: `check_conv_applicable_ptr<diagnose_level, P>() -> bool`
 */
template<class F, class... Cs>
struct facade_traits_impl_ex : diagnose_facade<F>
{
    using conv_meta = composite_meta<typename conv_traits<Cs>::meta...>;
    using indirect_accessor = facade_accessor<F, false>;
    using direct_accessor   = facade_accessor<F, true>;
};

/**
 * @brief The facade traits.
 *
 * @tparam F  The facade type.
 * @tparam Cs The convention types.
 *
 * `facade_traits<F>` inherits members from `diagnose_facade<F>` for convenience.
 * * V: `applicable : bool`
 * * F: `check_applicable<diagnose_level>() -> bool`
 * * V: `conv_applicable_ptr<P> : bool`
 * * F: `check_conv_applicable_ptr<diagnose_level, P>() -> bool`
 */
template<class F, class = void>
struct facade_traits_impl : diagnose_facade<F> {};

template<class F>
struct facade_traits_impl<F,
    std::enable_if_t<diagnose_facade<F>::applicable, void>>
    : instantiated_t<facade_traits_impl_ex, typename F::convention_types, F>
{
    using copyability_meta = lifetime_meta_t<
            copyability_meta_provider, F::constraints.copyability>;
    using relocatability_meta = lifetime_meta_t<
            relocatability_meta_provider,
            F::constraints.copyability == constraint_level::trivial ?
                constraint_level::trivial : F::constraints.relocatability>;
    using destructibility_meta = lifetime_meta_t<
            destructibility_meta_provider, F::constraints.destructibility>;

    /**
     * @brief Meta (table of dispatcher function pointers).
     *
     * @remark `lifetime_meta<>` entries, if exist, are always placed *before*
     *         `dispatcher_meta<>` entries.
     */
    using meta = composite_meta<copyability_meta,
                                relocatability_meta,
                                destructibility_meta,
                                typename facade_traits_impl::conv_meta>;

    static constexpr bool has_indirection = !std::is_same_v<
        typename facade_traits_impl::indirect_accessor,
        composite_accessor_impl<>>;
};

/**
 * @brief The facade traits.
 *
 * @tparam F  The facade type.
 * @tparam Cs The convention types.
 *
 * `facade_traits<F>` provides the following members:
 * * V: `applicable : bool`
 * * F: `check_applicable<diagnose_level>() -> bool`
 * * V: `conv_applicable_ptr<P> : bool`
 * * F: `check_conv_applicable_ptr<diagnose_level, P>() -> bool`
 * * T: `copyability_meta`
 * * T: `relocatability_meta`
 * * T: `destructibility_meta`
 * * T: `meta = composite_meta_impl<>`
 * * V: `has_indirection : bool`
 */
template<class F>
using facade_traits = facade_traits_impl<F>;


////////////////////////////////////////////////////////////////////////////////
/**
 * @brief Diagnose a proxiable type.
 *
 * @tparam P The proxiable type.
 * @tparam F The facade type.
 *
 * `diagnose_proxiable<P, F>` provides the following members:
 * * V: `is_facade_applicable : bool`
 * * F: `check_facade_applicable<diagnose_level>() -> bool`
 * * V: `is_proxiable_well_formed : bool`
 * * F: `check_proxiable_well_formed<diagnose_level>() -> bool`
 */
template<class P, class F>
struct diagnose_proxiable
{
    ////////////////////
    // facade
    ////////////////////
private:
    template<diagnose_level level>
    static constexpr bool check_facade_applicable(void)
    {
        constexpr bool is_facade_applicable =
            facade_traits<F>::template check_applicable<level>();
        return is_facade_applicable;
    }

    static constexpr bool is_facade_applicable =
        check_facade_applicable<diagnose_level::none>();

    ////////////////////
    // proxiable
    ////////////////////
private:
    template<diagnose_level level>
    static constexpr bool check_proxiable_applicable(void)
    {
        constexpr bool is_proxiable_size_well_formed =
            (sizeof(P) <= F::constraints.max_size);
        if constexpr (level == diagnose_level::assert)
        {
            static_assert(is_proxiable_size_well_formed,
            "The proxiable P is inapplicable for the facade F\n"
            "========================================\n"
            "| sizeof(P) must be no larger than F::constraints.max_size\n"
            "========================================\n");
        }
        constexpr bool is_proxiable_align_well_formed =
            (alignof(P) <= F::constraints.max_align);
        if constexpr (level == diagnose_level::assert)
        {
            static_assert(is_proxiable_align_well_formed,
            "The proxiable P is inapplicable for the facade F\n"
            "========================================\n"
            "| alignof(P) must be no larger than F::constraints.max_align\n"
            "========================================\n");
        }
        constexpr bool is_copyability_consistent =
            has_copyability_concept<P, F::constraints.copyability>;
        if constexpr (level == diagnose_level::assert)
        {
            static_assert(is_copyability_consistent,
            "The proxiable P is inapplicable for the facade F\n"
            "========================================\n"
            "| P is inconsistent with F::constraints.copyability\n"
            "| * Try to set F::constraints.copyability to a larger value\n"
            "========================================\n");
        }
        constexpr bool is_relocatability_consistent =
            has_relocatability_concept<P, F::constraints.relocatability>;
        if constexpr (level == diagnose_level::assert)
        {
            static_assert(is_relocatability_consistent,
            "The proxiable P is inapplicable for the facade F\n"
            "========================================\n"
            "| P is inconsistent with F::constraints.relocatability\n"
            "| * Try to set F::constraints.relocatability to a larger value\n"
            "========================================\n");
        }
        constexpr bool is_destructibility_consistent =
            has_destructibility_concept<P, F::constraints.destructibility>;
        if constexpr (level == diagnose_level::assert)
        {
            static_assert(is_destructibility_consistent,
            "The proxiable P is inapplicable for the facade F\n"
            "========================================\n"
            "| P is inconsistent with F::constraints.destructibility\n"
            "| * Try to set F::constraints.destructibility to a larger value\n"
            "========================================\n");
        }
        constexpr bool conv_applicable_ptr =
            diagnose_facade<F>::template check_conv_applicable_ptr<level, P>();
        return is_proxiable_size_well_formed  &&
               is_proxiable_align_well_formed &&
               is_copyability_consistent      &&
               is_relocatability_consistent   &&
               is_destructibility_consistent  &&
               conv_applicable_ptr;
    }

    ////////////////////
    // applicable
    ////////////////////
public:
    template<diagnose_level level>
    static constexpr bool check_applicable(void)
    {
        return check_facade_applicable<level>() &&
               check_proxiable_applicable<level>();
    }

    static constexpr bool applicable =
        check_facade_applicable<diagnose_level::none>();
};

template<class P, class F>
inline constexpr bool is_proxiable_applicable =
        diagnose_proxiable<P, F>
        ::template check_applicable<diagnose_level::none>();


////////////////////////////////////////////////////////////////////////////////
/**
 * @brief The helper template to invoke a proxy.
 *
 * @tparam F The facade type.
 */
template<class F>
struct proxy_helper
{
    /**
     * @brief Get the table of dispatcher function pointers.
     *
     * @param[in] p The proxy.
     *
     * @return A reference to the `composite_meta_impl<>` object.
     */
    static inline const auto& get_meta(const proxy<F>& p) noexcept
    {
        return *p.meta_ptr().operator->();
    }

    /**
     * @brief Invokes the actual callable.
     *
     * @tparam D The dispatch type.
     * @tparam O The overload type.
     * @tparam Q The qualifier.
     * @tparam Args The argument types.
     *
     * @param[in] p The qualified reference to the proxy.
     *
     * The proxy type `proxy<F>` is deduced from the facade type `F`.
     */
    template<bool IS_DIRECT, class D, class O, qualifier_type Q, class... Args>
    static auto invoke(add_qualifier_t<proxy<F>, Q> p, Args&&... args)
        noexcept(overload_traits<O>::is_noexcept)
        -> typename overload_traits<O>::return_type
    {
        // The meta provider that provides the dispatcher function.
        using MP = meta_provider<IS_DIRECT, D, O>;
        // The meta provider that provides the dispatcher function.
        using M [[maybe_unused]] = dispatcher_meta<MP>;
        // The dispatcher meta holds the pointer to the dispatcher function,
        // which is the base class subobject of the meta.
        auto dispatcher = p.meta_ptr()->M::dispatcher;
        if constexpr (IS_DIRECT && Q == qualifier_type::rv)
        {
            meta_ptr_reset_guard guard{p.meta_ptr()};
            return dispatcher(
                    std::forward<add_qualifier_t<std::byte, Q>>(*p.ptr_),
                    std::forward<Args>(args)...);
        }
        else
        {
            return dispatcher(
                    std::forward<add_qualifier_t<std::byte, Q>>(*p.ptr_),
                    std::forward<Args>(args)...);
        }
    }

    /**
     * @brief Obtain the proxy from the accessor.
     *
     * @tparam A The accessor type, which is the base class of the proxy.
     * @tparam Q The qualifier.
     *
     * @param[in] a The accessor, which is the base class subobject of the proxy.
     *
     * The proxy type `proxy<F>` is deduced from the facade type `F`.
     * The accessor type `A` is deduced from one of the conventions of `F`, which
     * has a named member function, and is the base class subobject of the proxy.
     *
     * `Q` is determined by the qualifier type of the `proxy<F>`.
     *
     * For example,
     * ~~~
     * proxy<F> p;
     * p->foo();
     * // p.operator->() is `indirect_accessor&`,
     * // thus, Q is `qualifier_type::lv`.
     * ~~~
     *
     * For example,
     * ~~~
     * const proxy<F> p;
     * p->foo();
     * // p.operator->() is `const indirect_accessor&`,
     * // thus Q is `qualifier_type::const_lv`.
     * ~~~
     */
    template<class A, qualifier_type Q>
    static add_qualifier_t<proxy<F>, Q> access(add_qualifier_t<A, Q> a) noexcept
    {
        using PX = proxy<F>;
        using AQ = add_qualifier_t<A, Q>;
        using PQ = add_qualifier_t<PX, Q>;
        // If `a` is a direct accessor.
        if constexpr (std::is_base_of_v<A, PX>)
        {
            return static_cast<PQ>(std::forward<AQ>(a));
        }
        // Otherwise, `a` is an indirect accessor.
        else
        {
            static_assert(offsetof(proxy<F>, ia_meta_) == 0u);
            using BQP = add_qualifier_ptr_t<std::byte, Q>;
            return reinterpret_cast<PQ>(
                    *(reinterpret_cast<BQP>(std::addressof(a))));
        }
    }
};


////////////////////////////////////////////////////////////////////////////////
template<bool IS_DIRECT, class D, class O, class F, class... Args,
    std::enable_if_t<
    diagnose_facade<F>::applicable, void>* = nullptr>
auto proxy_invoke(proxy<F>& p, Args&&... args)
    noexcept(overload_traits<O>::is_noexcept)
    -> typename overload_traits<O>::return_type
{
    return proxy_helper<F>::template invoke<
        IS_DIRECT, D, O, qualifier_type::lv>(
            p, std::forward<Args>(args)...);
}

template<bool IS_DIRECT, class D, class O, class F, class... Args,
    std::enable_if_t<
    diagnose_facade<F>::applicable, void>* = nullptr>
auto proxy_invoke(const proxy<F>& p, Args&&... args)
    noexcept(overload_traits<O>::is_noexcept)
    -> typename overload_traits<O>::return_type
{
    return proxy_helper<F>::template invoke<
        IS_DIRECT, D, O, qualifier_type::const_lv>(
            p, std::forward<Args>(args)...);
}

template<bool IS_DIRECT, class D, class O, class F, class... Args,
    std::enable_if_t<
    diagnose_facade<F>::applicable, void>* = nullptr>
auto proxy_invoke(proxy<F>&& p, Args&&... args)
    noexcept(overload_traits<O>::is_noexcept)
    -> typename overload_traits<O>::return_type
{
    return proxy_helper<F>::template invoke<
        IS_DIRECT, D, O, qualifier_type::rv>(
            std::forward<proxy<F>>(p), std::forward<Args>(args)...);
}

template<bool IS_DIRECT, class D, class O, class F, class... Args,
    std::enable_if_t<
    diagnose_facade<F>::applicable, void>* = nullptr>
auto proxy_invoke(const proxy<F>&& p, Args&&... args)
    noexcept(overload_traits<O>::is_noexcept)
    -> typename overload_traits<O>::return_type
{
    return proxy_helper<F>::template invoke<
        IS_DIRECT, D, O, qualifier_type::const_rv>(
            std::forward<const proxy<F>>(p), std::forward<Args>(args)...);
}


////////////////////////////////////////
/**
 * @brief Obtain a qualified reference to `proxy<F>` from an `accessor`.
 *
 * @tparam F The facade type.
 * @tparam A The accessor type, which is the base class of the proxy.
 * @param[in] a The accessor, which is the base class subobject of the proxy.
 */
template<class F, class A>
proxy<F>& access_proxy(A& a) noexcept
{
    return proxy_helper<F>::template access<
        A, qualifier_type::lv>(a);
}

template<class F, class A>
const proxy<F>& access_proxy(const A& a) noexcept
{
    return proxy_helper<F>::template access<
        A, qualifier_type::const_lv>(a);
}

template<class F, class A>
proxy<F>&& access_proxy(A&& a) noexcept
{
    return proxy_helper<F>::template access<
        A, qualifier_type::rv>(std::forward<A>(a));
}

template<class F, class A>
const proxy<F>&& access_proxy(const A&& a) noexcept
{
    return proxy_helper<F>::template access<
        A, qualifier_type::const_rv>(std::forward<const A>(a));
}


////////////////////////////////////////////////////////////////////////////////
/**
 * @brief Merge a tuple of conventions and a convention type.
 *
 * @tparam CT The tuple of conventions.
 * @tparam D  The dispatch type.
 * @tparam Os The overload types.
 */
template<class CT, class C>
struct add_conv_reduction
{
    using type = CT;
};

template<class... Cs, class C>
struct add_conv_reduction<std::tuple<Cs...>, C>
{
    using type = std::tuple<Cs..., C>;
};

template<class CT, bool IS_DIRECT, class D, class... Os>
using add_conv_t = typename add_conv_reduction<CT,
                   convention<IS_DIRECT, D, Os...>>::type;


////////////////////////////////////////////////////////////////////////////////
/**
 * @brief The facade type.
 *
 * @tparam CT     The tuple of conventions.
 * @tparam Constr The `proxiable_ptr_constraints_impl<>` type.
 *
 * `facade<>` provides the following members for diagnostic:
 * * V: `proxiable_applicable<P> : bool`
 * * F: `assert_proxiable_applicable<P>() -> bool`
 */
template<class CT, class Constr>
struct facade
{
    using convention_types = CT;

    static constexpr proxiable_ptr_constraints
    constraints { Constr::max_size,
                  Constr::max_align,
                  Constr::copyability,
                  Constr::relocatability,
                  Constr::destructibility };

    template<class P>
    static constexpr bool assert_proxiable_applicable(void)
    {
        return diagnose_proxiable<P, facade>
               ::template check_applicable<diagnose_level::assert>();
    }

    template<class P>
    static constexpr bool proxiable_applicable =
        diagnose_proxiable<P, facade>
        ::template check_applicable<diagnose_level::none>();

};


}  // namespace details


////////////////////////////////////////////////////////////////////////////////
/**
 * @brief The proxy.
 *
 * @tparam F The facade type.
 *
 * `proxy<F>` provides the following members for diagnostic:
 * * V: `proxiable_applicable<P> : bool`
 * * F: `assert_proxiable_applicable<P>() -> bool`
 */
template<class F>
class NSFX_ENFORCE_EBO proxy
    : public details::facade_traits<F>::direct_accessor
{
private:
    // `meta_ptr()`.
    friend struct details::proxy_helper<F>;

    using facade_traits = details::facade_traits<F>;

    using meta = typename facade_traits::meta;
    using direct_accessor = typename facade_traits::direct_accessor;
    using indirect_accessor = typename facade_traits::indirect_accessor;

    using composite_accessor_meta =
            details::composite_accessor_meta<indirect_accessor, meta>;

    using MetaPtr = details::meta_ptr<meta>;

    static constexpr bool can_copy =
        (F::constraints.copyability != constraint_level::none);

    static constexpr bool can_move =
        (F::constraints.relocatability != constraint_level::none);

    static constexpr bool can_dtor =
        (F::constraints.destructibility != constraint_level::none);

public:
    template<class P>
    static constexpr bool assert_proxiable_applicable(void)
    {
        return details::diagnose_proxiable<P, F>
               ::template check_applicable<diagnose_level::assert>();
    }

    template<class P>
    static constexpr bool proxiable_applicable =
        details::diagnose_proxiable<P, F>
        ::template check_applicable<diagnose_level::none>();

public:
    proxy(void) noexcept = default;

    ////////////////////
    // Copy constructor
    ////////////////////
public:
    proxy(const proxy& rhs)
        noexcept(F::constraints.copyability >= constraint_level::nothrow)
        : proxy{rhs, details::copy_t{}}
    {}

private:
    /**
     * @brief Copy contruct.
     *
     * @remark The proxy is **not** copyable, if `copyability == none`.
     */
    template<class PX,
    std::enable_if_t<!PX::can_copy, void>* = nullptr>
    proxy(const PX&, details::copy_t) = delete;

    /**
     * @brief Copy contruct.
     *
     * @remark The proxy is copyable, only if `copyability != none`.
     */
    template<class PX,
    std::enable_if_t<PX::can_copy, void>* = nullptr>
    proxy(const PX& rhs, details::copy_t)
        noexcept(F::constraints.copyability >= constraint_level::nothrow)
        // NOTE: Since `copy_assign()` may call this constructor, `ia_meta_`
        //       must remain null if exception throws.
        : ia_meta_{}
    {
        static_assert(F::constraints.copyability != constraint_level::none);
        if (rhs.meta_ptr().has_value())
        {
            // If copyability is trivial.
            if constexpr (
                F::constraints.copyability == constraint_level::trivial)
            {
                std::uninitialized_copy_n(rhs.ptr_, sizeof(*ptr_), ptr_);
            }
            // Otherwise, copyability is nontrivial or nothrow.
            else
            {
                // May throw.
                rhs.meta_ptr()->facade_traits::copyability_meta
                                ::dispatcher(*ptr_, *rhs.ptr_);
            }
            // If copyability disptatcher throws, `ia_meta_` remains null.
            ia_meta_ = rhs.ia_meta_;
        }
    }

    ////////////////////
    // Move constructor
    ////////////////////
public:
    proxy(proxy&& rhs)
        noexcept(F::constraints.relocatability >= constraint_level::nothrow)
        : proxy{std::move(rhs), details::move_t{}}
    {}

private:
    template<class PX,
    std::enable_if_t<!PX::can_move && !PX::can_copy, void>* = nullptr>
    proxy(PX&&, details::move_t) = delete;

    template<class PX,
    std::enable_if_t<!PX::can_move && PX::can_copy, void>* = nullptr>
    proxy(PX&& rhs, details::move_t)
        noexcept(std::is_nothrow_constructible_v<proxy, const proxy, details::copy_t>)
        : proxy{rhs, details::copy_t{}}
    {}

    template<class PX,
    std::enable_if_t<PX::can_move, void>* = nullptr>
    proxy(PX&& rhs, details::move_t)
        noexcept(F::constraints.relocatability >= constraint_level::nothrow)
        // NOTE: Since `move_assign()` may call this constructor, `ia_meta_`
        //       must remain null if exception throws.
        : ia_meta_{}
    {
        if (rhs.meta_ptr().has_value())
        {
            // If relocatability is trivial.
            if constexpr (
                F::constraints.relocatability == constraint_level::trivial)
            {
                std::uninitialized_copy_n(rhs.ptr_, sizeof(*ptr_), ptr_);
                ia_meta_ = rhs.ia_meta_;
            }
            // Otherwise, relocatability is nontrivial or nothrow.
            else
            {
                details::meta_ptr_reset_guard guard{rhs.meta_ptr()};
                // May throw.
                rhs.meta_ptr()->facade_traits::relocatability_meta
                                ::dispatcher(*ptr_, *rhs.ptr_);
                // If relocatability dispatcher throws, `ia_meta_` remains null.
                ia_meta_ = rhs.ia_meta_;
            }
        }
    }

    ////////////////////
    // Emplace constructor
    ////////////////////
public:
    proxy(std::nullptr_t) noexcept : proxy{} {}

#if defined(_MSC_VER) // Also applies to clang++ for Windows.
    /**
     * @brief Construct a proxy by a proxiable object.
     *
     * @tparam P The proxiable type.
     * @param[in] p The proxiable object.
     */
    template<class P,
    std::enable_if_t<(
        (!std::is_same_v<std::decay_t<P>, proxy>) &&
        (details::is_proxiable_applicable<std::decay_t<P>, F>) &&
        (std::is_constructible_v<std::decay_t<P>, P>)),
        void>* = nullptr>
    proxy(P&& p)
        noexcept(std::is_nothrow_constructible_v<std::decay_t<P>, P>)
    {
        initialize<std::decay_t<P>>(std::forward<P>(p));
    }
#else // !defined(_MSC_VER))
    /**
     * @brief Construct a proxy by a proxiable object.
     *
     * @tparam P The proxiable type.
     * @param[in] p The proxiable object.
     *
     * @remark
     *   This overload shall be written as:
     *   ~~~
     *   template<class P,
     *   std::enable_if_t<(
     *       (!std::is_same_v<std::decay_t<P>, proxy>) &&
     *       (details::is_proxiable_applicable<std::decay_t<P>, F>) &&
     *       (std::is_constructible_v<std::decay_t<P>, P>)),
     *       void>* = nullptr>
     *   proxy(P&& p)
     *       noexcept(std::is_nothrow_constructible_v<std::decay_t<P>, P>)
     *   {
     *       initialize<std::decay_t<P>>(std::forward<P>(p));
     *   }
     *   ~~~
     *   However, in Linux, g++ and clang++ c++17 have an issue that even if
     *   `(!std::is_same_v<std::decay_t<P>, proxy>)` is evaluated to `false`,
     *   `(details::is_proxiable_applicable<std::decay_t<P>, F>)` and
     *   `(std::is_constructible_v<std::decay_t<P>, P>)` are evaluated.
     *   When `P` is `proxy`, the issue causes compilation errors.
     *   Thus, the conditions are divided into two parts, where the tests of
     *   `(details::is_proxiable_applicable<std::decay_t<P>, F>)` and
     *   `(!std::is_same_v<std::decay_t<P>, proxy>)` are postponed.
     */
    template<class P,
    std::enable_if_t<(
        (!std::is_same_v<std::decay_t<P>, proxy>)),
        void>* = nullptr>
    proxy(P&& p,
        std::enable_if_t<(
            (details::is_proxiable_applicable<std::decay_t<P>, F>) &&
            (std::is_constructible_v<std::decay_t<P>, P>)),
            void>* = nullptr)
        noexcept(std::is_nothrow_constructible_v<std::decay_t<P>, P>)
    {
        initialize<std::decay_t<P>>(std::forward<P>(p));
    }
#endif // defined(_MSC_VER)

public:
    template<class P, class... Args,
    std::enable_if_t<(
        (details::is_proxiable_applicable<P, F>) &&
        (std::is_constructible_v<P, Args...>)),
        void>* = nullptr>
    explicit proxy(std::in_place_type_t<P>, Args&&... args)
        noexcept(std::is_nothrow_constructible_v<P, Args...>)
    {
        initialize<P>(std::forward<Args>(args)...);
    }

    template<class P, class U, class... Args,
    std::enable_if_t<(
        (details::is_proxiable_applicable<P, F>) &&
        (std::is_constructible_v<P, std::initializer_list<U>&, Args...>)),
        void>* = nullptr>
    explicit proxy(std::in_place_type_t<P>, std::initializer_list<U> il,
                   Args&&... args)
        noexcept(std::is_nothrow_constructible_v<
                 P, std::initializer_list<U>&, Args...>)
    {
        initialize<P>(il, std::forward<Args>(args)...);
    }

    ////////////////////
    // Copy assignment
    ////////////////////
public:
    proxy& operator=(const proxy& rhs)
        noexcept(F::constraints.copyability     >= constraint_level::nothrow &&
                 F::constraints.destructibility >= constraint_level::nothrow)
    {
        return copy_assign(rhs);
    }

private:
    template<class PX,
    std::enable_if_t<!PX::can_copy || !PX::can_dtor, void>* = nullptr>
    proxy& copy_assign(const PX& ) = delete;

    template<class PX,
    std::enable_if_t<PX::can_copy && PX::can_dtor, void>* = nullptr>
    proxy& copy_assign(const PX& rhs)
        noexcept(F::constraints.copyability     >= constraint_level::nothrow &&
                 F::constraints.destructibility >= constraint_level::nothrow)
    {
        if (this != &rhs)
        {
            if constexpr (
                F::constraints.destructibility != constraint_level::trivial)
            {
                // May throw.
                std::destroy_at(this);
            }
            // Copy construct, may throw.
            std::construct_at(this, rhs);
        }
        return *this;
    }

    ////////////////////
    // Move assignment
    ////////////////////
public:
    proxy& operator=(proxy&& rhs)
        noexcept(
            (F::constraints.relocatability >= constraint_level::nothrow ||
             (F::constraints.relocatability == constraint_level::none &&
              F::constraints.copyability    >= constraint_level::nothrow)) &&
            F::constraints.destructibility >= constraint_level::nothrow)
    {
        return move_assign(std::move(rhs));
    }

private:
    template<class PX,
    std::enable_if_t<(!PX::can_copy && !PX::can_move) ||
                      !PX::can_dtor, void>* = nullptr>
    proxy& move_assign(PX&& ) = delete;

    template<class PX,
    std::enable_if_t<PX::can_copy && !PX::can_move &&
                     PX::can_dtor, void>* = nullptr>
    proxy& move_assign(PX&& rhs)
        noexcept(F::constraints.copyability     >= constraint_level::nothrow &&
                 F::constraints.destructibility >= constraint_level::nothrow)
    {
        return copy_assign(rhs);
    }

    template<class PX,
    std::enable_if_t<PX::can_move &&
                     PX::can_dtor, void>* = nullptr>
    proxy& move_assign(PX&& rhs)
        noexcept(F::constraints.relocatability  >= constraint_level::nothrow &&
                 F::constraints.destructibility >= constraint_level::nothrow)
    {
        if (this != &rhs)
        {
            if constexpr (
                F::constraints.destructibility != constraint_level::trivial)
            {
                // May throw.
                std::destroy_at(this);
            }
            // May throw.
            std::construct_at(this, std::move(rhs));
        }
        return *this;
    }

    ////////////////////
    // Emplace assignment
    ////////////////////
public:
    proxy& operator=(std::nullptr_t)
        noexcept(F::constraints.destructibility >= constraint_level::nothrow)
    {
        reset();
        return *this;
    }

    template<class P,
    std::enable_if_t<(
        (!std::is_same_v<std::decay_t<P>, proxy>) &&
        (details::is_proxiable_applicable<std::decay_t<P>, F>) &&
        (std::is_constructible_v<std::decay_t<P>, P>) &&
        // NOTE: MSVC C++17 has an issue that member fields of constexpr object
        //       (e.g., `F::constraints`) cannot be evaluated directly within
        //       the conditional part of the `enable_if<>`.
        //       Thus `F::constraints.destructibility != constraint_level::none`
        //       cannot be written here directly.
        (can_dtor)),
        void>* = nullptr>
    proxy& operator=(P&& ptr)
        noexcept((std::is_nothrow_constructible_v<std::decay_t<P>, P>) &&
                 (F::constraints.destructibility >= constraint_level::nothrow))
    {
        reset();
        initialize<std::decay_t<P>>(std::forward<P>(ptr));
        return *this;
    }

public:
    template<class P, class... Args,
    std::enable_if_t<(
        (details::is_proxiable_applicable<P, F>) &&
        (std::is_constructible_v<P, Args...>) &&
        // NOTE: MSVC C++17 has an issue that member fields of constexpr object
        //       (e.g., `F::constraints`) cannot be evaluated directly within
        //       the conditional part of the `enable_if<>`.
        //       Thus `F::constraints.destructibility != constraint_level::none`
        //       cannot be written here directly.
        (can_dtor)),
        void>* = nullptr>
    P& emplace(Args&&... args)
        noexcept((std::is_nothrow_constructible_v<P, Args...>) &&
                 (F::constraints.destructibility >= constraint_level::nothrow))
    {
        reset();
        return initialize<P>(std::forward<Args>(args)...);
    }

    template<class P, class U, class... Args,
    std::enable_if_t<(
        (details::is_proxiable_applicable<P, F>) &&
        (std::is_constructible_v<P, std::initializer_list<U>&, Args...>) &&
        // NOTE: MSVC C++17 has an issue that member fields of constexpr object
        //       (e.g., `F::constraints`) cannot be evaluated directly within
        //       the conditional part of the `enable_if<>`.
        //       Thus `F::constraints.destructibility != constraint_level::none`
        //       cannot be written here directly.
        (can_dtor)),
        void>* = nullptr>
    P& emplace(std::initializer_list<U> il, Args&&... args)
        noexcept((std::is_nothrow_constructible_v<
                  P, std::initializer_list<U>&, Args...>) &&
                 (F::constraints.destructibility >= constraint_level::nothrow))
    {
        reset();
        return initialize<P>(il, std::forward<Args>(args)...);
    }

    ////////////////////
    // Destructor
    ////////////////////
public:
    /**
     * @brief Destructor.
     *
     * @post The proxiable object held by `ptr_` is destructed.
     * @post `ia_meta_` is **not** reset, since the destructor of
     *       `composite_accessor_meta<>` does not reset.
     */
    ~proxy(void)
        // NOTE: If destructibility is none, the proxy would **not** be nothrow
        // destructible, thus would **not** be nothrow constructible.
        noexcept(F::constraints.destructibility >= constraint_level::nothrow)
    {
        static_assert(F::constraints.destructibility != constraint_level::none);
        if constexpr (
            F::constraints.destructibility == constraint_level::nontrivial ||
            F::constraints.destructibility == constraint_level::nothrow)
        {
            if (meta_ptr().has_value())
            {
                meta_ptr()->facade_traits::destructibility_meta
                            ::dispatcher(*ptr_);
            }
        }
    }

    ////////////////////
    // Other
    ////////////////////
public:
    void reset(void)
        noexcept(F::constraints.destructibility >= constraint_level::nothrow)
    {
        std::destroy_at(this);
        meta_ptr().reset();
    }

    bool has_value() const noexcept { return meta_ptr().has_value(); }

    explicit operator bool() const noexcept { return meta_ptr().has_value(); }

    friend bool operator==(const proxy& lhs, std::nullptr_t) noexcept
    {
        return !lhs.has_value();
    }

    friend bool operator!=(const proxy& lhs, std::nullptr_t) noexcept
    {
        return lhs.has_value();
    }

    friend bool operator==(std::nullptr_t, const proxy& rhs) noexcept
    {
        return !rhs.has_value();
    }

    friend bool operator!=(std::nullptr_t, const proxy& rhs) noexcept
    {
        return rhs.has_value();
    }

    ////////////////////
    // Swap
    ////////////////////
private:
    static constexpr bool can_swap =
        F::constraints.relocatability >= constraint_level::nontrivial ||
        F::constraints.copyability    == constraint_level::trivial;

public:
    template<class PX,
    std::enable_if_t<std::is_same_v<PX, proxy> && can_swap, void>* = nullptr>
    void swap(PX& rhs)
        noexcept(F::constraints.relocatability >= constraint_level::nothrow ||
                 F::constraints.copyability    == constraint_level::trivial)
    {
        if constexpr (
            F::constraints.relocatability == constraint_level::trivial ||
            F::constraints.copyability    == constraint_level::trivial)
        {
            std::swap(ia_meta_, rhs.ia_meta_);
            std::swap(ptr_, rhs.ptr);
        }
        else
        {
            if (meta_ptr().has_value())
            {
                if (rhs.meta_ptr().has_value())
                {
                    proxy temp = std::move(*this);
                    std::construct_at(this, std::move(rhs));
                    std::construct_at(&rhs, std::move(temp));
                }
                else
                {
                    std::construct_at(&rhs, std::move(*this));
                }
            }
            else if (rhs.meta_ptr().has_value())
            {
                std::construct_at(this, std::move(rhs));
            }
        }
    }

    friend void swap(proxy& lhs, proxy& rhs)
        noexcept(noexcept(lhs.swap(rhs)))
    {
        lhs.swap(rhs);
    }

    ////////////////////
    // operator->()
    ////////////////////
public:
    template<class _ = void,
    std::enable_if_t<(std::is_void_v<_> &&
        facade_traits::has_indirection),
        void>* = nullptr>
    auto operator->() noexcept
    {
        indirect_accessor& ia = ia_meta_;
        return std::addressof(ia);
    }

    template<class _ = void,
    std::enable_if_t<(std::is_void_v<_> &&
         facade_traits::has_indirection),
        void>* = nullptr>
    auto operator->() const noexcept
    {
        const indirect_accessor& ia = ia_meta_;
        return std::addressof(ia);
    }

    ////////////////////
    // operator*()
    ////////////////////
public:
    template<class _ = void,
    std::enable_if_t<(std::is_void_v<_> &&
         facade_traits::has_indirection),
        void>* = nullptr>
    auto& operator*() & noexcept
    {
        indirect_accessor& ia = ia_meta_;
        return ia;
    }

    template<class _ = void,
    std::enable_if_t<(std::is_void_v<_> &&
        facade_traits::has_indirection),
        void>* = nullptr>
    auto& operator*() const& noexcept
    {
        const indirect_accessor& ia = ia_meta_;
        return ia;
    }

    template<class _ = void,
    std::enable_if_t<((std::is_void_v<_> &&
        facade_traits::has_indirection)),
        void>* = nullptr>
    auto&& operator*() && noexcept
    {
        indirect_accessor& ia = ia_meta_;
        return std::forward<indirect_accessor>(ia);
    }

    template<class _ = void,
    std::enable_if_t<(std::is_void_v<_> &&
        facade_traits::has_indirection),
        void>* = nullptr>
    auto&& operator*() const&& noexcept
    {
        const indirect_accessor& ia = ia_meta_;
        return std::forward<const indirect_accessor>(ia);
    }

private:
    /**
     * @brief Initialize a proxy.
     *
     * @tparam P The proxiable type.\n
     *           It **must** be a value type deduced from `std::decay_t<>`.
     * @tparam Args The argument types of the constructor of `P`.
     * @param[in] args The arguments to construct an object of `P`.
     *
     * @return The constructed object of `P`.
     */
    template<class P, class... Args>
    P& initialize(Args&&... args)
    {
        assert(!meta_ptr().has_value());
        // May throw.
        std::construct_at(reinterpret_cast<P*>(ptr_),
                          std::forward<Args>(args)...);
        // If the construction of the proxiable throws, `ia_meta_` remains null.
        ia_meta_ = composite_accessor_meta{std::in_place_type<P>};
        return *std::launder(reinterpret_cast<P*>(ptr_));
    }

          MetaPtr& meta_ptr(void)       noexcept { return ia_meta_.meta_ptr_; }
    const MetaPtr& meta_ptr(void) const noexcept { return ia_meta_.meta_ptr_; }

          indirect_accessor ia(void)       noexcept { return ia_meta_; }
    const indirect_accessor ia(void) const noexcept { return ia_meta_; }

    /// The table of dispatcher function pointers.
    composite_accessor_meta ia_meta_;
    /// The storage that holds the proxiable object.
    alignas(F::constraints.max_align) std::byte ptr_[F::constraints.max_size];
};


////////////////////////////////////////////////////////////////////////////////
/**
 * @brief Facade builder.
 *
 * @tparam CT     The tuple of conventions.
 * @tparam Constr The `proxiable_ptr_constraints_impl<>` type.
 *
 * `facade<>` provides the following members for diagnostic:
 * * V: `proxiable_applicable<P> : bool`
 * * F: `check_proxiable_applicable<diagnose_level, P>() -> bool`
 */
template<class CT = std::tuple<>,
         class Constr = details::proxiable_ptr_constraints_impl<>>
struct basic_facade_builder
{
    basic_facade_builder(void) = delete;

    template<class D, class... Os>
    using add_indirect_convention = basic_facade_builder<
                details::add_conv_t<CT, false, D, Os...>, Constr>;

    template<class D, class... Os>
    using add_direct_convention = basic_facade_builder<
                details::add_conv_t<CT, true, D, Os...>, Constr>;

    template<class D, class... Os>
    using add_convention = add_indirect_convention<D, Os...>;

    template<std::size_t max_size, std::size_t max_align = max_size>
    using restrict_layout = basic_facade_builder<
        CT, typename Constr::template restrict_layout<max_size, max_align>>;

    template<constraint_level level>
    using support_copy = basic_facade_builder<
        CT, typename Constr::template support_copy<level>>;

    template<constraint_level level>
    using support_relocation = basic_facade_builder<
        CT, typename Constr::template support_relocation<level>>;

    template<constraint_level level>
    using support_destruction = basic_facade_builder<
        CT, typename Constr::template support_destruction<level>>;

    using build = details::facade<CT, Constr>;
};

using facade_builder = basic_facade_builder<>;


} // namespace pro
} // namespace nsfx


////////////////////////////////////////////////////////////////////////////////
/// If there is 1 argument:
/// NSFX_NAME_MACRO_N(MACRO, A1,  3,  2,   ,,)     => MACRO
///                                       N is nothing
/// If there are 2 arguments:
/// NSFX_NAME_MACRO_N(MACRO, A1, A2,  3,  2,,)     => MACRO2
///                                       N is 2
/// If there are 3 arguments:
/// NSFX_NAME_MACRO_N(MACRO, A1, A2, A3,  3,  2,,) => MACRO3
///                                       N is 3
#define NSFX_PRO_NAME_MACRO_N(MACRO, a1, a2, a3, N, ...)  MACRO##N

#define NSFX_PRO_EXPAND_MACRO(X)  X

#define NSFX_PRO_EXPAND_MACRO_N(MACRO, ...)       \
    NSFX_PRO_EXPAND_MACRO(NSFX_PRO_NAME_MACRO_N(  \
        MACRO, __VA_ARGS__, 3, 2,,)(__VA_ARGS__))

/**
 * @brief Define a dispatch type for member function.
 *
 * @param FUNC  The name of the member function.
 * @param FNAME The name of the accessor function.
 */
#define NSFX_PRO_DEF_ACCESSOR_IMPL2(FUNC, FNAME)                                \
    template<class F, bool IS_DIRECT, class D, class... Os>                     \
    struct NSFX_ENFORCE_EBO accessor { accessor(void) = delete; };                               \
    /* When there are two or more overloads, this specialization is chosen. */  \
    template<class F, bool IS_DIRECT, class D, class O1, class O2, class... Os> \
    struct NSFX_ENFORCE_EBO accessor<F, IS_DIRECT, D, O1, O2, Os...>            \
        : accessor<F, IS_DIRECT, D, O1>                                         \
        , accessor<F, IS_DIRECT, D, O2>                                         \
        , accessor<F, IS_DIRECT, D, Os>...                                      \
    {                                                                           \
        /* Collect the overloads from all base accessors.                   */  \
        using accessor<F, IS_DIRECT, D, O1>::FNAME;                             \
        using accessor<F, IS_DIRECT, D, O2>::FNAME;                             \
        using accessor<F, IS_DIRECT, D, Os>::FNAME...;                          \
    };                                                                          \
    template<class F, bool IS_DIRECT, class D, class R, class... Args>          \
    struct accessor<F, IS_DIRECT, D, R(Args...)>                                \
    {                                                                           \
        R FNAME(Args&&... args)                                                 \
        {                                                                       \
            return ::nsfx::pro::details::proxy_invoke<                          \
                    IS_DIRECT, D, R(Args...)>(                                  \
                        ::nsfx::pro::details::access_proxy<F>(*this),           \
                        std::forward<Args>(args)...);                           \
        }                                                                       \
    };                                                                          \
    template<class F, bool IS_DIRECT, class D, class R, class... Args>          \
    struct accessor<F, IS_DIRECT, D, R(Args...) noexcept>                       \
    {                                                                           \
        R FNAME(Args&&... args) noexcept                                        \
        {                                                                       \
            return ::nsfx::pro::details::proxy_invoke<                          \
                    IS_DIRECT, D, R(Args...) noexcept>(                         \
                        ::nsfx::pro::details::access_proxy<F>(*this),           \
                        std::forward<Args>(args)...);                           \
        }                                                                       \
    };                                                                          \
    template<class F, bool IS_DIRECT, class D, class R, class... Args>          \
    struct accessor<F, IS_DIRECT, D, R(Args...) &>                              \
    {                                                                           \
        R FNAME(Args&&... args) &                                               \
        {                                                                       \
            return ::nsfx::pro::details::proxy_invoke<                          \
                    IS_DIRECT, D, R(Args...) &>(                                \
                        ::nsfx::pro::details::access_proxy<F>(*this),           \
                        std::forward<Args>(args)...);                           \
        }                                                                       \
    };                                                                          \
    template<class F, bool IS_DIRECT, class D, class R, class... Args>          \
    struct accessor<F, IS_DIRECT, D, R(Args...) & noexcept>                     \
    {                                                                           \
        R FNAME(Args&&... args) & noexcept                                      \
        {                                                                       \
            return ::nsfx::pro::details::proxy_invoke<                          \
                    IS_DIRECT, D, R(Args...) & noexcept>(                       \
                        ::nsfx::pro::details::access_proxy<F>(*this),           \
                        std::forward<Args>(args)...);                           \
        }                                                                       \
    };                                                                          \
    template<class F, bool IS_DIRECT, class D, class R, class... Args>          \
    struct accessor<F, IS_DIRECT, D, R(Args...) &&>                             \
    {                                                                           \
        R FNAME(Args&&... args) &&                                              \
        {                                                                       \
            return ::nsfx::pro::details::proxy_invoke<                          \
                    IS_DIRECT, D, R(Args...) &&>(                               \
                        ::nsfx::pro::details::access_proxy<F>(*this),           \
                        std::forward<Args>(args)...);                           \
        }                                                                       \
    };                                                                          \
    template<class F, bool IS_DIRECT, class D, class R, class... Args>          \
    struct accessor<F, IS_DIRECT, D, R(Args...) && noexcept>                    \
    {                                                                           \
        R FNAME(Args&&... args) && noexcept                                     \
        {                                                                       \
            return ::nsfx::pro::details::proxy_invoke<                          \
                    IS_DIRECT, D, R(Args...) && noexcept>(                      \
                        ::nsfx::pro::details::access_proxy<F>(*this),           \
                        std::forward<Args>(args)...);                           \
        }                                                                       \
    };                                                                          \
    template<class F, bool IS_DIRECT, class D, class R, class... Args>          \
    struct accessor<F, IS_DIRECT, D, R(Args...) const>                          \
    {                                                                           \
        R FNAME(Args&&... args) const                                           \
        {                                                                       \
            return ::nsfx::pro::details::proxy_invoke<                          \
                    IS_DIRECT, D, R(Args...) const>(                            \
                        ::nsfx::pro::details::access_proxy<F>(*this),           \
                        std::forward<Args>(args)...);                           \
        }                                                                       \
    };                                                                          \
    template<class F, bool IS_DIRECT, class D, class R, class... Args>          \
    struct accessor<F, IS_DIRECT, D, R(Args...) const noexcept>                 \
    {                                                                           \
        R FNAME(Args&&... args) const noexcept                                  \
        {                                                                       \
            return ::nsfx::pro::details::proxy_invoke<                          \
                    IS_DIRECT, D, R(Args...) const noexcept>(                   \
                        ::nsfx::pro::details::access_proxy<F>(*this),           \
                        std::forward<Args>(args)...);                           \
        }                                                                       \
    };                                                                          \
    template<class F, bool IS_DIRECT, class D, class R, class... Args>          \
    struct accessor<F, IS_DIRECT, D, R(Args...) const&>                         \
    {                                                                           \
        R FNAME(Args&&... args) const&                                          \
        {                                                                       \
            return ::nsfx::pro::details::proxy_invoke<                          \
                    IS_DIRECT, D, R(Args...) const&>(                           \
                        ::nsfx::pro::details::access_proxy<F>(*this),           \
                        std::forward<Args>(args)...);                           \
        }                                                                       \
    };                                                                          \
    template<class F, bool IS_DIRECT, class D, class R, class... Args>          \
    struct accessor<F, IS_DIRECT, D, R(Args...) const& noexcept>                \
    {                                                                           \
        R FNAME(Args&&... args) const& noexcept                                 \
        {                                                                       \
            return ::nsfx::pro::details::proxy_invoke<                          \
                    IS_DIRECT, D, R(Args...) const& noexcept>(                  \
                        ::nsfx::pro::details::access_proxy<F>(*this),           \
                        std::forward<Args>(args)...);                           \
        }                                                                       \
    };                                                                          \
    template<class F, bool IS_DIRECT, class D, class R, class... Args>          \
    struct accessor<F, IS_DIRECT, D, R(Args...) const&&>                        \
    {                                                                           \
        R FNAME(Args&&... args) const&&                                         \
        {                                                                       \
            return ::nsfx::pro::details::proxy_invoke<                          \
                    IS_DIRECT, D, R(Args...) const&&>(                          \
                        ::nsfx::pro::details::access_proxy<F>(*this),           \
                        std::forward<Args>(args)...);                           \
        }                                                                       \
    };                                                                          \
    template<class F, bool IS_DIRECT, class D, class R, class... Args>          \
    struct accessor<F, IS_DIRECT, D, R(Args...) const&& noexcept>               \
    {                                                                           \
        R FNAME(Args&&... args) const&& noexcept                                \
        {                                                                       \
            return ::nsfx::pro::details::proxy_invoke<                          \
                    IS_DIRECT, D, R(Args...) const&& noexcept>(                 \
                        ::nsfx::pro::details::access_proxy<F>(*this),           \
                        std::forward<Args>(args)...);                           \
        }                                                                       \
    }

#define NSFX_PRO_DEF_ACCESSOR_IMPL(FUNC)  \
        NSFX_PRO_DEF_ACCESSOR_IMPL2(FUNC, FUNC)

/**
 * @brief Define an accessor class template.
 *
 * @param FUNC  The name of the member function.
 * @param FNAME The name of the accessor function.
 *
 * @verbatim
 * NSFX_PRO_DEF_ACCESSOR(FUNC) => NSFX_PRO_DEF_ACCESSOR_IMPL2(FUNC, FUNC)
 *
 * NSFX_PRO_DEF_ACCESSOR(FUNC, FNAME) => NSFX_PRO_DEF_ACCESSOR_IMPL2(FUNC, FNAME)
 * @endverbatim
 */
#define NSFX_PRO_DEF_ACCESSOR(...)  \
        NSFX_PRO_EXPAND_MACRO_N(NSFX_PRO_DEF_ACCESSOR_IMPL, __VA_ARGS__)

/**
 * @brief Define a dispatch type that binds to the name of member function.
 *
 * @param NAME  The name of the dispatch type.
 * @param FUNC  The name of the actual member function.
 * @param FNAME The name of the accessor member function.
 */
#define NSFX_PRO_DEF_MEM_DISPATCH_IMPL3(NAME, FUNC, FNAME)                      \
struct NAME                                                                     \
{                                                                               \
    template<class T, class... Args,                                            \
        /* The operator is avaiable if `T::FUNC(Args...)` is a callable.    */  \
        /* This SFINAE scheme is adopted to avoid compilation errors when   */  \
        /* `std::is_invocable<>` is used to examine invalid invoking forms  */  \
        /* of the dispatch type.                                            */  \
        /* When an invalid form is examined, the operator would be defined; */  \
        /* However, the noexcept specifier and the definition are invalid.  */  \
        /* e.g., `meta_provider<>` uses `invocable_dispatch_concept<>` to   */  \
        /* examine the dispatch type.                                       */  \
        ::std::enable_if_t<true, std::void_t<decltype(                          \
        ::std::declval<T>().FUNC(::std::declval<Args>()...))>>* = nullptr>      \
    decltype(auto) operator()(T&& self, Args&&... args) const                   \
        noexcept(noexcept(::std::declval<T>().FUNC(::std::declval<Args>()...))) \
    {                                                                           \
        return ::std::forward<T>(self).FUNC(::std::forward<Args>(args)...);     \
    }                                                                           \
    NSFX_PRO_DEF_ACCESSOR_IMPL2(FUNC, FNAME);                                   \
}

#define NSFX_PRO_DEF_MEM_DISPATCH_IMPL2(NAME, FUNC)  \
        NSFX_PRO_DEF_MEM_DISPATCH_IMPL3(NAME, FUNC, FUNC)

/**
 * @brief Define a dispatch type for member function.
 *
 * @param NAME  The name of the dispatch type.
 * @param FUNC  The name of the member function.
 * @param FNAME The name of the accessor function.
 */
#define NSFX_PRO_DEF_MEM_DISPATCH(NAME, ...) \
    NSFX_PRO_EXPAND_MACRO_N(NSFX_PRO_DEF_MEM_DISPATCH_IMPL, NAME, __VA_ARGS__)

/**
 * @brief Define a dispatch type that binds to the name of free function.
 *
 * @param NAME  The name of the dispatch type.
 * @param FUNC  The name of the actual free function.
 * @param FNAME The name of the accessor member function.
 */
#define NSFX_PRO_DEF_FREE_DISPATCH_IMPL3(NAME, FUNC, FNAME)                     \
struct NAME                                                                     \
{                                                                               \
    template<class T, class... Args,                                            \
        /* The operator is avaiable if `FUNC(T, Args...)` is a callable.    */  \
        /* This SFINAE scheme is adopted to avoid compilation errors when   */  \
        /* `std::is_invocable<>` is used to examine invalid invoking forms  */  \
        /* of the dispatch type.                                            */  \
        /* When an invalid form is examined, the operator would be defined; */  \
        /* However, the noexcept specifier and the definition are invalid.  */  \
        /* e.g., `meta_provider<>` uses `invocable_dispatch_concept<>` to   */  \
        /* examine the dispatch type.                                       */  \
        ::std::enable_if_t<                                                     \
        ::std::is_invocable_v<decltype(FUNC), T, Args...>, void>* = nullptr>    \
    decltype(auto) operator()(T&& self, Args&&... args) const                   \
        noexcept(noexcept(FUNC(::std::declval<T>(), ::std::declval<Args>()...)))\
    {                                                                           \
        return ::std::forward<T>(self).FUNC(::std::forward<Args>(args)...);     \
    }                                                                           \
}

#define NSFX_PRO_DEF_FREE_DISPATCH_IMPL2(NAME, FUNC, FNAME)  \
        NSFX_PRO_DEF_FREE_DISPATCH_IMPL3(NAME, FUNC, FUNC)

/**
 * @brief Define a dispatch type for member function.
 *
 * @param NAME  The name of the dispatch type.
 * @param FUNC  The name of the member function.
 * @param FNAME The name of the accessor function.
 */
#define NSFX_PRO_DEF_FREE_DISPATCH(NAME, ...) \
    NSFX_PRO_EXPAND_MACRO_N(NSFX_PRO_DEF_FREE_DISPATCH_IMPL, NAME, __VA_ARGS__)


#endif // PROXY_HPP__49B6AD65_E042_4FA8_9E5E_407FC8D2809A
