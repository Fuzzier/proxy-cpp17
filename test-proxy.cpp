/**
 * @file
 *
 * @brief Test proxy.
 *
 * @author  Wei Tang <gauchyler@uestc.edu.cn>
 * @date    2025-03-20
 *
 * @copyright Copyright (c) 2025.
 *   National Key Laboratory of Science and Technology on Communications,
 *   University of Electronic Science and Technology of China.
 *   All rights reserved.
 */

#include "proxy.hpp"
#include <memory>
#include <iostream>

struct P
{
    P(void) noexcept = default;
    P(int x) noexcept : x{x} {};

    double foo(int a, float b)
    { return x + a + b; }
    double foo(float b) const
    { return x + b; }

    int bar(void) noexcept
    { return x; }
    int bar(int a) const noexcept
    { return x + a; }

    int x {0};
};

NSFX_PRO_DEF_MEM_DISPATCH(foo_mem_dispatch, foo);
NSFX_PRO_DEF_MEM_DISPATCH(bar_mem_dispatch, bar);

using F = nsfx::pro::facade_builder
    ::add_convention<foo_mem_dispatch, double(int, float), double(float) const>
    ::add_convention<bar_mem_dispatch, int(void), int(int) const>
#if defined(_MSC_VER) && (_MSC_VER < 1936)
    // NOTE: Before msvc 19.36, `std::unique_ptr<>` is neither nothrow movable
    //       nor nothrow destructible.
    ::support_relocation<nsfx::pro::constraint_level::nontrivial>
    ::support_destruction<nsfx::pro::constraint_level::nontrivial>
#endif // defined(_MSC_VER) && (_MSC_VER < 1936)
    ::build;

using PX = nsfx::pro::proxy<F>;

static_assert(F::proxiable_applicable<P*>);
static_assert(F::proxiable_applicable<std::unique_ptr<P>>);
static_assert(F::proxiable_applicable<std::shared_ptr<P>>);
static_assert(!F::proxiable_applicable<int>);
// Call this constexpr function to show diagnostic information.
// static_assert(F::assert_proxiable_applicable<int>());

static_assert(PX::proxiable_applicable<P*>);
static_assert(PX::proxiable_applicable<std::unique_ptr<P>>);
static_assert(PX::proxiable_applicable<std::shared_ptr<P>>);
static_assert(!PX::proxiable_applicable<int>);
// Call this constexpr function to show diagnostic information.
// static_assert(PX::assert_proxiable_applicable<int>());

int main(void)
{
    int x = 1;
    {
        // NOTE: In g++, `std::unique_ptr::operator*()` may not be `noexcept`.
        //       Thus the overloads of conventions shall use `noexcept` with care.
        PX px{std::make_unique<P>(x)};
        double retval = px->foo(1, 2.5f);
        std::cout << "foo(1, 2.5f) = " << retval << std::endl;
    }
    {
        PX px{std::make_shared<P>(x)};
        int retval = px->bar(1);
        std::cout << "bar(1) = " << retval << std::endl;
    }

    return 0;
}
