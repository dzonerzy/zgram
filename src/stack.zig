//! How far down the native stack a parse may go.
//!
//! The generated parser recurses once per nesting level of the input, so
//! deeply nested input (`((((...` a hundred thousand deep) would run the
//! thread out of stack and crash the process. Recursive rules compare their
//! frame address with the limit computed here and fail the parse instead.

const std = @import("std");
const builtin = @import("builtin");

/// Kept free below the limit: the frames between two checks (checks sit on
/// recursion cycles, not in every rule), the helpers the parser calls, and
/// whatever the caller does with the error.
const MARGIN: usize = 256 * 1024;

/// When the thread's stack can't be found: this far below the caller
const FALLBACK_BUDGET: usize = 256 * 1024;

/// The lowest address of the current thread's stack, found once per thread
threadlocal var cached_low: usize = 0;
threadlocal var cached_size: usize = 0;
threadlocal var looked_up: bool = false;

const pthread_t = std.c.pthread_t;
const pthread_attr_t = std.c.pthread_attr_t;
extern "c" fn pthread_getattr_np(thread: pthread_t, attr: *pthread_attr_t) c_int;
extern "c" fn pthread_attr_getstack(attr: *const pthread_attr_t, addr: *?*anyopaque, size: *usize) c_int;
extern "c" fn pthread_attr_destroy(attr: *pthread_attr_t) c_int;
extern "c" fn pthread_get_stackaddr_np(thread: pthread_t) ?*anyopaque;
extern "c" fn pthread_get_stacksize_np(thread: pthread_t) usize;
extern "kernel32" fn GetCurrentThreadStackLimits(low: *usize, high: *usize) callconv(.winapi) void;

fn lookUp() void {
    looked_up = true;
    switch (builtin.os.tag) {
        .windows => {
            var low: usize = 0;
            var high: usize = 0;
            GetCurrentThreadStackLimits(&low, &high);
            if (high > low) {
                cached_low = low;
                cached_size = high - low;
            }
        },
        .linux => {
            var attr: pthread_attr_t = undefined;
            if (pthread_getattr_np(std.c.pthread_self(), &attr) != 0) return;
            defer _ = pthread_attr_destroy(&attr);
            var addr: ?*anyopaque = null;
            var size: usize = 0;
            if (pthread_attr_getstack(&attr, &addr, &size) != 0 or addr == null) return;
            cached_low = @intFromPtr(addr.?);
            cached_size = size;
        },
        .macos, .ios, .tvos, .watchos => {
            const self_thread = std.c.pthread_self();
            const top = @intFromPtr(pthread_get_stackaddr_np(self_thread) orelse return);
            const size = pthread_get_stacksize_np(self_thread);
            if (size == 0 or size > top) return;
            cached_low = top - size;
            cached_size = size;
        },
        else => {},
    }
}

/// The address below which a parse on this thread must not put a frame.
/// `here` is an address in the caller's frame.
pub fn limit(here: usize) usize {
    if (!looked_up) lookUp();
    if (cached_size == 0 or here < cached_low or here - cached_low > cached_size) {
        // Unknown stack (or not the one we looked up: a coroutine's): a
        // fixed budget below the caller
        return if (here > FALLBACK_BUDGET) here - FALLBACK_BUDGET else 0;
    }
    // On a small stack the margin is a share of it, so that there is
    // something left to parse with
    return cached_low + @min(MARGIN, cached_size / 3);
}
