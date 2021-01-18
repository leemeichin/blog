---
title: Using Ruby's C API inside Ruby
date: 2021-01-18
category: programming
status: published
---

A thought occurred to me in my mask-wearing, lockdown-addled brain last night: why the hell did I choose _now_ to stop drinking? It's for my own good, I told myself, and so my thoughts shifted further into the absurd with nary a mind-altering substance in sight to stop them.

One of those thoughts stuck out in particular, because of how ridiculous it sounded: could you optimise your Ruby code by using FFI with Ruby's C bindings? I'm not talking about making a native extension in pure C, I'm talking about making Ruby talk to itself through a foreign function interface using the ffi gem[^1].

Let's apply some method to this madness and set up some bindings, otherwise we're dead in the water. Let's be descriptive and call our FFI module `LibRuby`. No naming conflicts at all there, _no sirree_!

```ruby
require 'ffi'

module LibRuby
  extend FFI::Library

  ffi_lib 'ruby'

  typedef :pointer, :value
  typedef :pointer, :id

  attach_variable :rb_mKernel, :value
  attach_function :rb_const_get, [:value, :id], :value
  attach_function :rb_intern, [:string], :id
  attach_function :rb_funcall, [:value, :id, :int, :varargs], :value
  attach_function :rb_str_new_cstr, [:string], :value
end
```

If you look at the code in this module, you'll notice that I used `attach_variable` to get access to the Kernel module, and `attach_function` for the method calls. The `:id` and `:value` types are just aliases for `:pointer`, because `VALUE` and `ID` in the C API are themselves pointers. It's for the sake of documentation, so it's clearer what order you pass arguments in.

Ruby's built in modules and classes are already defined globally with a naming scheme. In this case, `Kernel` is a variable called `rb_mKernel`, where `rb` is a prefix that every C function has in common (so you know it's for Ruby as C doesn't have namespaces), and the letter `m` means `module`. If it was `c` instead it would mean `class`.

Anyway this boilerplate should give us enough to do a hello world using Ruby's C API but at runtime, in Ruby, so it's time to fire up `irb`.

<aside>It should go without saying that at this point, you're not just playing with fire, you're inviting it to burn down your house. Be careful lest the `segfault`s creep up on you.</aside>

Let's take it from the top and talk through this ungodly incantation. Go ahead and copy that little module into your console! If it fails, make sure you've got the `ffi` gem installed[^2].

Once you're done, you can save some keystrokes by importing that module.

```ruby
include LibRuby
```

In order to call `puts` in Ruby through the C API, we'll need to get a reference to the module it's defined in (`Kernel`), and also get the method name as a symbol (like you might normally do with `.to_sym`).

```ruby
kernel = LibRuby.rb_mKernel
puts_method = rb_intern('puts')
```

Oh, before we continue, better disable the garbage collector. This is a simple way to stop the oscillating turbine from splattering unpleasant substances around the room. (More on that later, but see if you can guess why.)

```ruby
GC.disable
```

We can't just pass in a normal string to `puts` without things going 💥, as everything is an object in Ruby and therefore we need to
get a pointer to a `String` instance (or in internal Ruby lingo, one of those `VALUE`s).

```ruby
str = rb_str_new_cstr('welcome, mortals')
```

Now we have all of the ingredients to make the actual call, which syntactically and aesthetically blows idiomatic Ruby out of the water. Delicately paste this into your console and you should see the string printed out. You'll also get a return value like `#<FFI::Pointer address=0x0000000000000008>`, which will refer to `Qnil`. `Qnil` is a pointer to Ruby's `nil` object.

```ruby
rb_funcall(kernel, puts_method, 1, :value, str)
```

Run it again a few times, and with different strings. If you're feeling experimental, attach more functions in `LibRuby` and see what else you can print out! Ruby's extension documentation should be a good place to start[^3].

### So, why disable the GC?

For every step in this post up to creating a `String` object, we've been using function bindings and global variables. Global variables and constants won't be garbage collected, because the global scope will always maintain a reference to them; besides which, it would be quite bad if your classes and modules suddenly disappeared after a GC pass.

The string object is different, however, as on the C side of things Ruby is taking a pointer to a C string (a `const char *`), allocating memory, and giving back a pointer to the new object. Eventually the GC will run and free up the memory at the pointer's address, and the string will no longer exist. You'll probably find something else at that address instead, or just garbage.

Disabling the GC in this instance is a **shitty hack** because it's a direct admission that the code is _not memory safe_. Hopefully you didn't need me to tell you that, though, and the quality of the code in this post was self-evident.

How would you fix it? Well, now we've found out that we _can_ write Ruby with itself we'll explore that next time. And there'll be benchmarks, too.

Until then, I'll see you further into the abyss.

[^1]: <https://github.com/ffi/ffi>
[^2]: `gem install ffi -- --enable-system-libffi`
[^3]: <https://ruby-doc.org/core-2.7.0/doc/extension_rdoc.html>
