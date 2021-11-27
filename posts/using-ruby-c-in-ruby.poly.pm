* Using Ruby's C API inside Ruby
◊define-meta[date]{2021-01-18}
◊define-meta[published #t]
◊define-meta[category]{programming}

A thought occurred to me in my mask-wearing, lockdown-addled brain last night: why the hell did I choose ◊em{now} to stop drinking? It's for my own good, I told myself, and so my thoughts shifted further into the absurd with nary a mind-altering substance in sight to stop them.

One of those thoughts stuck out in particular, because of how ridiculous it sounded: could you optimise your Ruby code by using FFI with Ruby's C bindings? I'm not talking about making a native extension in pure C, I'm talking about making Ruby talk to itself through a foreign function interface using the ffi gem◊^[1].

Let's apply some method to this madness and set up some bindings, otherwise we're dead in the water. Let's be descriptive and call our FFI module ◊code{LibRuby}. No naming conflicts at all there, ◊em{no sirree}!

◊codeblock['ruby]{
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
}

If you look at the code in this module, you'll notice that I used ◊code{attach_variable} to get access to the Kernel module, and ◊code{attach_function} for the method calls. The ◊code{:id} and ◊code{:value} types are just aliases for ◊code{:pointer}, because ◊code{VALUE} and ◊code{ID} in the C API are themselves pointers. It's for the sake of documentation, so it's clearer what order you pass arguments in.

Ruby's built in modules and classes are already defined globally with a naming scheme. In this case, ◊code{Kernel} is a variable called ◊code{rb_mKernel}, where ◊code{rb} is a prefix that every C function has in common (so you know it's for Ruby as C doesn't have namespaces), and the letter ◊code{m} means ◊code{module}. If it was ◊code{c} instead it would mean ◊code{class}.

Anyway this boilerplate should give us enough to do a hello world using Ruby's C API but at runtime, in Ruby, so it's time to fire up ◊code{irb}.

◊aside{
  It should go without saying that at this point, you're not just playing with fire, you're inviting it to burn down your house. Be careful lest the `segfault`s creep up on you.
}

Let's take it from the top and talk through this ungodly incantation. Go ahead and copy that little module into your console! If it fails, make sure you've got the ◊code{ffi} gem installed◊^[2].

Once you're done, you can save some keystrokes by importing that module.

◊codeblock['ruby]{
  include LibRuby
}

In order to call ◊code{puts} in Ruby through the C API, we'll need to get a reference to the module it's defined in (◊code{Kernel}), and also get the method name as a symbol (like you might normally do with ◊code{.to_sym}).

◊codeblock['ruby]{
  kernel = LibRuby.rb_mKernel
  puts_method = rb_intern('puts')
}

Oh, before we continue, better disable the garbage collector. This is a simple way to stop the oscillating turbine from splattering unpleasant substances around the room. (More on that later, but see if you can guess why.)

◊codeblock['ruby]{
  GC.disable
}

We can't just pass in a normal string to ◊code{puts} without things going 💥, as everything is an object in Ruby and therefore we need to
get a pointer to a ◊code{String} instance (or in internal Ruby lingo, one of those ◊code{VALUE}s).

◊codeblock['ruby]{
  str = rb_str_new_cstr('welcome, mortals')
}

Now we have all of the ingredients to make the actual call, which syntactically and aesthetically blows idiomatic Ruby out of the water. Delicately paste this into your console and you should see the string printed out. You'll also get a return value like ◊code{#<FFI::Pointer address=0x0000000000000008>}, which will refer to ◊code{Qnil}. ◊code{Qnil} is a pointer to Ruby's ◊code{nil} object.

◊codeblock['ruby]{
  rb_funcall(kernel, puts_method, 1, :value, str)
}

Run it again a few times, and with different strings. If you're feeling experimental, attach more functions in ◊code{LibRuby} and see what else you can print out! Ruby's extension documentation should be a good place to start◊^[3].

◊h3{So, why disable the GC?}

For every step in this post up to creating a ◊code{String} object, we've been using function bindings and global variables. Global variables and constants won't be garbage collected, because the global scope will always maintain a reference to them; besides which, it would be quite bad if your classes and modules suddenly disappeared after a GC pass.

The string object is different, however, as on the C side of things Ruby is taking a pointer to a C string (a ◊code{const char *}), allocating memory, and giving back a pointer to the new object. Eventually the GC will run and free up the memory at the pointer's address, and the string will no longer exist. You'll probably find something else at that address instead, or just garbage.

Disabling the GC in this instance is a ◊strong{shitty hack} because it's a direct admission that the code is ◊em{not memory safe}. Hopefully you didn't need me to tell you that, though, and the quality of the code in this post was self-evident.

How would you fix it? Well, now we've found out that we ◊code{can} write Ruby with itself we'll explore that next time. And there'll be benchmarks, too.

Until then, I'll see you further into the abyss.

◊footnotes{
  ◊^[1]{◊<>["https://github.com/ffi/ffi"]}
  ◊^[2]{◊code{`gem install ffi -- --enable-system-libffi`}}
  ◊^[3]{◊<>["https://ruby-doc.org/core-2.7.0/doc/extension_rdoc.html"]}
}