---
title: Gettin' Ziggy With It On The Pi Zero
date: 2021-01-05
status: published
category: programming
---

Alright, you can read the article first and shoot me later for a title like that, and what will inevitably become a series of Zig-based puns.

Zig, for the unaware, is a fancy language that looks to be to C what Rust is to C++. Honestly, I recommend you read the summary on the main page[^1] to find out more yourself, as the best I can do is to just parrot what has already been written. However, you can see it as a valid _alternative_ to C and Zig itself has claimed that it wants to be a better version of C than C itself. An ambitious challenge, for sure. To that end, Zig itself ships its own C compiler.

I've been interested in giving Zig a spin for quite a while, and once my Raspberry Pi Zero W[^2] and OLED display[^3] arrived in the post, I decided that this would be my best opportunity to try it out. I'm not really going to cover the process of wiring up the hardware, suffice to say that once you've got your Pi Zero you'll need to be able to SSH into it, and that you'll need a [solderless] GPIO header[^4] to plug the OLED display into. I recommend the Zero **W** because the W means 'WiFi', which means that if you connect it to your network you can SSH in without faffing around with USB cables and what not. It's not a requirement, though.

With that out of the way, let's see if we can write something in Zig to power this little display. It's going to be a simple program that simply fills the entire screen by turning the pixels from black (off) to white (on). As an extra challenge, we will do this without pulling in dependencies like WiringPi[^5], or relying on existing drivers, as lovely as they are.

Instead, we will be directly using the i<sup>2</sup>c dev interface[^6]. If you're using Debian and/or Ubuntu on your Pi and your own machine, you can grab these libraries with a simple `sudo apt install i2c-dev`. You will need to enable i<sup>2</sup>c on your Pi separately though, through `sudo raspi-config`[^7].

Ready to... get Ziggy with it? Oh, I bet you are. 😋 If you want to skip to the end and just grab the code, though, you can find this all on GitHub[^8]. I called it Stardust, like *Zig*gy Stardust. Get it?

🥁

---

## Hello, Pi.

The first and most complicated part of any low-level project is the bit where you try and establish a build system of some sorts. We're going to forget about that completely for now and apply some elbow-grease to the situation.

The next step is to define a `main` function that grabs a file descriptor (or handle) corresponding to our OLED display. According to the aforementioned dev interface docs, we'll need to open a file and check it with `ioctl`.

```zig
const std = @import("std");

const c = @cImport({
  @cInclude("linux/i2c.h");
  @cInclude("linux/i2c-dev.h");
  @cInclude("sys/ioctl.h");
});

const i2c_device = "/dev/i2c-1"; // this is assumed correct on a Pi Zero, but may be i2c-0 on an older Pi.
const i2c_addr: c_int = 0x3c; // this is typed as a C-style int for ABI compatibility with C

pub fn main() !void {
  const stdout = std.io.getStdOut().outStream();

  const fd = try fs.openFileAbsolute(i2c_device, fs.File.OpenFlags{ .write = true, .read = true });
  defer fd.close();

  if (c.ioctl(fd.handle, c.I2C_SLAVE, i2c_addr) < 0)) {
    try stdout.print("ioctl failed, errno: {}\n", c.errno);
  }

  stdout.print("Init successful.\n", .{});
}
```

You might have noticed something odd: we're not really writing much Zig here, it's practically 95% interop with C. The beauty of Zig is that this interop is so simple and intuitive that it's the _easiest_ way to get started if you're going to be linking against existing C libraries. Get the software working first, abstract it later, as they say, and you might already start to get an idea of what we could convert into idiomatic Zig libraries in future.

The actual Zig code you see though, is quite different to the C stuff. That `defer fd.close()`, for example, _ensures_ that the file descriptor we opened up will be closed when we're done. If we don't do that, then it'll stay open and there'll be a leak.

There's also the `try` macro, used in combination with the `!void` return type, which will be super familiar if you've written some Rust and have dealt with option types. It's short hand for executing the code and catching/dealing with the error, with `!void` being another shorthand for `anyerror!void`, namely: this function returns either nothing, or an error if there is one.

WHat we've actually done, however, is open the device file `/dev/i2c-1`, and then used the `ioctl` library to specify which device in particular we want to talk to. You can find out this value by running `i2cdevice -y 1`, like so:

```
pi@raspberrypi:~ $ i2cdetect -y 1
     0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f
00:          -- -- -- -- -- -- -- -- -- -- -- -- --
10: -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
20: -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
30: -- -- -- -- -- -- -- -- -- -- -- -- 3c -- -- --
40: -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
50: -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
60: -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
70: -- -- -- -- -- -- -- --
```

<aside>In my case, the device can be accessed at address `0x3C`, which is how I defined `i2c_addr` above.</aside>

We're at a good point now to try and compile this thing and then run it on the Pi. If we get the message 'Init successful.' then we're golden.

---

## Build and Push

Zig comes with a nice little build system out of the box, but we're not going to use it right now because it's a work in progress. I'll leave that as an exercise to you, the reader, and I urge you to contribute any documentation you come up with to Zig. Instead, we'll use the CLI which is just as powerful and, gracefully, a bit more discoverable for our purposes.

Are you writing this code on the Pi itself? Probably not, I imagine, and nor do you need to.

> Cross-compiling is a first-class use case
>
> Andrew Kelley, Creator of Zig

Let's build a binary, then. Save your code into a file, say, `stardust.zig` and then proceed.

```console
zig build-exe stardust.zig  -target arm-linux-musleabihf -mcpu arm1176jzf_s -O ReleaseSafe -lc
```

To unpack that a little, the `target` is a triplet stating that we want to build this using the musl[^9] libc ABI, on a 32bit ARM architecture. `mcpu` goes along with that to make sure the resulting binary will work on our Pi Zero. I grabbed these values from an issue on Zig's github repo[^10], so credit goes to the author of that issue for unintentionally guiding me forward.

Passing the optimiser flag (`-O`) isn't strictly necessary, so you can omit this if you require a debug build and stack traces with errors.

`-lc` basically says that this binary needs to be linked against libc.

Once the build finishes, you should find a shiny new executable called `stardust` in the same directory as your code. You can get it onto your Pi with `scp`, like so:

```console
scp stardust pi@raspberrypi:~/stardust
```

<aside>You will need to change `pi@raspberrypi` to whatever else you've configured if you've changed the defaults.</aside>

SSH into your Pi after that, and try and run it! Does it return successfully? I hope so!

Let's move on and make this kitten purr. Meow 🐈.

---

## Getting this show on the road

In true _draw the rest of the fucking owl_ fashion[^11], what follows is a bit of a code-dump since the primary method of communicating with your OLED display is to, literally, write a few bytes to a file. The registers available and what can be written to them are often described in a meticulously detailed datasheet[^12], but they're not exactly light reading and we can save a bit of time by grabbing the info from elsewhere. A lot of the constants that follow are gracefully derived from those listed in a certain `owenosborn`'s wiringPi-based driver.[^13]. Credit where credit's due, eh.

```zig
const SET_CONTRAST = 0x81;
const SET_DISPLAY_ALL_ON_RESUME = 0xA4;
const SET_DISPLAY_ALL_ON = 0xA5;
const SET_NORMAL_DISPLAY = 0xA6;
const SET_INVERT_DISPLAY = 0xA7;
const SET_DISPLAY_OFF = 0xAE;
const SET_DISPLAY_ON = 0xAF;
const SET_DISPLAY_OFFSET = 0xD3;
const SET_COLUMN_ADDR = 0x21;
const SET_PAGE_ADDR = 0x22;
const SET_COM_PINS = 0xDA;
const SET_VCOM_DETECT = 0xDB;
const SET_DISPLAY_CLOCK_FREQ = 0xD5;
const SET_PRECHARGE = 0xD9;
const SET_MULTIPLEX_RATIO = 0xA8;
const SET_LOW_COLUMN = 0x00;
const SET_HIGH_COLUMN = 0x10;
const SET_START_LINE = 0x40;
const SET_START_PAGE = 0xB0;
const SET_MEMORY_MODE = 0x20;
const SET_COM_SCAN_INC = 0xC0;
const SET_COM_SCAN_DEC = 0xC8;
const SET_SEG_REMAP = 0xA0;
const SET_CHARGE_PUMP = 0x8D;
```

The registers available to an i<sup>2</sup>c compatible device will depend on the device itself, so it's not really safe to copy and paste these without knowing exactly what you're dealing with. This is driver level code so it's not like you'll get some fancy validation error if you write the wrong bytes, you'll more likely fuck it up and burn down your house[^14].

Next we'll want to init the display and get it into a clean state, with the cursor pointing at the first pixel.

```zig
fn init_display(fd: fs.File) !void {
    const cmds = [_]u8{
        SET_MULTIPLEX_RATIO, 0x3F,                   0x00,
        SET_START_LINE,      SET_SEG_REMAP,          SET_COM_SCAN_DEC,
        SET_COM_PINS,        0x32,                   SET_DISPLAY_ALL_ON_RESUME,
        SET_NORMAL_DISPLAY,  SET_DISPLAY_CLOCK_FREQ, 0x80,
        SET_CHARGE_PUMP,     0x14,                   SET_MEMORY_MODE,
        0x20,
    };

    inline for (cmds) |cmd| {
        _ = try fd.write(&[2]u8{ 0x00, cmd });
    }
}

fn display_off(fd: fs.File) !void {
    _ = try fd.write(&[2]u8{ 0x00, SET_DISPLAY_OFF });
}

fn display_on(fd: fs.File) !void {
    _ = try fd.write(&[2]u8{ 0x00, SET_DISPLAY_ON });
}

fn reset_cursor(fd: fs.File) !void {
    const cmds = [_]u8{
        SET_COLUMN_ADDR,
        0x00,
        0x7F,
        SET_PAGE_ADDR,
        0x00,
        0x07,
    };

    inline for (cmds) |cmd| {
        _ = try fd.write(&[2]u8{ 0x00, cmd });
    }
}
```

Wow, actual Zig code! The formatting may look a little odd because that's what `zig fmt` decides is appropriate.

`init_display` is quite a complex beast that issues a whole series of commands that sets up the display for further use. A more detailed explanation of that will be in another post, for the sake of brevity, but in essence it was adapted from AdaFruit's CircuitPi driver, written in Python[^15].

The recurring theme in all of these new functions is that the entire basis of their existence is to create an array of two bytes, and then write them to file descriptor we opened right at the start. The data structure looks something like this:

```c
buf[0] = 0x00; // the register to be written to
buf[1] = 0x??; // the value to assign to that register
```

The file opened in `main` isn't a traditional file as you know it, but it points to all of the devices connected to your GPIO header on the Pi. Therefore, if you know enough about the hardware at a low enough level, you can control all of them by writing the right bytes to the right register, at the right address.

The rest of the code, e.g. `reset_cursor`, resets the state of the display in such a way that you can write a pixel and the cursor will advance, linearly, to the next one.

```zig
fn fill(fd: fs.File) !void {
    var i: usize = 0;

    while (i < 1024) {
        _ = try fd.write(&[2]u8{ 0x40, 0xFF });
        i += 1;
    }
}
```

This `fill` function will (rather quickly) turn the display solid white, updating each pixel one at a time. Before we continue though, let's go through some more Zig specifics; namely, `inline`.

---

## A zig-a-Zig aaaahhhh...

<aside>Reach out to me at [pleasemakeitstop@mrlee.dev](mailto:pleasemakeitstop@mrlee.dev) if this is too much for you.</aside>

Zig has some nice language features intended to replace and improve upon C/C++ preprocessor macros. The `inline` keyword is one such thing, and when applied to a `for` or `while` loop it'll unroll it at compile time. A simple optimisation but a useful one. We don't use it, but you also have `comptime`, which is powerful enough to be able to implement generics, if you so desire. We're not going to go into that here though, and you can read more about it from a certain Loris Cro[^16].

---

This post is getting pretty long-winded, and all I wanted to do was show how to set some pixels on a tiny display. Let's wrap this up then, since we're almost ready to recompile. Just one finishing touch, which is to call the functions we defined. Update `main` to look like this:

```zig
pub fn main() !void {
    const stdout = std.io.getStdOut().outStream();
    const fd = try fs.openFileAbsolute(i2c_device, fs.File.OpenFlags{ .write = true, .read = true });
    defer fd.close();

    if (c.ioctl(fd.handle, c.I2C_SLAVE, i2c_addr) < 0) {
        try stdout.print("ioctl failed, errno: {}\n", c.errno);
        return;
    }

    try stdout.print("init\n", .{});
    try display_off(fd);
    try init_display(fd);
    try display_on(fd);
    try reset_cursor(fd);

    try stdout.print("turn on\n", .{});
    try display_on(fd);

    try stdout.print("fill\n", .{});
    try fill(fd);
}
```

Once you're done, rebuild the binary and `scp` it over, like you did the first time. SSH into your Pi and run it again (i.e `./stardust`), and see your display light up! 🥳

---

Hopefully that worked, but if it didn't, get in touch with your feedback at [wtf@mrlee.dev](mailto:wtf@mrlee.dev) and help contribute to this post being a better, more informative read. After all, _works on my machine!_ can only go so far.

[^1]: <https://ziglang.org>
[^2]: <https://thepihut.com/products/raspberry-pi-zero-w>
[^3]: <https://thepihut.com/products/adafruit-pioled-128x32-monochrome-oled-add-on-for-raspberry-pi-ada3527>
[^4]: <https://thepihut.com/products/gpio-hammer-header-solderless>
[^5]: <http://wiringpi.com/>
[^6]: <https://www.kernel.org/doc/Documentation/i2c/dev-interface>
[^7]: <https://learn.adafruit.com/adafruits-raspberry-pi-lesson-4-gpio-setup/configuring-i2c>
[^8]: <https://github.com/mrleedev/stardust>
[^9]: <https://musl.libc.org/>
[^10]: <https://github.com/ziglang/zig/issues/4875>
[^11]: <https://knowyourmeme.com/memes/how-to-draw-an-owl>
[^12]: <https://cdn-shop.adafruit.com/datasheets/SSD1306.pdf>
[^13]: <https://github.com/owenosborn/SSD1306-OLED-WiringPi/blob/master/ssd1306.h>
[^14]: Possibly exaggerated for effect. Possibly.
[^15]: <https://github.com/adafruit/Adafruit_CircuitPython_SSD1306/blob/master/adafruit_ssd1306.py>
[^16]: <https://kristoff.it/blog/what-is-zig-comptime/>

```

```
