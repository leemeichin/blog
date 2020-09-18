---
title: Hardening your server
date: 2020-30-08
status: draft
category: devops
---

There was a time before Docker, Kubernetes (K8S for short), and cloud providers where the standard way of deploying your application to production typically involved buying a VPS or two from Linode[^1], Rackspace[^2], or similar. In some cases you might even buy some dedicated hardware. Of course, for many business that don't use AWS or Azure, that's still the case.

Using something like Docker or K8S for your own small server is taking overkill to a whole new level, and there are still so many use-cases where you are much better off with your £3/month VPS from Digital Ocean[^3] or some other local provider. Mine is run by Hostworld UK[^4]. Of course, the danger here is that your vanilla OS install isn't going to be well protected against the internet, which will leave it vulnerable to attack. Most likely it would become part of a botnet, used to mine crypto, or otherwise your blogging software might be injected with Javascript-based malware to be delivered to your unwitting readers. Not good.

Luckily enough, there are a few simple steps you can take to bring your new server up to the bare minimum standard of security, although of course this will not be the end of the story for you. Keep in mind that any new software you add to a machine, or any of your own code that you deploy, can open up their own holes into the system. It's not limited to remote code execution (e.g. being able to run shell scripts or execute malicious files because of a vulnerability), but SQL injection, cross-site scripting (XSS), denial of service (DOS) and so on.

Let's get started, anyway. I'm writing this with Ubuntu in mind as, with all due respect, it tends to be the lowest common denominator of Linux distros these days. Aside from certain installation instructions, such as using `apt` to install software, the meat of this post should be distro agnostic.

You should do these steps roughly in order, as in some cases you will end up locked out of the machine and will need to provision a fresh image with your host.

---

## Create a non-root user

Keeping the root user account open to the world, with only a password to protect it, is asking for trouble. By default your server isn't going to limit the number of attempts to log in, so it's a prime target for a brute-force attack. Once an attacker has root, the game is over.

So, let's first make an account for ourselves and then give it the ability to run `sudo`, which will allow for temporary root privileges:

```bash
createuser my_name
usermod -a -G sudo my_name
```

Make sure to choose a secure password! This will be used to invoke `sudo`, but will **not** be used for logging in.

## Generate an SSH key on your machine

Many people will advocate using more than one SSH key, for the different services you use (Github, your personal server, whatever else...). I think it's a good idea, and we can tweak our SSH client so we don't have to remember which key is being used when connecting to the server.

This key is going to be used to log in _interactively_ to another server, which is another way of saying you'll have a shell to do whatever you want in, and it's not like running `git push` and forgetting about it. To be safe, you should give this key a password, and make it different to the one you used for your account on the server.

```bash
ssh-keygen -f ~/.ssh/my_server
```

You should now have two new files in your `.ssh` directory: `my_server` and `my_server.pub`. The latter one ending in `.pub` is the one we can share publically, and is the one this new server needs to know about. Go ahead and upload it:

```bash
ssh-copy-id -i ~/.ssh/my_server my_name@my_server
```

It'll ask for your password before uploading. After that, you can try logging into the server again (`ssh -i ~/.ssh/my_server my_name@my_server`) and if you get in using only the password used for your key (if you did use one), then you're good!

## Disable root access and password login

The only time you should require a password when dealing with your server is when you need `sudo`. And you shouldn't depend on direct root access at all in the long run.

In order to achieve that, we can enforce the usage of public key authentication by disabling password login, and we can also disable the ability to log in as the root user.

This requires editing a file. Let's do it in `nano` for the sake of simplicity.

```bash
nano /etc/ssh/sshd.conf
```

