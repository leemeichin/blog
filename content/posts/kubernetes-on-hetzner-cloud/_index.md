---
title: Kubernetes on Hetzner Cloud In No Time At All
date: 2023-06-05
category: programming
type: 'bookcase'
---

Last week I made the rookie mistake of not backing up my SSH keys for my personal web server. I had keys on my work laptop, which had been wiped because I left the company; and I had them on my PC, which I also wiped because I turned it on for the first time in 8 months and a fresh Windows 11 update fucked it all up.

I went into the admin console on my webhost and tried to open a shell from there, but I forgot the root password. They had an option to reset the root password but that failed. Locked out from the server in every way, I decided it was time to try something new.

My previous setup was a little Ansible playbook which did the job, but not without fault. I wasn't satisfied with it so I decided it was a good time to start fresh. And on that note, history lesson over.

What follows is a comprehensive guide to build a low-cost Kubernetes cluster (that automatically scales) with the following features:

- Prometheus monitoring (with Grafana)
- A private docker registry
- Nginx load balancer with LetsEncrypt
- Unattended cluster upgrades
- CI with Github Actions

Do you want or need all of this? I don't know. Once you've provisioned the cluster it's fair game.
