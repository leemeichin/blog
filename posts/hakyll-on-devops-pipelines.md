---
title: Hakyll on DevOps Pipelines
date: 2020-08-18
status: published
category: programming
---

In a way, this is total overkill for a static site. If I have the repo cloned on my machine and I want to publish a new post, I can do it in two commands:

```bash
stack exec site build
scp -r _site/ deploy@mrlee.dev:/var/www/www.mrlee.dev/
```

It's flawed compared to using `rsync`, as it won't remove existing files, but it does the job in less than a second or two.

The thing is, this isn't so quick if I want to publish a post from a different computer that doesn't have any programming tools installed. I would have to install `stack`[^1], which is a build tool for Haskell, and then I would have to run `stack build`. This can take at least half an hour as the command will pull down the correct version of `GHC` and a 'snapshot' (basically a huge collection of all the Hackage[^2] libraries available for that build) before it even _thinks_ about compiling my `site.hs` file. It also means to committing a few gigs of storage space for all of that.

I like to write from my little Surface Pro when I'm out and about, so I'd rather not do a full-blown compilation on that for the sake of my battery. Enter Azure DevOps Pipelines[^3].

I've been keen on playing with these pipelines for a while, and much like any dev-tool, it has a free tier for open source repos. So does Github Actions[^4], which actually shares some of the underlying architecture of DevOps Pipelines, but I wanted to play with something different.

Let's do a step-by-step walk through my setup.

----------

```yaml
trigger:
  - master
pool:
  vmImage: 'ubuntu-latest'
```

This is pretty much CI boilerplate. The build will run on any PR that targets `master`, and it uses Ubuntu as the underlying image. I'm not doing any Docker stuff here.

```yaml
jobs:
- job: build
  steps: ...
```

I only have a couple of jobs in this pipeline, to keep it simple. The next bunch of steps are nested under this.

```yaml
- script: |
      mkdir -p ~/.local/bin $(Build.BinariesDirectory)
      curl -L https://get.haskellstack.org/stable/linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
  displayName: Install Stack
```

Won't get far without grabbing the latest stable Stack binary.

```yaml
- task: Cache@2
  displayName: Cache Stack/GHC snapshot
  inputs:
    key: 'stack | root'
    path: .stack/
    cacheHitVar: 'STACK_SNAPSHOT_RESTORED'
```

Later on there will be a step that runs `stack build`, which will take about 40 minutes in CI. It would be a waste to repeatedly download all of that, so I'm caching the root stack folder for good measure. The `cacheHitVar` is something we will reference later.

```yaml
- task: Cache@2
  displayName: Cache local stack deps
  inputs:
    key: 'stack | stack.yaml.lock'
    path: .stack-work/
    cacheHitVar: 'STACK_DEPS_RESTORED'
```

This is the same as the last step, but it's for the dependencies my static site requires. I want to cache these separately so adding a new project dependency doesn't force a full refresh of the Stack snapshot.

```yaml
- script: |
      export PATH=$HOME/.local/bin:$PATH
      stack --no-terminal --stack-root $(System.DefaultWorkingDirectory)/.stack setup
  displayName: Build Snapshot
  condition: ne(variables.STACK_SNAPSHOT_RESTORED, 'true')
```

Notice the `STACK_SNAPSHOT_RESTORED` condition at the bottom there? This step sets up GHC and the Stack snapshot, but only if one wasn't restored from the cache. If the cache has it, then it will have alread been fetched.

```yaml
- script: |
      export PATH=$HOME/.local/bin:$PATH
      stack --no-terminal --stack-root  $(System.DefaultWorkingDirectory)/.stack build
  displayName: Build Dependencies
  condition: ne(variables.STACK_DEPS_RESTORED, 'true')
```

This is the same as above, but for the project dependencies. So far so good. We're almost done now.

```yaml
- script: |
      export PATH=$HOME/.local/bin:$PATH
      stack --no-terminal --stack-root $(System.DefaultWorkingDirectory)/.stack install --local-bin-path $(Build.BinariesDirectory)
  displayName: Build Site Executable
```

Since I've already run `stack build`, this just copies the binary to a different location, which I use to store it as a build artifact. `Build.BinariesDirectory` is a special place on the VM to store compiled build artifacts. It doesn't matter where specifically that is, only that it's the same across steps.

```yaml
- task: PublishBuildArtifacts@1
  displayName: Save static site binary
  inputs:
    pathToPublish: $(Build.BinariesDirectory)
    artifactName: site
```

This is where that binaries directory comes into play, as I can tell Azure to upload everything in there as a build artifact, which I can then refer to in another job. This isn't quite the same as a cache, as a build is not expected to fail if the cache goes missing. It would fail if the binary isn't there though.

So, that's the first step done, but what about actually publishing a post? I have two jobs for that, which are very similar (one for draft posts/staging, one for prod). I'll describe one of them.

```yaml
- job: deploy_published
  dependsOn: build
  condition: and(succeeded(), eq(variables['build.sourceBranchName'], 'master'))
  steps: ...
```

The key to this step is the condition. This will run only if the `build` job was successful, *and* the branch being built is the master branch. Practically, this only runs if I push straight to master or merge a PR. The staging version runs only on PRs.

```yaml
- task: DownloadBuildArtifacts@0
  displayName: Download site binary
  inputs:
    artifactName: site
    downloadPath: $(System.DefaultWorkingDirectory)
```

Time to put that binary I compiled to good use. It downloads it into the main working directory and I'll call it directly in a later step. The executable is self-contained (or otherwise dynamically links stuff the image already has), so I don't need to pull down Stack/GHC stuff again.

```yaml
- script: |
      export PATH=$(System.DefaultWorkingDirectory)/site:$PATH
      chmod +x $(System.DefaultWorkingDirectory)/site/site
      site build
  displayName: Build with published posts
```

This is the same as running `stack exec site build` on my local machine. It compiles the static site, so finally I'll have a new version to upload.

```yaml
- task: InstallSSHKey@0
  displayName: Setup SSH
  inputs:
    knownHostsEntry: '$(NexusKnownHost)'
    sshKeySecureFile: 'nexus_deploy'
```

I host this blog on my own little VPS, which means that the server needs to know that the CI is authorised to connect to it with its SSH key. This is the same as having a deploy key on GitHub, and requires generating a keypair to be stored in CI, with the public key being added to your `authorized_keys` file of the appropriate user on the server.

_(At this point I'll say that if you're doing this yourself, make sure to properly harden your server. I'll describe this more in a follow-up post.)_

There's only step left now, and that's to deploy!

```yaml
- task: CopyFilesOverSSH@0
  displayName: Deploy to prod
  inputs:
    sshEndpoint: 'Nexus'
    sourceFolder: '_site/'
    contents: '**'
    targetFolder: '/var/www/www.mrlee.dev'
    cleanTargetFolder: true
    readyTimeout: '20000'
```

This is similar to running `rsync` to deploy, except that it knows where to get your private key from and where to connect to. This is defined elsewhere in Azure DevOps, through the UI, rather than in the YAML file.

To solve the issue I first mentioned, `cleanTargetFolder` makes sure to delete the previous deployment before copying the new one over. Problem solved!

To see the pipeline in full, you can check out the full YAML file[^5] and also the public builds[^6]. I've been using it with success for the past couple of weeks now.

[^1]: <https://docs.haskellstack.org/en/stable/README/> 
[^2]: <https://hackage.haskell.org/>
[^3]: <https://dev.azure.com/>
[^4]: <https://github.com/features/actions>
[^5]: <https://github.com/mrleedev/www.mrlee.dev/blob/master/azure/pipeline.yml>
[^6]: <https://dev.azure.com/mrleedev/www.mrlee.dev/_build/results?buildId=115>