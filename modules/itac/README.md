# ITAC2 for GPP

This is a port of the `itac` app from OCS. Work is ongoing.

## Quick Start

This software works a lot like the old `itac` command, but it contacts the GPP database instead of working with local files. Edits are done using Explore rather than via yaml files.

### 1. Install Coursier

If you have not yet installed `cs`, you can do so by cfollwoing the instructions [here](https://get-coursier.io/docs/cli-overview.html#install-native-launcher).

Depending on how you installed it, you may need to move `cs` into a directory that's on your executable path, such as `/usr/local/bin/`.

On success you will be able to do `cs --help` and see some some text.

### 2. Install ITAC

Use `cs setup` to set up Java, configure your path, and install ITAC2. This will not interfere with your existing `itac` install, if you have one.

```
cs setup --yes --channel edu.gemini:itac2-channel --apps itac2
```

On success you will be able to do `itac2` and see some help text.

### 3. Set up a Workspace

ITAC2 workspaces are **not compatible** with the old ones; some of the configuration is slightly different. However it is possible to run the new software against old proposals (see below).

Create an empty directory, go into it, and create a workspace for a given semester. There is some test data in the dev database for 2000B so start there.

```
itac2 init 2000B
```

### 4. Configure your API Key

Edit `common.yaml` and paste your GPP API key into the indicated place on the `authorization` line. Note that this key is tied to *your* account and should be kept private.

### 5. Create a Queue

You should now be able to create a dual-site queue via

`itac2 queue`

### 6. Run Other Commands

Do `itac2 --help` to see a list of implemented commands.

## OCS Compatibility Mode

If you copy your `band-N/` folders from an OCS ITAC workspace into your ITAC2 workspace you can run `itac --ocs <command>` to run a command against these proposals rather than the ones in the ODB. Note that **edit files are not supported** here; what you will see is the original queue with a lot of rejections. The goal here is simply to demonstrate that the new code does the same thing as the old code, modulo differences coming from merged proposals.

