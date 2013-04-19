# adeimantus

> Manage ejabberd clusters with Capistrano

See a list of available tasks:

```shell
$ cap -vT
```

Learn more about a specific task:

```shell
$ cap -e [taskname] (e.g cap -e cluster:launch_instance)
```

## Useful tips

All remote tasks run in all instances in the cluster by default. To specify which hosts to target, pass the `HOSTS` environment variable to the `cap` command:

```shell
$ cap cluster:reload HOSTS=[hostname]
```

On deployment, capistrano will update each instance with the _HEAD_ of the master branch in the origin repository. If for some reason you need to deploy the code in your working copy, pass the `DEPLOY_FROM_LOCAL` variable to the `cap` command:

```shell
$ cap cluster:deploy DEPLOY_FROM_LOCAL=1
```

## Keys and deploying

To be able to run tasks in the cluster, an admin must add their key to each instance's `authorized_keys` file. To achieve that, add the key to `instance/keys/adeimantus.pub` and run:

```shell
$ cap cluster:update_keys
```

The local file is referenced, so there's no need to deploy new code before running this task.

To deploy new code, each instance must be able to authenticate with github. This happens via SSH agent forwarding, and means that the admin must have access to the adeimantus github repository via their public key. SSH agent forwarding must also be enabled in the local machine. One way to achieve this is by adding:

    Hosts *.amazonaws.com
        ForwardAgent yes
        IdentityFile [path_to_private_key]

to the local `~/.ssh/config` file.

## License

MIT
