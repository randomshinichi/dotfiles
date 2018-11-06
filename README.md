1. add ssh public key to /root/.ssh/authorized_keys
2. `apt-get install ssh python-apt`

Other significant dots in home:
```
.config/Slack
.env_secrets
.goland
.pycharm
.minecraft
.mozila
.quodlibet
.ssh
~/snap/
```

```
tar cvjpf source.tbz2 --exclude source/go source/ && tar cvjpf dotfiles.tbz2 .env_secrets .electrum/ .GoLand2018.2/ .PyCharmCE2018.2/ .minecraft/ .mozilla/ .quodlibet/ .ssh/ snap/
 ```