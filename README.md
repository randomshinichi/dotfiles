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
tar cvjpf home.tbz2 .env_secrets .electrum/ .PyCharmCE2018.2/ .minecraft/ .mozilla/ .quodlibet/ .ssh/ Downloads/ Dropbox/ snap/ source/ --exclude source/go
 ```
