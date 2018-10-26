1. add ssh public key to /root/.ssh/authorized_keys
2. `apt-get install ssh && sudo systemctl enable ssh`
2. `apt-get install python-apt`
To have the local machine do `ansible-pull -U....` is tempting but needs the target to have git, python-apt installed first. Hell, if I had to install all that by myself, then why run it at all?

That said, it isn't that hard to point this playbook to localhost.
