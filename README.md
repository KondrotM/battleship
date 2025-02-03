# Battleship Game in Ada

Thank you for checking out this project! I am working to create a CLI battleship-style game in the programming language Ada in order to gain a working knowledge of it. 

Below are steps to help run this project locally:

```bash
# Install Ada tooling, this will differ if not running debian-based OS
apt install gnat gprbuild
# Download and install Alire (ada package manager)
wget https://github.com/alire-project/alire/releases/download/v2.0.2/alr-2.0.2-bin-x86_64-linux.zip 
unzip ./alr-2.0.2-bin-x86_64-linux.zip 
ln -s /usr/bin/alr ./alr-2.0.2-bin-x86_64-linux/bin/alr 
# ^ Store wherever is appropriate, e.g. /opt, or move the binary to /usr/bin directly 

# Clone this repo and install deps
git clone git@github.com:KondrotM/battleship.git
cd ./battleship/battleship
alr install

# Run the project!
alr run
```

Goals:
- [x] Prototype complete
- [x] Boat placement and user input
- [ ] Intelligent AI guessing
- [ ] Ada unit testing 
- [ ] Compile/Host on web site