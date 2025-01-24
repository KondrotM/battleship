# Learning ADA

- `gnat` : Freely-available Ada-95 compiler (in deb repos)
- `gprbuild` : Compiler and builder for Ada projects (in deb repos)
- `alire` : Ada package manager (clone via git)
- `alr-completion.bash` : Source this to get alr completion
  - `alr` must be in /bin (at least symlinked)
  - It seems to take a long time to source, may not be worth inside of .bashrc / .zshrc (change -P -> -p for zsh)
  - `alias salr="source /home/mk/Documens/alire/scripts/alr-completion.bash" >> ~/.zshrc`


## Hello world
- Using [ada-lang.io](https://ada-lang.io/docs/learn/tutorial/hello-world/)
- `alr init --bin my_hello_world` : Create basic project structure
- `alire.toml` : Describes what's in the project
- `my_hello_world.gpr` : Low-level GNAT project file
- `my_hello_world.adb` : Ada source code with logic

- `alr build` : Creates binary
- `alr run` : Runs binary

```adb
-- Context clause (imports)
with Ada.Text_IO;

-- Start of program
procedure My_Hello_World is
-- Define variables here
begin -- Starts execution
   Ada.Text_IO.Put_Line ("Hello, World!");
end My_Hello_World; -- End of program
```

## Passing args
- Use `Ada.Command_Line`
- After building, you can `./bin/program arg1 arg2 ..`
- You can also `alr run --args="arg1 arg2 .."`

## For loop
```adb
for Next in First_Value .. Last_Value loop
   -- Do these steps.
end loop;
```


## Five structural elements
To review [here](https://ada-lang.io/docs/learn/overview/five-structural-elements) when creating more complex code




