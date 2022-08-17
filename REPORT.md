- `std`
The tool adds a terminal user interface for exploring packages
in practice for plutus-app and plutus-core this adds little value as we only 
have a few scripts in bash and thats it 

add a bunch of useful library functions to simplify flake code 
self.incl for src filtering 
self.deSystemize to remove system 


It can be used as a library to simplify folder and file management and provide 
a nice abstraction layer 


- `flakes` 
A more standardized approach at organizing nix code
A uniform interface for creating shells and packaging code
Very convenient for oneline installs of code from outside repositories 
Very convenient for nixConfig, defining caches
Fully pure build 

- `single marconi repo`
Cannot be done without duplicating lots of code


- Value added 
Main value added is for refactoring the nix code

DevX for nix-users will improve slightly 
DevX for non-nix users will stay the same 
