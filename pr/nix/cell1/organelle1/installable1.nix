inputs: 
inputs.nixpkgs.stdenv.mkDerivation {
    name = "test";
    src = ./source.txt; 
    phases = ["installPhase"];
    installPhase = ''
        mkdir -p $out 
        ls -la $src
        cp $src $out
    '';
}