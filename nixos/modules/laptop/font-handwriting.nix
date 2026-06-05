{ pkgs }:

pkgs.stdenv.mkDerivation {
  pname = "handwriting";
  version = "1.0.0";
  src = ../../src/handwriting.zip;
  unpackPhase = ''
    runHook preUnpack
    ${pkgs.unzip}/bin/unzip $src
    runHook postUnpack
  '';

  installPhase = ''
    runHook preInstall
    install -Dm644 *.ttf -t $out/share/fonts/truetype
  '';
}
