rem config
convert_tileset ink.bin ink_n.bin
java -jar ..\kickass\kickass.jar ink.asm
@exomizer.exe sfx basic,2049 ink.prg -o inkExo.prg