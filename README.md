Die beiden Anwendungsfälle können von der Kommandozeile aus gestartet werden.
Voraussetzung ist, dass sbt und eine JRE installiert sind.

### Anwendungsfall GenerateKBs:
Nur der Parameter signatureElements ist obligatorisch, alle anderen sind optional.

```bash
sbt 'run-main run.GeneratePipeline \
        --signatureElements a b \
        --formulas --conditionals \
        --min 1 --max 1 \
        --outputPath /path/to/folder \
        --compressed' 
```
        
        
### Anwendungsfall ConvertKBs:
Die beiden Pfade zu den Dateien mit den komprimierten Wissensbasen sind obligatorisch, der Ausgabepfad ist optional.

```bash
sbt 'run-main run.ConvertPipeline \
        --inputPath /path/to/allConditionals.txt \
        --binaryPath /path/to/allKBs.bin \
        --outputPath /path/to/folder ' 
```