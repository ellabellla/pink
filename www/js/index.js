import init, {compile, Compiled, run} from "../../pkg/pink.js";
init()
    .then(() => {
        let compileButton = document.getElementById("compile-button");
        let runButton = document.getElementById("run-button");

        let code = document.getElementById("code");
        let asm = document.getElementById("asm");
        let prettyAsm = document.getElementById("pretty-asm");
        let asmStart = document.getElementById("asm-start");
        let asmGlobals = document.getElementById("asm-globals");
        let outputCanvas = document.getElementById("output-canvas");

        const width = outputCanvas.width;
        const height = outputCanvas.height;

        compileButton.addEventListener("click", (event) => {
            try {
                let compiled = compile(code.value,width, height);
                prettyAsm.value = compiled.pretty_asm;
                asm.value = compiled.asm;
                asmStart.innerHTML = compiled.start;
                asmGlobals.innerHTML = compiled.globals;
            } catch (err) {
                prettyAsm.value = `ERROR:\n${err}`;
                asmStart.value = 0;
                asmGlobals.value = 0;
            }
        });

        runButton.addEventListener("click", (event) => {
            try {
                let outputConsole = document.getElementById("output-console");
                outputConsole.innerHTML = "";

                const width = outputCanvas.width;
                const height = outputCanvas.height;
                const matrix = run(
                        new Compiled(asm.value, prettyAsm.value, Number.parseInt(asmStart.innerHTML), Number.parseInt(asmGlobals.innerHTML)),
                        width,
                        height
                    );
                const data = new Uint8ClampedArray(matrix.memory); 
                const image = new ImageData(data, width, height);

                var ctx = outputCanvas.getContext("2d");

                ctx.putImageData(image, 0, 0);
                ctx.imageSmootingEnabled = false;
                ctx.globalCompositeOperation = "copy";
                ctx.drawImage(outputCanvas, 0, 0, width, height);
            } catch(err) {
                outputConsole.value = `ERROR:\n${err}`;
            }
        })
        
    });