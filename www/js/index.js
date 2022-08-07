import init, {compile, Compiled, run} from "../../pkg/pink.js";

let compileButton = document.getElementById("compile-button");
compileButton.disabled = true;
let runButton = document.getElementById("run-button");
runButton.disabled = true;

let code = document.getElementById("code");
let asm = document.getElementById("asm");
let prettyAsm = document.getElementById("pretty-asm");
let asmStart = document.getElementById("asm-start");
let asmGlobals = document.getElementById("asm-globals");
let outputCanvas = document.getElementById("output-canvas");
var ctx = outputCanvas.getContext("2d");
let outputConsole = document.getElementById("output-console");

const width = outputCanvas.width;
const height = outputCanvas.height;

init()
    .then(() => {
        compileButton.addEventListener("click", (event) => {
            outputConsole.value = "";
            prettyAsm.value = "";
            asm.value = "";
            asmStart.innerHTML = 0;
            asmGlobals.innerHTML = 0;
            ctx.clearRect(0, 0, width, height);

            try {
                let compiled = compile(code.value,width, height);
                prettyAsm.value = compiled.pretty_asm;
                asm.value = compiled.asm;
                asmStart.innerHTML = compiled.start;
                asmGlobals.innerHTML = compiled.globals;
            } catch (err) {
                outputConsole.value = `ERROR:\n${err}`;
                asmStart.value = 0;
                asmGlobals.value = 0;
            }
        });

        runButton.addEventListener("click", (event) => {
            if (asm.value == "") {
                return;
            }

            outputConsole.value = "";
            const program =new Compiled(asm.value, prettyAsm.value, Number.parseInt(asmStart.innerHTML), Number.parseInt(asmGlobals.innerHTML));
            runButton.disabled = true;
            run_program(program, width, height).finally(() => {
                runButton.disabled = false;
            });
        });

        compileButton.disabled = false;     
        runButton.disabled = false;   
    });

    async function run_program(program, width, height) {
        try {
            const matrix = run(
                    program,
                    width,
                    height
                );
            const data = new Uint8ClampedArray(matrix.memory); 
            const image = new ImageData(data, width, height);

            ctx.putImageData(image, 0, 0);
            ctx.imageSmootingEnabled = false;
            ctx.globalCompositeOperation = "copy";
            ctx.drawImage(outputCanvas, 0, 0, width, height);
        } catch(err) {
            ctx.clearRect(0, 0, width, height);
            outputConsole.value = `ERROR:\n${err}`;
        }
    }