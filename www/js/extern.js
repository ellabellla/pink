export function println(msg) {
    let outputConsole = document.getElementById("output-console");
    outputConsole.value += `${msg}\n`;
}