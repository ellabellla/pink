export function println(msg) {
    let outputConsole = document.getElementById("output-console");
    outputConsole.innerHTML += `${msg}\n`;
}