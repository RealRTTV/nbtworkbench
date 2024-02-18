var clipboard = '';
var shouldOpenDialog = false;

const dialog = document.getElementById("file_dialog");

function openDialog(event) {
    if (!shouldOpenDialog) return;
    shouldOpenDialog = false;
    dialog.click();
}

setTimeout(() => {
    const canvas = document.getElementsByTagName("canvas")[0];

    canvas.addEventListener("pointerdown", openDialog);
    canvas.addEventListener("pointerup", openDialog);
    canvas.addEventListener("keydown", openDialog);
    canvas.addEventListener("keyup", openDialog);
    canvas.addEventListener("keypress", openDialog);

}, 1000);

export function tryOpenDialog() {
    shouldOpenDialog = true;
}

export function getClipboard() {
    return clipboard;
}

export function onInput() {
    if (!navigator.userAgent.toLowerCase().includes("firefox")) {
        window.navigator.clipboard.readText().then((str) => clipboard = str).catch(x => x)
    }
}

export function save(name, bytes) {
    const download = document.getElementById("download");
    download.href = URL.createObjectURL(new Blob([bytes], { endings: "transparent" }));
    download.download = name;
    download.click();
}
