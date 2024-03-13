export function tryOpenDialog() {
    shouldOpenDialog = true;
}

export function getClipboard() {
    return clipboard;
}

export function onInput() {
    if (!navigator.userAgent.toLowerCase().includes("firefox") && !navigator.userAgent.toLowerCase().includes("safari")) {
        window.navigator.clipboard.readText().then((str) => clipboard = str).catch(x => x)
    }
}

export function save(name, bytes) {
    const download = document.getElementById("download");
    download.href = URL.createObjectURL(new Blob([bytes], { endings: "transparent" }));
    download.download = name;
    download.click();
}
