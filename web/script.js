export function tryOpenDialog() {
    document.getElementById("shouldOpenDialog").innerText = "true";
}

export function getClipboard() {
    return document.getElementById("clipboard").innerText;
}

export function onInput() {
    if (!navigator.userAgent.toLowerCase().includes("firefox") && !navigator.userAgent.toLowerCase().includes("safari")) {
        window.navigator.clipboard.readText().then((str) => document.getElementById("clipboard").innerText = str).catch(x => x)
    }
}

export function save(name, bytes) {
    const download = document.getElementById("download");
    download.href = URL.createObjectURL(new Blob([bytes], { endings: "transparent" }));
    download.download = name;
    download.click();
}

export function onPanic(error) {
    let stack = new Error().stack;
    error = error + "\n\nStack:\n\n" + stack + "\n";
    document.getElementById("crashReportRaw").crashData = error;
    let panic = document.getElementById("panic");
    panic.style.opacity = "0.8";
    panic.style.display = "grid";
    panic.style.zIndex = "1";
    console.error(error)
}
