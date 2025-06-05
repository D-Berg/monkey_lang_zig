const program = "3 + 3";
let memory = null;
let instance = null;

const console_element = document.getElementById("console");

// Intercept input and store it for WASM to read
const stdin = document.getElementById("stdin");
const run_button = document.getElementById("run");

const importObject = {
    env: {
        js_write(fd, addr, len) {
            try {
                // read the message from the wasm memory's underyling buffer
                let buff = memory.buffer.slice(addr, addr + len);
                // use TextDecoder to convert the raw buffer into a string
                let str = new TextDecoder().decode(buff);
                switch (fd) {
                    case 1: // STDOUT
                        console_element.innerHTML += `<span class="stdout">${str}</span>`;
                        return len;
                    case 2: //STDERR
                        console_element.innerHTML += `<span class="stderr">${str}</span>`;
                        return len;
                }
                return 0; // in the case that 0 bytes were written
            } catch (err) {
                return -1; // return -1 if an error occurs
            }
        },
    }
};

WebAssembly.instantiateStreaming(fetch("bin/monkey_web.wasm"), importObject).then(
    (obj) => {

        const instance = obj.instance;
        memory = instance.exports.memory;

        const eval = instance.exports.wasm_evaluate;
        const alloc = instance.exports.alloc;
        const free = instance.exports.free;

        run_button.addEventListener('click', () => {
            console_element.innerHTML = ""

            const input = stdin.value;

            if (input.length == 0) {
                console_element.innerHTML += "<span>Please provide a program\n</span>"
                return;
            };

            const encoder = new TextEncoder();
            const encoded = encoder.encode(input);

            const len = encoded.length
            const ptr = alloc(len);
            const buf = new Uint8Array(memory.buffer, ptr, len);
            buf.set(encoded);

            run_button.textContent = "Run 🔴";
            const result = eval(ptr, len);
            run_button.textContent = "Run 🟢";

            free(ptr, len);

            console.log("wasm exited with code:", result);

        });


    },
);

stdin.addEventListener('keydown', function(e) {
  if (e.key === 'Tab') {
    e.preventDefault();

    // Get cursor position
    const start = this.selectionStart;
    const end = this.selectionEnd;

    const tab = '    ';

    // Set new value with tab inserted
    this.value = this.value.substring(0, start) + tab + this.value.substring(end);

    // Move cursor after inserted tab
    this.selectionStart = this.selectionEnd = start + tab.length;
  }
});
